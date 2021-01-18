# -*- coding: utf-8 -*-
#
# GT4Py - GridTools4Py - GridTools for Python
#
# Copyright (c) 2014-2019, ETH Zurich
# All rights reserved.
#
# This file is part the GT4Py project and the GridTools framework.
# GT4Py is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or any later
# version. See the LICENSE.txt file at the top-level directory of this
# distribution for a copy of the license or check <https://www.gnu.org/licenses/>.
#
# SPDX-License-Identifier: GPL-3.0-or-later

import functools
import subprocess as sub
from typing import Any, Dict, List, Set, Tuple
from copy import deepcopy
from collections.abc import Iterable

from gt4py import backend as gt_backend
from gt4py import definitions as gt_definitions
from gt4py import ir as gt_ir
from gt4py import utils as gt_utils
from gt4py.utils import text as gt_text
from gt4py.utils.attrib import attribkwclass as attribclass
from gt4py.utils.attrib import attribute

OPTIMIZATION_METHODS = {
    "IJKLoop",
    "Prefetching",
    "ReadonlyCaching",
    "LoopUnrolling",
    "KCaching",
    "BlocksizeAdjusting",
}


def _get_optimization_methods():
    import os

    opt_methods = {
        method: True if method in os.environ else False for method in OPTIMIZATION_METHODS
    }
    if "BlocksizeAdjusting" in os.environ:
        opt_methods["BlocksizeAdjusting"] = os.environ["BlocksizeAdjusting"]
    else:
        opt_methods["BlocksizeAdjusting"] = ""
    return opt_methods


@attribclass
class CUDAIntrinsic(gt_ir.Statement):
    # CUDA statement to be directly inserted into kernels
    body = attribute(of=str)


@attribclass
class LDGFieldRef(gt_ir.Ref):
    # Read-only (in the stage) fieldref
    fieldref = attribute(of=gt_ir.FieldRef)


@attribclass
class L1Prefetch(gt_ir.Statement):
    # Generate L1 Prefetching intrinsic
    fieldref = attribute(of=gt_ir.FieldRef)


class MarkKCachePass(gt_ir.IRNodeVisitor):
    def __init__(self, exec_order: str):
        self.fields_in_applyblock_: Set[Tuple[str, int]] = set()  # Set[(name, offset_in_k)]
        self.k_cache_vars_: List[str] = list()
        self.exec_order_: str = exec_order

    def __call__(self, node: gt_ir.ApplyBlock, **kwargs):
        return self.visit_ApplyBlock(node, **kwargs)

    def visit_FieldRef(self, node: gt_ir.FieldRef, *, write: bool = False):
        if not write:
            self.fields_in_applyblock_.add(
                (node.name, node.offset["K"])
            )  # ASSUME K is the third index!

    def visit_Assign(self, node: gt_ir.Assign, **kwargs):
        self.visit(node.target, write=True, **kwargs)
        self.visit(node.value, **kwargs)

    def visit_ApplyBlock(self, node: gt_ir.ApplyBlock, **kwargs):
        self.fields_in_applyblock_ = set()
        self.k_cache_vars_ = list()
        self.visit(node.body, **kwargs)  # -> BlockStmt: .stmts: List[Decl|Assign|If|BlockStmt]
        # Mark all possible fields for k caching
        offset_dict = {field[0]: [] for field in self.fields_in_applyblock_}
        for field in self.fields_in_applyblock_:
            offset_dict[field[0]].append(field[1])
        for field_name in offset_dict.keys():
            max_offset = max(offset_dict[field_name])
            min_offset = min(offset_dict[field_name])
            if max_offset - min_offset == 1:  # Only do k caching of size 2
                self.k_cache_vars_.append(field_name)
        return self.k_cache_vars_, offset_dict


class InsertKCachePass(gt_ir.IRNodeMapper):
    def __init__(
        self,
        field_decls: Dict[str, gt_ir.FieldDecl],
        k_cache_vars: List[str],
        offset_dict: Dict[str, List[int]],
        exec_order: str,
    ):
        self.fields_: Dict[str, gt_ir.FieldDecl] = field_decls
        self.k_cache_vars_: Dict[str, str] = {
            field_name: field_name + "_cache" for field_name in k_cache_vars
        }
        self.minmax_offset_dict_: Dict[str, Tuple[int, int]] = dict()
        assert exec_order == "forward" or exec_order == "backward"
        if exec_order == "forward":
            self.minmax_offset_dict_ = {
                key: (min(val), max(val)) for key, val in offset_dict.items()
            }
        elif exec_order == "backward":
            self.minmax_offset_dict_ = {
                key: (max(val), min(val)) for key, val in offset_dict.items()
            }
        else:
            self.minmax_offset_dict_ = dict()
        self.writeback_stmt_dict_: Dict[str, gt_ir.Assign] = dict()
        self.read_stmt_dict_: Dict[str, gt_ir.Assign] = dict()

    def __call__(self, node: gt_ir.ApplyBlock):
        assert isinstance(node, gt_ir.ApplyBlock)
        return self.visit(node)

    def _get_var_ref(self, node: gt_ir.FieldRef):
        k_offset = node.offset["K"]
        if k_offset == self.minmax_offset_dict_[node.name][0]:
            index = 0
        else:
            index = 1
        return gt_ir.VarRef(name=self.k_cache_vars_[node.name], index=index, isarray=True)

    def visit_FieldRef(self, path: tuple, node_name: str, node: gt_ir.FieldRef):
        if node.name in self.k_cache_vars_:
            new_node = self._get_var_ref(node)
            self.read_stmt_dict_[node.name + "_" + str(new_node.index)] = gt_ir.Assign(
                target=new_node, value=node
            )
            return True, new_node
        else:
            return True, node

    def visit_Assign(self, path: tuple, node_name: str, node: gt_ir.Assign):
        field_name = node.target.name
        if field_name in self.k_cache_vars_:
            original_target = node.target
            new_target = self._get_var_ref(original_target)
            self.writeback_stmt_dict_[field_name + "_" + str(new_target.index)] = gt_ir.Assign(
                target=original_target, value=new_target
            )
            node.target = new_target
        node.value = self.visit(node.value)
        return True, node

    def visit_ApplyBlock(self, path: tuple, node_name: str, node: gt_ir.ApplyBlock):
        self.read_stmt_dict_ = dict()
        new_node = deepcopy(node)
        for field_name, cache_name in self.k_cache_vars_.items():
            new_node.local_symbols[cache_name] = gt_ir.VarDecl(
                name=cache_name,
                data_type=self.fields_[field_name].data_type,
                length=2,  # Only do k caching of size 2
                is_api=False,
            )
        self.generic_visit(path, node_name, new_node)
        new_node.body.stmts.extend([assign for _, assign in self.writeback_stmt_dict_.items()])
        for _, cache_name in self.k_cache_vars_.items():
            new_node.body.stmts.append(
                gt_ir.Assign(
                    target=gt_ir.VarRef(name=cache_name, index=0, isarray=True),
                    value=gt_ir.VarRef(name=cache_name, index=1, isarray=True),
                )
            )
        new_node.body.stmts = [
            assign for name, assign in self.read_stmt_dict_.items() if name[-1] == "1"
        ] + new_node.body.stmts
        new_node.init_stmt.extend(
            [assign for name, assign in self.read_stmt_dict_.items() if name[-1] == "0"]
        )
        return True, new_node


class MarkIJKLoopPass(gt_ir.IRNodeVisitor):
    def __init__(self):
        self.offsets: List[bool] = [False, False, False]

    def __call__(self, node: gt_ir.Node):
        self.offsets = [False, False, False]
        self.visit(node)
        return self.offsets

    def visit_FieldRef(self, node: gt_ir.FieldRef):
        offset = [node.offset.get(name, 0) for name in node.offset]
        assert len(offset) == len(self.offsets)
        for i in range(len(offset)):
            if offset[i] != 0:
                self.offsets[i] = True


class ReorderLoopPass(gt_ir.IRNodeVisitor):
    def __call__(self, node: gt_ir.ApplyBlock):
        self.visit_ApplyBlock(node)

    def visit_ApplyBlock(self, node: gt_ir.ApplyBlock):
        markijklooppass = MarkIJKLoopPass()
        offsets = markijklooppass(node)
        node.metadata["offsets"] = offsets
        node.metadata["IJKLoop"] = False
        if not offsets[0] and not offsets[1]:
            node.metadata["IJKLoop"] = True


class UnrollLoopPass(gt_ir.IRNodeVisitor):
    def __init__(self):
        self.line_count_: int = 0

    def __call__(self, node: gt_ir.Node):
        self.visit(node)

    def visit_BlockStmt(self, node: gt_ir.BlockStmt):
        self.line_count_ += len(node.stmts)
        for stmt in node.stmts:
            self.visit(stmt)

    def visit_ApplyBlock(self, node: gt_ir.ApplyBlock):
        self.line_count_ = 0
        unroll_num = 1
        if self.line_count_ <= 1:
            unroll_num = 8
        elif self.line_count_ <= 2:
            unroll_num = 4
        elif self.line_count_ <= 4:
            unroll_num = 2
        else:
            unroll_num = 1
        node.metadata["unroll_num"] = unroll_num


class PrefetchingLDGPass(gt_ir.IRNodeMapper):
    def __init__(self, fields: Dict[str, gt_ir.FieldDecl], opt_methods: Dict[str, bool]):
        self.prefetching_block_: Dict[str, gt_ir.FieldRef] = dict()
        self.read_only_fields_: Set[str] = set()
        self.fields_: Dict[str, gt_ir.FieldDecl] = fields
        self.opt_methods_: Dict[str, bool] = opt_methods

    def __call__(self, node: gt_ir.Stage):
        return self.visit(node)

    def visit_FieldRef(self, path: tuple, node_name: str, node: gt_ir.FieldRef):
        assert isinstance(node, gt_ir.FieldRef)
        if self.opt_methods_["ReadonlyCaching"] and node.name in self.read_only_fields_:
            return True, LDGFieldRef(fieldref=node)
        elif self.opt_methods_["Prefetching"] and self.fields_[node.name].is_api:
            offset = [node.offset.get(name, 0) for name in node.offset]
            if node.name not in self.prefetching_block_ or offset == [0, 0, 0]:
                self.prefetching_block_[node.name] = L1Prefetch(fieldref=node)
        return True, node

    def visit_ApplyBlock(self, path: tuple, node_name: str, node: gt_ir.ApplyBlock):
        self.prefetching_block_ = dict()
        new_node = deepcopy(node)
        self.visit(new_node.body)
        new_node.body.stmts = [
            node for _, node in self.prefetching_block_.items()
        ] + new_node.body.stmts
        return True, new_node

    def visit_Stage(self, path: tuple, node_name: str, node: gt_ir.Stage):
        self.read_only_fields_ = set()
        for accessor in node.accessors:
            if isinstance(accessor, gt_ir.FieldAccessor):
                if accessor.intent == gt_ir.AccessIntent.READ_ONLY:
                    self.read_only_fields_.add(accessor.symbol)
        for i in range(len(node.apply_blocks)):
            applyblock = node.apply_blocks[i]
            node.apply_blocks[i] = self.visit(applyblock)
        return True, node


class OptimizationPass(gt_ir.IRNodeVisitor):
    def __init__(self, opt_methods: Dict[str, bool]):
        self.fields_: Dict[str, gt_ir.FieldDecl] = dict()
        self.opt_methods_: Dict[str, bool] = {
            method: opt_methods[method] if method in opt_methods else False
            for method in OPTIMIZATION_METHODS
        }
        if not self.opt_methods_["IJKLoop"]:
            self.opt_methods_["LoopUnrolling"] = False
            self.opt_methods_["KCaching"] = False

    def apply(self, impl_node: gt_ir.StencilImplementation):
        impl_node = self.visit(impl_node)
        return impl_node

    def visit_Stage(self, node: gt_ir.Stage, *, exec_order: str = "unknown", **kwargs):
        for i in range(len(node.apply_blocks)):
            applyblock = node.apply_blocks[i]
            applyblock.metadata["IJKLoop"] = False
            applyblock.metadata["unroll_num"] = 1
            if self.opt_methods_["IJKLoop"]:
                reorderlooppass = ReorderLoopPass()
                reorderlooppass(applyblock)
            if (
                self.opt_methods_["KCaching"]
                and applyblock.metadata["IJKLoop"]
                and exec_order in ["forward", "backward"]
            ):
                markkcachepass = MarkKCachePass(exec_order=exec_order)
                k_cache_vars_, offset_dict = markkcachepass(applyblock, **kwargs)
                insertkcachepass = InsertKCachePass(
                    self.fields_, k_cache_vars_, offset_dict, exec_order
                )
                new_block = insertkcachepass(applyblock)
                node.apply_blocks[i] = new_block
        if self.opt_methods_["LoopUnrolling"]:
            unrolllooppass = UnrollLoopPass()
            unrolllooppass(node)
        if self.opt_methods_["Prefetching"] or self.opt_methods_["ReadonlyCaching"]:
            prefetchingldgpass = PrefetchingLDGPass(self.fields_, self.opt_methods_)
            prefetchingldgpass(node)
        return node

    def visit_StencilImplementation(self, node: gt_ir.StencilImplementation):
        self.fields_ = node.fields
        for i in range(len(node.multi_stages)):
            multi_stage = node.multi_stages[i]
            exec_order = str(multi_stage.iteration_order).lower()
            for j in range(len(multi_stage.groups)):
                group = multi_stage.groups[j]
                for k in range(len(group.stages)):
                    stage = group.stages[k]
                    self.visit(stage, exec_order=exec_order)
                    # node.multi_stages[i].groups[j].stages[k] = deepcopy(new_stage)
        return node


class MultiStageGraphCreator(gt_ir.IRNodeVisitor):
    def __call__(self, node: gt_ir.StencilImplementation) -> Dict[str, Set[str]]:
        self.name: str = ""
        self.graph: Dict[str, Set[str]] = dict()
        self.reads: Dict[str, Set[str]] = dict()
        self.writes: Dict[str, Set[str]] = dict()
        self.visit(node)
        return self.graph

    def _add_edge(self, graph: Dict[str, Set[str]], source_node: str, dest_node: str) -> None:
        if source_node not in graph:
            graph[source_node] = set()
        graph[source_node].add(dest_node)

    def visit_MultiStage(self, node: gt_ir.MultiStage) -> None:
        self.name = node.name
        self.graph[self.name] = set()
        self.reads.clear()
        self.generic_visit(node)

        for field_name in self.reads:
            if field_name in self.writes:
                for multistage in self.writes[field_name]:
                    if multistage != self.name:
                        self._add_edge(self.graph, self.name, multistage)

    def visit_FieldRef(self, node: gt_ir.FieldRef, **kwargs: Any) -> None:
        if "write_field" not in kwargs or kwargs["write_field"] == "":
            self._add_edge(self.writes, node.name, self.name)
        else:
            self._add_edge(self.reads, node.name, self.name)

    def visit_Assign(self, node: gt_ir.Assign, **kwargs: Any) -> None:
        kwargs["write_field"] = ""
        self.visit(node.target, **kwargs)
        kwargs["write_field"] = node.target.name
        self.visit(node.value, **kwargs)


class OptExtGenerator(gt_backend.GTPyExtGenerator):

    TEMPLATE_FILES = {
        "computation.hpp": "new_computation.hpp.in",
        "computation.src": "new_computation.src.in",
        "bindings.cpp": "bindings.cpp.in",
    }
    COMPUTATION_FILES = ["computation.hpp", "computation.src"]
    BINDINGS_FILES = ["bindings.cpp"]
    BLOCK_SIZES = (256, 1, 1)
    ITERATORS = [iter.lower() for iter in gt_definitions.CartesianSpace.names]

    def __init__(self, class_name, module_name, gt_backend_t, options):
        super().__init__(class_name, module_name, gt_backend_t, options)
        self.access_map_: Dict[str, Any] = dict()
        self.tmp_fields_: Dict[str, bool] = dict()
        self.curr_stage_: str = ""
        self.last_interval_: List[Dict[str, int]] = list()
        self.fuse_k_loops_: bool = "cuda" not in self.gt_backend_t
        self.splitters_: Tuple[str, ...] = ()

    def _compute_max_threads(self, block_sizes: tuple, max_extent: gt_definitions.Extent):
        max_threads = 0
        extra_threads = 0
        max_extents = []
        for pair in tuple(max_extent):
            max_extents.extend(list(pair))
        if "cuda" in self.gt_backend_t:
            extra_thread_minus = 0  # 1 if max_extents[0] < 0 else 0
            extra_thread_plus = 0  # 1 if max_extents[1] > 0 else 0
            extra_threads = extra_thread_minus + extra_thread_plus
            max_threads = block_sizes[0] * (
                block_sizes[1] + max_extents[3] - max_extents[2] + extra_threads
            )
        return max_extents, max_threads, extra_threads

    def _format_source(self, source):
        try:
            proc = sub.run(["clang-format"], stdout=sub.PIPE, input=source, encoding="ascii")
            if proc.returncode == 0:
                return proc.stdout
        except FileNotFoundError:
            pass
        return source

    def _format_conditional(self, entry_conditional):
        for iterator in self.ITERATORS:
            entry_conditional = (
                entry_conditional.replace(f"eval.{iterator}()", f"{iterator} - {iterator}_min")
                .replace(f"domain_size_{iterator.upper()}()", f"domain_size_{iterator}")
                .replace("<gt::int_t>", "<int>")
            )
        for splitter in self.splitters_:
            entry_conditional = entry_conditional.replace(f"{splitter}()", splitter)
        entry_conditional = entry_conditional.replace("eval", "")
        return entry_conditional

    def _format_iter_tuple(self, offset: List[int]):
        iter_tuple = []
        for i in range(len(offset)):
            iterator = self.ITERATORS[i]
            if offset[i] != 0:
                operator = "+" if offset[i] > 0 else ""
                iter_tuple.append(iterator + operator + str(offset[i]))
            else:
                iter_tuple.append(iterator)
        return iter_tuple

    def visit_BinOpExpr(self, node: gt_ir.BinOpExpr):
        if node.op.python_symbol == "**" and node.rhs.value == 2:
            node.op = gt_ir.BinaryOperator.MUL
            node.rhs = node.lhs
        return super().visit_BinOpExpr(node)

    def visit_FieldRef(self, node: gt_ir.FieldRef, *, ignore_check: bool = False, **kwargs):
        if not ignore_check:
            assert node.name in self.apply_block_symbols
        offset = [node.offset.get(name, 0) for name in node.offset]

        iter_tuple = self._format_iter_tuple(offset)

        data_type = "temp" if node.name in self.tmp_fields_ else "data"
        idx_key = f"{data_type}_" + "".join(iter_tuple)
        if idx_key not in self.access_map_:
            suffix = idx_key.replace(",", "").replace("+", "p").replace("-", "m")
            idx_name = f"idx_{suffix}"
            stride_name = f"{data_type}_strides"
            strides = [f"(({iter_tuple[i]}) * {stride_name}[{i}])" for i in range(len(iter_tuple))]
            idx_expr = " + ".join(strides)
            self.access_map_[idx_key] = dict(
                name=idx_name, expr=idx_expr, itype="int", stages=set()
            )

        self.access_map_[idx_key]["stages"].add(self.curr_stage_)
        return node.name + "[" + self.access_map_[idx_key]["name"] + "]"

    def visit_LDGFieldRef(self, node: LDGFieldRef):
        return f"__ldg(&{self.visit(node.fieldref)})"

    def visit_L1Prefetch(self, node: L1Prefetch):
        return f"__prefetch_global_l1(&{self.visit(node.fieldref)});"

    def visit_VarRef(self, node: gt_ir.VarRef, *, write_context=False, ignore_check=False):
        assert isinstance(node, gt_ir.VarRef)
        if not ignore_check:
            assert node.name in self.apply_block_symbols

        if write_context and node.name not in self.declared_symbols and not ignore_check:
            self.declared_symbols.add(node.name)
            source = self._make_cpp_type(self.apply_block_symbols[node.name].data_type) + " "
        else:
            source = ""

        idx = ", ".join(str(i) for i in node.index) if isinstance(node.index, Iterable) else ""
        if len(idx) > 0:
            idx = f"({idx})"
        if node.name in self.impl_node.parameters:
            source += "{name}{idx}".format(name=node.name, idx=idx)
        else:
            source += "{name}".format(name=node.name)
            if node.isarray:
                source += "[{idx}]".format(idx=node.index)

        return source

    def visit_Assign(self, node: gt_ir.Assign, **kwargs):
        lhs = self.visit(node.target, write_context=True, **kwargs)
        rhs = self.visit(node.value, **kwargs)
        source = "{lhs} = {rhs};".format(lhs=lhs, rhs=rhs)

        return [source]

    def visit_ApplyBlock(self, node: gt_ir.ApplyBlock):
        interval_definition = self.visit(node.interval)
        body_sources = gt_text.TextBlock()
        self.declared_symbols = set()
        for name, var_decl in node.local_symbols.items():
            # assert isinstance(var_decl, gt_ir.VarDecl)
            # body_sources.append(self._make_cpp_variable(var_decl))
            self.declared_symbols.add(name)
        self.apply_block_symbols = {**self.stage_symbols, **node.local_symbols}
        body_sources.extend(self.visit(node.body))

        return interval_definition, body_sources.text

    def visit_Stage(self, node: gt_ir.Stage):
        self.curr_stage_ = node.name
        stage_data = super().visit_Stage(node)
        stage_data["name"] = node.name
        stage_data["extents"] = []

        compute_extent = node.compute_extent
        for i in range(compute_extent.ndims):
            stage_data["extents"].extend(
                [compute_extent.lower_indices[i], compute_extent.upper_indices[i]]
            )

        stages: List[Dict[str, Any]] = list()
        for i in range(len(node.apply_blocks)):
            apply_block = node.apply_blocks[i]
            interval_start = apply_block.interval.start
            start_level = "min" if interval_start.level == gt_ir.LevelMarker.START else "max"
            start_offset = interval_start.offset

            interval_end = apply_block.interval.end
            end_level = "min" if interval_end.level == gt_ir.LevelMarker.START else "max"
            end_offset = interval_end.offset

            # TODO: Determine why this is needed...
            if end_level == "min" and end_offset > 0:
                end_offset -= 1
            elif start_level == "max" and start_offset < 0:
                start_offset += 1

            interval = [
                dict(level=start_level, offset=start_offset),
                dict(level=end_level, offset=end_offset),
            ]

            sub_stage = stage_data.copy()
            region = sub_stage["regions"][i]
            entry_conditional = (
                region["entry_conditional"] if "entry_conditional" in region else ""
            )
            if entry_conditional:
                entry_conditional = self._format_conditional(entry_conditional)

            sub_stage["body"] = region["body"]
            sub_stage["entry_conditional"] = entry_conditional
            sub_stage["interval"] = interval if interval != self.last_interval_ else []
            sub_stage["local_decls"] = [
                self._make_cpp_variable(var_decl)
                for name, var_decl in apply_block.local_symbols.items()
            ]
            sub_stage["IJKLoop"] = (
                apply_block.metadata["IJKLoop"] if "IJKLoop" in apply_block.metadata else False
            )
            sub_stage["unroll_num"] = (
                apply_block.metadata["unroll_num"] if "unroll_num" in apply_block.metadata else 1
            )
            sub_stage["init_read_stmt"] = [
                self.visit(stmt, ignore_check=True)[0] for stmt in apply_block.init_stmt
            ]
            del sub_stage["regions"]
            stages.append(sub_stage)

            if self.fuse_k_loops_:
                self.last_interval_ = interval.copy()

        return stages

    def visit_StencilImplementation(self, node: gt_ir.StencilImplementation):
        # Build multistage dataflow graph...
        graph_creator = MultiStageGraphCreator()
        multi_stage_graph = graph_creator(node)
        # print(multi_stage_graph)

        max_extent = functools.reduce(
            lambda a, b: a | b, node.fields_extents.values(), gt_definitions.Extent.zeros()
        )
        halo_sizes: Tuple[int, ...] = tuple(
            int(max(lower, upper)) for lower, upper in max_extent.to_boundary()
        )
        constants: Dict[str, str] = {}
        if node.externals:
            for name, value in node.externals.items():
                value = self._make_cpp_value(name)
                if value is not None:
                    constants[name] = value

        arg_fields: List[Any] = []
        tmp_fields: List[Dict[str, Any]] = []
        storage_ids: List[int] = []
        block_sizes: List[int] = list(self.BLOCK_SIZES)

        optimization_methods = _get_optimization_methods()
        if len(optimization_methods["BlocksizeAdjusting"].split(",")) == 3:
            block_sizes = [int(i) for i in optimization_methods["BlocksizeAdjusting"].split(",")]
            markijklooppass = MarkIJKLoopPass()
            offsets = markijklooppass(node)
            if not offsets[0] and not offsets[1]:
                block_sizes[1] = 1

        max_ndim: int = 0
        for name, field_decl in node.fields.items():
            if name not in node.unreferenced:
                max_ndim = max(max_ndim, len(field_decl.axes))
                field_attributes = {
                    "name": field_decl.name,
                    "dtype": self._make_cpp_type(field_decl.data_type),
                    "naxes": len(field_decl.axes),
                    "selector": tuple(
                        axis in field_decl.axes for axis in self.impl_node.domain.axes_names
                    ),
                }
                if field_decl.is_api:
                    if field_decl.layout_id not in storage_ids:
                        storage_ids.append(field_decl.layout_id)
                    field_attributes["layout_id"] = storage_ids.index(field_decl.layout_id)
                    arg_fields.append(field_attributes)
                else:
                    tmp_fields.append(field_attributes)
                    self.tmp_fields_[name] = True

        parameters: List[Dict[str, Any]] = [
            {"name": parameter.name, "dtype": self._make_cpp_type(parameter.data_type)}
            for name, parameter in node.parameters.items()
            if name not in node.unreferenced
        ]

        self.splitters_ = (
            tuple(splitter.name for splitter in gt_utils.flatten_iter(node.splitters))
            if hasattr(node, "splitters")
            else ()
        )

        optimization_pass = OptimizationPass(optimization_methods)
        optimization_pass.apply(node)
        multi_stages: List[Dict[str, Any]] = list()
        for multi_stage in node.multi_stages:
            stages: List[Dict[str, Any]] = list()
            self.last_interval_.clear()

            for group in multi_stage.groups:
                for stage in group.stages:
                    stages.extend(self.visit(stage))

            multi_stages.append(
                {
                    "name": f"{multi_stage.name}",
                    "exec": str(multi_stage.iteration_order).lower(),
                    "stages": stages,
                }
            )

        max_extents, max_threads, extra_threads = self._compute_max_threads(
            block_sizes, max_extent
        )

        template_args = dict(
            arg_fields=arg_fields,
            constants=constants,
            gt_backend=self.gt_backend_t,
            halo_sizes=halo_sizes,
            module_name=self.module_name,
            multi_stages=multi_stages,
            parameters=parameters,
            splitters=self.splitters_,
            stencil_unique_name=self.class_name,
            tmp_fields=tmp_fields,
            max_ndim=max_ndim,
            access_vars=list(self.access_map_.values()),
            multi_stage_graph=multi_stage_graph,
            block_sizes=block_sizes,
            max_extents=max_extents,
            max_threads=max_threads,
            extra_threads=extra_threads,
            do_k_parallel=False,
            debug=False,
            profile=True,
        )

        sources: Dict[str, Dict[str, str]] = {"computation": {}, "bindings": {}}
        for key, template in self.templates.items():
            source = self._format_source(template.render(**template_args))
            if key in self.COMPUTATION_FILES:
                sources["computation"][key] = source
            elif key in self.BINDINGS_FILES:
                sources["bindings"][key] = source

        return sources


@gt_backend.register
class CXXOptBackend(gt_backend.GTX86Backend):
    PYEXT_GENERATOR_CLASS = OptExtGenerator
    GT_BACKEND_T = "x86"
    _CPU_ARCHITECTURE = GT_BACKEND_T
    name = "cxxopt"


@gt_backend.register
class CUDABackend(gt_backend.GTCUDABackend):
    PYEXT_GENERATOR_CLASS = OptExtGenerator
    GT_BACKEND_T = "cuda"
    name = "cuda"
