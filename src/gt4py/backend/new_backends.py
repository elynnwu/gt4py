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

from gt4py import backend as gt_backend
from gt4py import definitions as gt_definitions
from gt4py import ir as gt_ir
from gt4py import utils as gt_utils


class MultiStageGraphCreator(gt_ir.IRNodeVisitor):
    def __call__(self, node: gt_ir.StencilImplementation) -> None:
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
    BLOCK_SIZES = (32, 8, 1)
    ITERATORS = [iter.lower() for iter in gt_definitions.CartesianSpace.names]

    def __init__(self, class_name, module_name, gt_backend_t, options):
        super().__init__(class_name, module_name, gt_backend_t, options)
        self.access_map_: Dict[str, Any] = dict()
        self.tmp_fields_: Dict[str, bool] = dict()
        self.curr_stage_: str = ""
        self.last_interval_: List[Dict[str, int]] = list()
        self.fuse_k_loops_: bool = "cuda" not in self.gt_backend_t
        self.splitters_: Tuple[str] = None

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
            entry_conditional = entry_conditional.replace(f"eval.{iterator}()", iterator)
        for splitter in self.splitters_:
            entry_conditional = entry_conditional.replace(f"{splitter}()", splitter)
        entry_conditional = entry_conditional.replace("eval", "")
        return entry_conditional

    def visit_BinOpExpr(self, node: gt_ir.BinOpExpr):
        if node.op.python_symbol == "**" and node.rhs.value == 2:
            node.op = gt_ir.BinaryOperator.MUL
            node.rhs = node.lhs
        return super().visit_BinOpExpr(node)

    def visit_FieldRef(self, node: gt_ir.FieldRef, **kwargs):
        assert node.name in self.apply_block_symbols
        offset = [node.offset.get(name, 0) for name in node.offset]

        iter_tuple = []
        for i in range(len(offset)):
            iterator = self.ITERATORS[i]
            if offset[i] != 0:
                operator = "+" if offset[i] > 0 else ""
                iter_tuple.append(iterator + operator + str(offset[i]))
            else:
                iter_tuple.append(iterator)

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

    def visit_VarRef(self, node: gt_ir.VarRef, *, write_context=False):
        assert node.name in self.apply_block_symbols

        if write_context and node.name not in self.declared_symbols:
            self.declared_symbols.add(node.name)
            source = self._make_cpp_type(self.apply_block_symbols[node.name].data_type) + " "
        else:
            source = ""

        idx = ", ".join(str(i) for i in node.index) if node.index else ""
        if len(idx) > 0:
            idx = f"({idx})"
        if node.name in self.impl_node.parameters:
            source += "{name}{idx}".format(name=node.name, idx=idx)
        else:
            source += "{name}".format(name=node.name)
            if idx:
                source += "[{idx}]".format(idx=idx)

        return source

    def visit_Stage(self, node: gt_ir.Stage):
        self.curr_stage_ = node.name
        stage_data = super().visit_Stage(node)
        stage_data["name"] = node.name
        stage_data["extents"]: List[int] = []

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
            if len(entry_conditional) > 0:
                entry_conditional = self._format_conditional(entry_conditional)

            sub_stage["body"] = region["body"]
            sub_stage["entry_conditional"] = entry_conditional
            sub_stage["interval"] = interval if interval != self.last_interval_ else []
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
        halo_sizes: Tuple[int] = tuple(
            max(lower, upper) for lower, upper in max_extent.to_boundary()
        )
        constants: Dict[str, str] = {}
        if node.externals:
            for name, value in node.externals.items():
                value = self._make_cpp_value(name)
                if value is not None:
                    constants[name] = value

        arg_fields: List[any] = []
        tmp_fields: List[str] = []
        storage_ids: List[int] = []
        block_sizes: List[int] = self.BLOCK_SIZES

        max_ndim: int = 0
        for name, field_decl in node.fields.items():
            if name not in node.unreferenced:
                max_ndim = max(max_ndim, len(field_decl.axes))
                field_attributes = {
                    "name": field_decl.name,
                    "dtype": self._make_cpp_type(field_decl.data_type),
                    "axes": "".join(field_decl.axes).lower(),
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
