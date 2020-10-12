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
from typing import Any, Dict, List

from gt4py import backend as gt_backend
from gt4py import definitions as gt_definitions
from gt4py import ir as gt_ir


class OptExtGenerator(gt_backend.GTPyExtGenerator):

    TEMPLATE_FILES = {
        "computation.hpp": "new_computation.hpp.in",
        "computation.src": "new_computation.src.in",
        "bindings.cpp": "bindings.cpp.in",
    }
    COMPUTATION_FILES = ["computation.hpp", "computation.src"]
    BINDINGS_FILES = ["bindings.cpp"]
    BLOCK_SIZES = (32, 8, 1)

    def __init__(self, class_name, module_name, gt_backend_t, options):
        super().__init__(class_name, module_name, gt_backend_t, options)
        self.access_map_ = dict()
        self.tmp_fields_ = dict()
        self.curr_stage_ = ""

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

    def visit_BinOpExpr(self, node: gt_ir.BinOpExpr):
        if node.op.python_symbol == "**" and node.rhs.value == 2:
            node.op = gt_ir.BinaryOperator.MUL
            node.rhs = node.lhs
        return super().visit_BinOpExpr(node)

    def visit_FieldRef(self, node: gt_ir.FieldRef, **kwargs):
        assert node.name in self.apply_block_symbols
        offset = [node.offset.get(name, 0) for name in node.offset]

        iter_tuple = []
        iterators = [iter.lower() for iter in gt_definitions.CartesianSpace.names]
        for i in range(len(offset)):
            iter = iterators[i]
            if offset[i] != 0:
                oper = ""
                if offset[i] > 0:
                    oper = "+"
                iter_tuple.append(iter + oper + str(offset[i]))
            else:
                iter_tuple.append(iter)

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
        return super().visit_Stage(node)

    def visit_StencilImplementation(self, node: gt_ir.StencilImplementation):
        max_extent = functools.reduce(
            lambda a, b: a | b, node.fields_extents.values(), gt_definitions.Extent.zeros()
        )
        halo_sizes = tuple(max(lower, upper) for lower, upper in max_extent.to_boundary())
        constants = {}
        if node.externals:
            for name, value in node.externals.items():
                value = self._make_cpp_value(name)
                if value is not None:
                    constants[name] = value

        arg_fields = []
        tmp_fields = []
        storage_ids = []
        block_sizes = self.BLOCK_SIZES

        max_ndim = 0
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

        parameters = [
            {"name": parameter.name, "dtype": self._make_cpp_type(parameter.data_type)}
            for name, parameter in node.parameters.items()
            if name not in node.unreferenced
        ]

        multi_stages = []
        for multi_stage in node.multi_stages:
            steps: List[Dict[str, Any]] = []
            last_interval: List[Dict[str, Any]] = []

            n_multi_stages = 0
            for group in multi_stage.groups:
                for stage in group.stages:
                    stage_start = stage.apply_blocks[0].interval.start
                    start_level = "min" if stage_start.level == gt_ir.LevelMarker.START else "max"
                    stage_end = stage.apply_blocks[0].interval.end
                    end_level = "min" if stage_end.level == gt_ir.LevelMarker.START else "max"
                    interval = [
                        dict(level=start_level, offset=stage_start.offset),
                        dict(level=end_level, offset=stage_end.offset),
                    ]

                    # Force a new multi-stage when intervals change...
                    if len(last_interval) > 0 and interval != last_interval:
                        if last_interval[1]["level"] == "min":
                            last_interval[1]["offset"] -= 1
                        multi_stages.append(
                            {
                                "name": f"{multi_stage.name}_{n_multi_stages}",
                                "exec": str(multi_stage.iteration_order).lower(),
                                "interval": last_interval,
                                "steps": steps,
                            }
                        )
                        n_multi_stages += 1
                        steps.clear()
                    last_interval = interval

                    extents: List[int] = []
                    compute_extent = stage.compute_extent
                    for i in range(compute_extent.ndims):
                        extents.extend(
                            [compute_extent.lower_indices[i], compute_extent.upper_indices[i]]
                        )

                    step = self.visit(stage)
                    step["stage_name"] = stage.name
                    step["extents"] = extents
                    steps.append(step)

            multi_stages.append(
                {
                    "name": f"{multi_stage.name}_{n_multi_stages}",
                    "exec": str(multi_stage.iteration_order).lower(),
                    "interval": last_interval,
                    "steps": steps,
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
            stencil_unique_name=self.class_name,
            tmp_fields=tmp_fields,
            max_ndim=max_ndim,
            access_vars=list(self.access_map_.values()),
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
