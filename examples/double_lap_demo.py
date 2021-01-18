#!/usr/bin/env python

import sys
import time
import traceback as tb

import numpy as np

import gt4py
from gt4py import gtscript


try:
    import cupy as cp
except ImportError:
    cp = None


py_backend = "numpy"
# py_backend = 'debug'
cpu_backend = "cxxopt"
gpu_backend = "cuda"
# cpu_backend = 'gtmc'
# gpu_backend = 'gtcuda'
rebuild = True  # False
managed_mem = False  # True


@gtscript.function
def lap(in_f, dt=0.25):
    return (
        -4.0 * in_f[0, 0, 0] + in_f[1, 0, 0] + in_f[-1, 0, 0] + in_f[0, -1, 0] + in_f[0, 1, 0]
    ) / (dt * dt)


@gtscript.stencil(backend=py_backend, rebuild=rebuild)
def double_lap_py(in_f: gtscript.Field[float], out_f: gtscript.Field[float]):
    with computation(PARALLEL), interval(...):
        tmp_f = lap(in_f)
        out_f = lap(tmp_f)


@gtscript.stencil(backend=cpu_backend, rebuild=rebuild)
def double_lap_mc(in_f: gtscript.Field[float], out_f: gtscript.Field[float]):
    with computation(PARALLEL), interval(...):
        tmp_f = lap(in_f)
        out_f = lap(tmp_f)


if cp:

    @gtscript.stencil(backend=gpu_backend, rebuild=rebuild)
    def double_lap_cu(in_f: gtscript.Field[float], out_f: gtscript.Field[float]):
        with computation(PARALLEL), interval(...):
            tmp_f = lap(in_f)
            out_f = lap(tmp_f)


def main():
    # Allocate storages
    hsize = int(sys.argv[1])  # vsize * 2
    vsize = int(sys.argv[2]) if len(sys.argv) > 2 else hsize
    shape = (hsize, hsize, vsize)
    nhalo = 3

    domain = (hsize - nhalo * 2, hsize - nhalo * 2, vsize)
    origin = (nhalo, nhalo, 0)
    in_data = np.fromfunction(lambda i, j, k: np.sin(i) * np.sin(j), shape, dtype=float)

    in_fld_py = gt4py.storage.from_array(
        data=in_data, backend=py_backend, default_origin=origin, shape=shape
    )
    out_fld_py = gt4py.storage.zeros(
        shape=shape, default_origin=origin, dtype=float, backend=py_backend
    )

    in_fld_mc = gt4py.storage.from_array(
        data=in_data, backend=cpu_backend, default_origin=origin, shape=shape
    )
    out_fld_mc = gt4py.storage.zeros(
        shape=shape, default_origin=origin, dtype=float, backend=cpu_backend
    )

    if cp:
        in_fld_cu = gt4py.storage.from_array(
            data=in_data,
            backend=gpu_backend,
            default_origin=origin,
            shape=shape,
            managed_memory=managed_mem,
        )
        out_fld_cu = gt4py.storage.zeros(
            shape=shape,
            default_origin=origin,
            dtype=float,
            backend=gpu_backend,
            managed_memory=managed_mem,
        )

    t_start = time.perf_counter()
    double_lap_py(in_fld_py, out_fld_py, domain=domain)
    t_run = (time.perf_counter() - t_start) * 1e3
    print("%s time: %.6lf" % (py_backend, t_run))
    np.savetxt("double_lap_py_output.csv", out_fld_py.data.flatten(), delimiter=",")

    t_start = time.perf_counter()
    double_lap_mc(in_fld_mc, out_fld_mc, domain=domain)
    t_run = (time.perf_counter() - t_start) * 1e3
    print("%s time: %.6lf" % (cpu_backend, t_run))
    np.savetxt("double_lap_cpu_output.csv", out_fld_mc.data.flatten(), delimiter=",")

    if cp:
        t_start = time.perf_counter()
        double_lap_cu(in_fld_cu, out_fld_cu, domain=domain)
        t_run = (time.perf_counter() - t_start) * 1e3
        print("%s time: %.6lf" % (gpu_backend, t_run))
        out_fld_cu.synchronize()
        np.savetxt("double_lap_gpu_output.csv", np.asarray(out_fld_cu).flatten(), delimiter=",")


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt as e:  # Ctrl-C
        print("Closing gracefully on keyboard interrupt...")
    except Exception as e:
        print("ERROR: " + str(e))
        tb.print_exc()
