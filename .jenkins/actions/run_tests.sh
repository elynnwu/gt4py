#!/usr/bin/env bash
python3 -m pip install --user virtualenv
python3 -m venv gt4pyenv
source gt4pyenv/bin/activate
pip3 install --upgrade wheel 
git clone https://github.com/VulcanClimateModeling/gt4py.git gt4py
pip3 install cupy-cuda102==7.7.0 pytest==6.1.2 hypothesis==5.41.4 clang-format==9.0.0
pip3 install -r gt4py/requirements-dev.txt
pip3 install --no-cache-dir -e gt4py[cupy-cuda102]
python3 -m gt4py.gt_src_manager install
python3 -m pytest -xv -k "not gtc" gt4py/tests
