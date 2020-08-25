#!/bin/bash
envloc=$1
RUN_CMD_FILE=$2
maxsleep=9000

. ${envloc}/env/machineEnvironment.sh
slurmscript="${envloc}/env/submit.${host}.slurm"
out="gt4py_tests_${BUILD_ID}.out"
if [ "${useslurm}" = true ] ; then
    # setup SLURM job
    /bin/sed -i 's|<NAME>|jenkins-gt4py-tests|g' ${slurmscript}
fi
${envloc}/env/runJob.sh ${RUN_CMD_FILE} ${slurmscript}
