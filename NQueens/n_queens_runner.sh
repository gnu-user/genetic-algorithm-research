#!/bin/bash
###############################################################################
#
# A helpful script that is a runner for executing a series of sharcnet jobs for
# the N Queens Problem.
# 
# Copyright (C) 2013, Jonathan Gillett
# All rights reserved.
#
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
###############################################################################

OUTPUT_DIR=""
LOG_DIR="/scratch/jgillett/log/n_queens/"
RUN_NUMBER_SHORT=(1)
RUNS_SHORT=30
RUN_NUMBER_LONG=(1 16)
RUNS_LONG=15
MUTATION_RATES=(0.01 $(seq 0.05 0.05 1.0))
QUEENS_SHORT=($(seq 8 1 12))
QUEENS_LONG=($(seq 13 1 16))
ALL_QUEENS=(${QUEENS_SHORT[@]} ${QUEENS_LONG[@]})


# Make sure the user provided the path
if [[ -z "$1" ]]
then
    echo "Usage: $0 {output directory}" 1>&2
    echo "Example: $0 n_queens/" 1>&2
    exit 1
fi

# Make sure the directories exist
if [[ -d "$1" ]]
then
    OUTPUT_DIR="$1"
else
    echo "Directory(s) provided do not exist!" 1>&2
    exit 1
fi

# Make sure the output directory has no files in it
if [[ "$(ls -A $OUTPUT_DIR)" ]]
then
     echo "Output directory contains files, choose a different directory!" 1>&2
     exit 1
fi

# Make a log directory for the sharcnet job execution logs
for queen_dir in ${ALL_QUEENS[@]}
do
    if [[ ! -d "$LOG_DIR/${queen_dir}" ]]
    then
        mkdir -p "$LOG_DIR/${queen_dir}"
    fi
done


# Execute multiple jobs for each N Queens short problem
for run_num in ${RUN_NUMBER_SHORT[@]}
do
    for queen in ${QUEENS_SHORT[@]}
    do
        # Execute the job for variable mutation rate
        log_file=q_${queen}_variable_${run_num}.log
        sqsub -q serial -r 7d --mpp 6G -o ${LOG_DIR}/${queen}/${log_file} n_queens.sh -o "${OUTPUT_DIR}" -r ${run_num} -n ${RUNS_SHORT} -q ${queen} -s 0

        # Execute the job for each of the fixed mutation rates
        for rate in ${MUTATION_RATES[@]}
        do
            log_file=q_${queen}_${rate}_${run_num}.log
            sqsub -q serial -r 7d --mpp 6G -o ${LOG_DIR}/${queen}/${log_file} n_queens.sh -o "${OUTPUT_DIR}" -r ${run_num} -n ${RUNS_SHORT} -m ${rate} -q ${queen} -s 0
        done
    done
done


# Execute multiple jobs for each N Queens long problem
for run_num in ${RUN_NUMBER_LONG[@]}
do
    for queen in ${QUEENS_LONG[@]}
    do
        # Execute the job for variable mutation rate
        log_file=q_${queen}_variable_${run_num}.log
        sqsub -q serial -r 7d --mpp 6G -o ${LOG_DIR}/${queen}/${log_file} n_queens.sh -o "${OUTPUT_DIR}" -r ${run_num} -n ${RUNS_LONG} -q ${queen} -s 0

        # Execute the job for each of the fixed mutation rates
        for rate in ${MUTATION_RATES[@]}
        do
            log_file=q_${queen}_${rate}_${run_num}.log
            sqsub -q serial -r 7d --mpp 6G -o ${LOG_DIR}/${queen}/${log_file} n_queens.sh -o "${OUTPUT_DIR}" -r ${run_num} -n ${RUNS_LONG} -m ${rate} -q ${queen} -s 0
        done
    done
done
