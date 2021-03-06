#!/bin/bash
###############################################################################
#
# Fix files that have duplicate results
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
##############################################################################
DATA_DIR=""

# Make sure the user provided the path
if [[ -z "$1" ]]
then
    echo "No path provided" 1>&2
    echo "Usage: fix_duplicates.sh {data dir}" 1>&2
    exit 1
fi

# Make sure the directories exist
if [[ -d "$1" ]]
then
    DATA_DIR="$1"
else
    echo "Directory provided do not exist!" 1>&2
    exit 1
fi


for file in ${DATA_DIR}/chromosome_similarity_{1..14}.csv
do
    head -n10001 $file > tmpfile
    mv tmpfile $file
done


for file in ${DATA_DIR}/duplicate_solutions_{1..14}.csv
do
    head -n10001 $file > tmpfile
    mv tmpfile $file
done


for file in ${DATA_DIR}/fitness_stats_{1..14}.csv
do
    head -n10001 $file > tmpfile
    mv tmpfile $file
done


# Keep only the first results for solution generation
for file in ${DATA_DIR}/solution_generation_{1..14}.csv
do
    # Get the line number where it is duplicated
    line_num=$(egrep -ne "^1," ${file} | egrep -ve "^2:" | cut -f1 -d:)

    # Keep only the first results, remove duplicates
    if [[ ! -z $line_num ]]
    then
        line_num=$((${line_num} - 1))
        head -n${line_num} $file > tmpfile
        mv tmpfile $file
    fi
done
