#!/bin/bash
###############################################################################
#
# Find files that have duplicate results
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


for file in $(find "${DATA_DIR}" -type f -name "*.csv")
do
  count=0
  prev_row=0
  
  for line in $(awk -F"," '{print $1}' ${file})
  do
      if [[ ${count} -eq 0 ]]
      then
          count=$((${count} +1))
          continue
      else
          if [[ ${line} -lt ${prev_row} ]]
          then
              echo "${file}: ${prev_row} ${line}"
              break
          fi
          prev_row=${line} 
      fi
   done
done
