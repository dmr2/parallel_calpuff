#!/bin/bash
#------------------------------------------------------------------
# run_parallel_calpuff.sh
#  ___               _ _     _    ___   _   _    ___ _   _ ___ ___ 
# | _ \__ _ _ _ __ _| | |___| |  / __| /_\ | |  | _ \ | | | __| __|
# |  _/ _` | '_/ _` | | / -_) | | (__ / _ \| |__|  _/ |_| | _|| _| 
# |_| \__,_|_| \__,_|_|_\___|_|  \___/_/ \_\____|_|  \___/|_| |_|
#
# Script written by D.J. Rasmussen and Mike Kleeman
# University of California, Davis 95616
#
# Parallel CALPUFF written by D.J. Rasmussen and Mike Kleeman
# University of California, Davis 95616
#
# Parallel CALPUFF is a parallelized variant of CALPUFF v5.8 
# originally written by J. Scire, Earth Tech
#
# The purpose of this script is to execute parallel CALPUFF
# across a pre-determined ring of nodes
#
#------------------------------------------------------------------
 MODEL_DIR=/data/r008a/dmr/models/CALPUFF/for_CARB/parallel_CALPUFF/
 EXEC=calpuff.exe
 # control file must be in the model directory with executable with this script
 CONTROL=parallel_test.inp
 
# DERIVED PARAMETERS
# ------------------
 USER_ID=${USER}   
 hostname=`hostname`
 date=`date`
 MASTER=`echo $(hostname) | cut -d '.' -f 1-1`
 WDIR=/scratch/${USER_ID}/calpuff/master_${MASTER}

# BUILD THE MACHINE FILE
# -----------------------
 MACHINE_FILE='machines.'${hostname%%.*}  # machine file to be used for the run
 NUMPROC=0                                # number of processors
 rm -f $MACHINE_FILE
 RUN_GROUP=( ${hostname%%.*} `mpdtrace | sort` ) # list of all nodes in rungroup
 RUN_GROUP_SIZE=${#RUN_GROUP[@]}
 ICOUNT=1
 while [ "$ICOUNT" -lt "$RUN_GROUP_SIZE" ]; do
  if [ "${RUN_GROUP[$ICOUNT]}" = "${hostname%%.*}" ]; then
   unset RUN_GROUP[$ICOUNT]
  fi
  ((ICOUNT++))
 done
 for NODE in ${RUN_GROUP[@]} 
 do
#  NCORE=`rsh ${NODE} "grep processor /proc/cpuinfo | sort -u | wc -l"`
  NCORE=`mpiexec -recvtimeout 3 -host ${NODE} count_core.exe < /dev/null`
  echo ${NODE}:$NCORE >> $MACHINE_FILE
  NUMPROC=`expr $NUMPROC + $NCORE`
 done 
 cp $MACHINE_FILE $MODEL_DIR

# COPY CALPUFF CODE FROM COMMON CODE DEPOSITORY TO LOCAL DIRECTORY
# ----------------------------------------------------------------
 echo 'Copying executables and control files to local directories'
 echo $RUN_GROUP
 for NODE in ${RUN_GROUP[@]} 
 do
   cmd1="cp ${MODEL_DIR}/${MACHINE_FILE} ${WDIR}"
   cmd2="cp ${MODEL_DIR}/${EXEC} ${WDIR}"
   cmd3="cp ${MODEL_DIR}/${CONTROL} ${WDIR}"
   cmd4="export pufinp=${WDIR}/${CONTROL}"
   echo "${NODE}: mkdir -p ${WDIR}"
   echo "${NODE}: ${cmd1}"
   echo "${NODE}: ${cmd2}"
   echo "${NODE}: ${cmd3}"
   echo "${NODE}: ${cmd4}"
   mpiexec -recvtimeout 3 -host ${NODE} run.exe mkdir -p ${WDIR} < /dev/null
   mpiexec -recvtimeout 3 -host ${NODE} run.exe ${cmd1} < /dev/null
   mpiexec -recvtimeout 3 -host ${NODE} run.exe ${cmd2} < /dev/null
   mpiexec -recvtimeout 3 -host ${NODE} run.exe ${cmd3} < /dev/null
   mpiexec -recvtimeout 3 -host ${NODE} run.exe ${cmd4} < /dev/null
done 

# RECORD MODEL START TIME
# -----------------------------------
 echo ' '
 echo PARALLEL CALPUFF MODEL -- RUN: $RUNCODE
 date
 echo ' '

# run the parallel CALPUFF code
  cat << EOF2 | mpiexec -machinefile ${WDIR}/${MACHINE_FILE} -wdir ${WDIR} -n $NUMPROC ./${EXEC} < /dev/null
EOF2

# DOCUMENT MODEL END DATE AND QUIT PROGRAM
echo ' '
echo PARALLEL CALPUFF MODEL RUN $RUNCODE COMPLETED on $MASTER at `date`
date
echo ' '
