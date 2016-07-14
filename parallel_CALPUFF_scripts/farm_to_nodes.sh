#!/bin/bash
######################################################################
# Author: Tony Held
# Created: 2001
# Purpose : execute a quotated operation to a range of nodes on the network

# updated 07-21-2003 by tony held
# option to delay between commands now included
# options no longer have to be in order and defaults are used

# updated 04-23-2003 by tony held
# nodes are pinged to see if they are alive to speed the detection of dead nodes
# diagnostic information such as elasped time and node counts now output

# example of usage
# farm-to-nodes -nb 10 65 "mkdir /scratch/simpsons"  
# (this will create a directory named simpsons on nodes 10 to 65)
# farm-to-nodes -b 10 15 "ls" (list in order files from diferent machines)

######################################################################


function usage
{
    echo "------------------------------------------------------------"
    echo "$0 Usage"
    echo "farm-to-nodes must be called with at least 3 arguments"
    echo "farm-to-nodes.sh [options] n1 n2 \"string\""
    echo "  execute string on nodes n1 to n2 inclusive"
    echo "    -n      - execute in non-blocking mode (parallel), default"  
    echo "    -b      - execute in blocking mode (serial)"
    echo "    -d delay -  delay time in seconds prior to issuing each command"
    echo "                time can be an integer (i.e. 5) or real (i.e. 0.36) number"
    echo "------------------------------------------------------------"
}


################################################################################
# intialize default parameters
################################################################################
blocking_mode="nb"
delay_time="0"

################################################################################
# Find the command line switches and arguments
################################################################################
while getopts ":nbd:" opt; do
    case $opt in
	n  ) blocking_mode="non-blocking" ;;
	b  ) blocking_mode="blocking"  ;;
	d  ) delay_time=$OPTARG ;;
#	     echo "Delay time: $delay_time" ;;
	\? ) usage # give the user a hint on usage
	     exit 1 
    esac
done
shift $(($OPTIND -1))

if [ ! $num_args = 3 ] ; then 
    usage # give the user a hint on usage
    exit 1
fi

# assign command line arguments to shell variables
first_node=$1
last_node=$2
command=$3


################################################################################
# diagnostic output
################################################################################
echo 
echo "Farming tasks tasks started in $blocking_mode mode."
echo "Delay time of $delay_time between sending each command"
echo "It may take some-time before they complete on the remote system(s),"
echo "in addition - if the task generates output - it may appear unexpectedly"
echo

################################################################################
# initialize elaspsed time and other counters
################################################################################

TIME1=$(date +%s) # elasped time information
num_args=$#             # find out how many arguments where called
dead_nodes=0            # node counters
good_nodes=0


################################################################################
# loop through the specified node range and issue the command
################################################################################
        
node_number=$first_node # variable which stores the number of the node
node_name="nxxx"        # string to store the actual node name (i.e. n009)

#loop through each node in the cluster

while [ $node_number -le $last_node ] 
do
    if [ $node_number -lt 10 ] ; then
        node_name="n00$node_number"
    elif [ $node_number -lt 100 ] ; then
        node_name="n0$node_number"
    else
        node_name="n$node_number"
    fi

    case "$node_name" in
#    "n075" | "n076" )
#        echo "skipping historically bad node $node_name" ;;
    * )
	echo "--- executing command ($command) on $node_name ---"

    # find if the node is alive by giving it a 1 packet ping with a
    # maximum wait time of 1 second.  if the ping does not work
    # the exit code will make the if test fail.  

	if ping -q -c 1 -w 2 $node_name  >& /dev/null
	then
	    live_nodes=$(($live_nodes + 1))
	    # run task in background if you are in non-blocking mode
	    if [ $blocking_mode = "blocking" ] ; then
		/usr/bin/rsh $node_name "$command"
	    else [ $blocking_mode = "non-blocking" ] 
		/usr/bin/rsh $node_name "$command" &
	    fi
	else
	    dead_nodes=$(($dead_nodes + 1))
	    echo "****${node_name} failed ping test and was skipped*****"
	fi

    # go into sleep mode if you have a non zero delay time
	if [ $delay_time -gt 0 ] ; then
	    if [ $node_number -lt $last_node ] ; then
		echo "pausing $delay_time seconds using the sleep command"
		sleep $delay_time
	    fi
	fi
        ;;
    esac 

    # move to next node  
    node_number=$(($node_number + 1))
done

################################################################################
# Output diagnositics of commands
################################################################################

TIME2=$(date +%s)
ELTIME=$[ $TIME2 - $TIME1 ]

echo "-----------------------------------------------------------------"

if [ $blocking_mode = "blocking" ] ; then
    echo "Farmed commands complete! Elasped Time (integer seconds): $ELTIME "
else
    echo "Farmed commands sent to nodes.  Ultimate completion time unknown."
    echo "Time to issue commands (integer seconds): $ELTIME "
fi

echo "Based on pinging, $live_nodes nodes  alive, $dead_nodes  dead"
