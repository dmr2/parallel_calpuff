PARALLEL CALPUFF TUTORIAL FOR ARB

Written by: D.J. Rasmussen   December 2012
	    UC-Davis 
	    dmr@ucdavis.edu


These are instructions for running an 8-day test simulation with 100 area sources as defined in Section 2 of the parallel CAPUFF ARB report.

The test scenario produces concentration fields for 7 species at gridded receptors only. The line printing option is turned-off in the control file.
Each process will create its own log or list file, e.g. "calpuff.lst0800" for master process. The first two numeric digits indicate total number of processors.
The second two numeric digits specify the process number (0 through nprocs-1)

INSTRUCTIONS:

 1. Untar and unzip "parallel_CALPUFF.tar.gz"

 2. Make object files in the directory "f90_src" with mpif90
 
 3. Build executable with make file in main directory with mpif90

 4. Open simulation control file "parallel_test.inp" and specify location of met data and location to write concentration file

 5. Start a ring with mpd and create a machines file, "machines"

 6. Copy built executable, control file, machines file, and run script to the same directory on the scratch drive of "master" node

 7. Copy built executable, control file, and machines file to same directory on the scratch drives of all worker nodes

 8. Open run script, "run", and adjust number of processors as needed, i.e.

   "exec -machinefile machines -np 4 ./calpuff.exe < /dev/null &" for 4 processors....

   "exec -machinefile machines -np 32 ./calpuff.exe < /dev/null &" for 32 processors....

 9. Start parallel CALPUFF and write output to a log file:

       [user@host]$ ./run > log 2>&1 &


