step2fds
========

step2fds is a collection of scripts and programs that reads step files, and converts the contained B-splines to FDS input file format. This is a very rudamentary piece of software, which I put together from bits of code I had wwritten as part of other projects.

It all started because I needed to get a relatively complex geometry with lots of curvy walls into FDS. I didn't have access to PyroSim (and it didn't quite live up to my expectations when I tried it), and writing my own "converter" seemed the simplest thing to do.

The collection consists of a bash script (which runs all the other scripts), 2 Python scripts (one to read the step file and summarize the important information, and one to write the FDS input file), and a Fortran program that produces the B-splines from the information contained in the step files (i.e. the summary produced by the Python script mentioned above). I used the Fortran program simply because I had done some work on B-splines earlier in my life and couldn't be bothered at the time to re-write it all in Python.

How to use this collection of code:

1. Trace the lines of your geometry using Splines (even for straight lines), and export to STEP format.
2. Save the step files in the stepfiles directory.
3. go to bin/ and compile the Fortran program by typing 'make' at the command prompt. You'll need a fortran compiler (GCC is recommended, but any should work-just make sure you change the Makefile) and makedepf90 (http://personal.inet.fi/private/erikedelmann/makedepf90/)
4. go to the scripts directory and adjust the values in spline2fds.sh (max_x, max_y etc)
5. Type './spline2fds.sh' at the command prompt.
6. Your FDS input file should appear in the fds_files directory (will take a few seconds, depending on the mesh density you need)
