In general I have structured my project as follows: 
The functions performing the calculations are all stored in "Sup_wald-Functions.R" (or the corrected version) and 
for the pv_sup routine the file provided by  https://www.ssc.wisc.edu/~bhansen/progs/progs_threshold.html.

Then for each simulation I started a new file which sources the functions from the other packages.
After installing the required packages a change of the working directory should suffice to re-run
the Simulations. However for best performance you might change in the function file at line 157. 
plan(multisession,workers=8), workers corresponds to the number of logical processors your device has.
