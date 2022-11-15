The simulations reported in my seminar paper were obtained using "SupWald-Functions.R" 
However Uliana kindly pointed out some things to me which I didn´t notice. I Have therefore added the 
corrected version of my function-file. There I have corrected the time indexes for the error terms
during data generation. And I have also trimmed the observations for x,y,q and z such that they all correspond
to the same time index.

Both files should work, however i haven´t tested the corrected file extensively due to lack of time.

In general I have structured my project as follows: 
The functions performing the calculations are all stored in "Sup_wald-Functions.R" (or the corrected version) and 
for the pv_sup routine the file provided by  https://www.ssc.wisc.edu/~bhansen/progs/progs_threshold.html.

Then for each simulation I started a new file which sources the functions from the other packages.
After installing the required packages a change of the working directory should suffice to re-run
the Simulations. However for best performance you might change in the function file at line 157. 
plan(multisession,workers=8), workers corresponds to the number of logical processors your device has.