capture log close
* Long & Freese 3rd edtion - Chapter 2 tutorial | 2014-01-26
log using tutorial, replace text
version 13.1
clear all
set linesize 80
macro drop _all
set scheme s2manual

// loading and examining the data

use science4, clear
codebook, compact

//  examining individual variables

summarize work
tabulate work, missing

//  graphing variables

dotplot pub1
graph export tutorial_dotplot.emf, replace

//  creating a dummy variable

generate isfac = (work==1) if work<.

//  checking transformations

tabulate isfac work, missing

//  labeling variables and values

label variable isfac "Scientist is faculty member in university"
label define isfac 0 "NotFac" 1 "Faculty"
label values isfac isfac

tabulate isfac

//  creating an ordinal variable

tab job, missing

generate jobprst = job
recode jobprst .=. 1/1.99=1 2/2.99=2 3/3.99=3 4/5=4
label variable jobprst "Ranking of university job"
label define prstlbl 1 "Adeq" 2 "Good" 3 "Strong" 4 "Elite"
label values jobprst prstlbl

tabulate jobprst, missing

//  combining variables

generate pubsum = pub3 + pub6 + pub9
label variable pubsum "Total publications in 9 years since PhD"
summarize pub3 pub6 pub9 pubsum

graph matrix pub3 pub6 pub9 pubsum, ///
    half msymbol(smcircle_hollow)
graph export tutorial_graph_matrix.emf, replace

//  saving the new data

label data "sciwork.dta | revised science4 data | 2014-01-24"
note _dta: "Revised by Scott Long | tutorial.do | 2014-01-24"
save sciwork, replace

//  closing the log

log close
exit
