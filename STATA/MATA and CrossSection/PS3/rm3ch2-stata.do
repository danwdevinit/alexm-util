capture log close
log using rm3ch2-stata, replace text

//  scott long & jeremy freese (jslong@indiana.edu & jfreese@northwestern.edu)
//  regression models for categorical outcomes using stata, 3rd
//  chapter 2: using stata | 2014-02-20

version 13.1
clear all
macro drop _all
set linesize 80
set scheme s2manual
program drop _all

//  __

use binlfp4, clear
tabulate hc wc, /// the next line is treated as a continuation of this one
     row nolabel

use binlfp4, clear
tabulate hc wc if age>40, row
summarize inc k5 wc
summarize
summarize l*

use binlfp4, clear
codebook lfp k5 k618 agecat wc hc lwg inc, compact
summarize inc, detail
tabulate hc
tabulate hc, nolabel
tabulate hc wc
tab1 hc wc
dotplot inc
graph export rm3ch2-dotplot.emf, replace
describe lfp k5 k618 agecat wc hc lwg inc
codebook inc

generate age2 = age
summarize age2 age

gen age3 = age if age>40
summarize age3 age

gen agesq = age^2
gen lnage = ln(age)

gen age4 = age
replace age4 = 40 if age<40
summarize age4 age

//  recode

use recodedata4, clear // note changed dataset name
recode origvar (1=2) (3=4), generate(myvar1)
recode origvar (2=1) (*=0), generate(myvar2)
recode origvar (2=1) (nonmissing=0), generate(myvar3)
recode origvar (1/4=2), generate(myvar4)
recode origvar (1 3 4 5=7), generate(myvar5)
recode origvar (min/5=min), generate(myvar6)
recode origvar (missing=9), generate(myvar7)

//  labels

use gsskidvalue4, clear
gen agesq = age*age
label variable agesq "age-squared of respondent"
codebook age agesq, compact

label variable agesq
codebook agesq, compact

label define Lyesno 1 yes 0 no
label define Lposneg4 1 veryN 2 negative 3 positive 4 veryP
label define Lagree4 1 StrongA 2 Agree 3 Disagree 4 StrongD
label define Lagree5 1 StrongA 2 Agree 3 Neutral 4 Disagree 5 StrongD

label values female Lyesno
label values black Lyesno
label values anykids Lyesno
describe female black anykids

tabulate anykids

label define degree 0 "no_hs" 1 "hs" 2 "jun_col" 3 "bachelor" 4 "graduate"
label values degree degree
tabulate degree

//  macros

use binlfp4, clear
local myoptions ", cell miss nolabel chi2 nokey"
tab lfp wc $myoptions

local myoptions ", cell miss nolabel chi2 nokey"
tab lfp wc `myoptions'

local demogvars "age white female"
local edvars "highsch college graddeg"
di ". regress y `demogvars' `edvars'"
di ". regress y `demogvars' `edvars' x1 x2 x3"

local vars age age squared income education female occupation dadeduc dadocc ///
   momeduc momocc
di "`vars'"

local wclabel : variable label wc
display "`wclabel'"

//  graphics

use lfpgraph4, clear
codebook income agecat1pr1 agecat2pr1 agecat3pr1, compact

list income agecat1pr1 agecat2pr1 agecat3pr1

graph twoway scatter agecat1pr1 agecat2pr1 agecat3pr1 income, ///
    ytitle(Probability)
graph export rm3ch2-scatter.emf, replace

graph twoway (connected agecat1pr1 income) ///
             (scatter agecat2pr1 agecat3pr1 income), ytitle(Probability)
graph export rm3ch2-connected.emf, replace

graph twoway connected agecat1pr1 agecat2pr1 agecat3pr1 income, ///
   title("Predicted Probability of Female LFP") ///
   subtitle("(as predicted by logit model)") ///
   ytitle("Probability") xtitle("Family income, excluding wife's") ///
   caption("Data from 1976 PSID compiled by T Mroz")
graph export rm3ch2-titles.emf, replace

graph twoway connected agecat1pr1 agecat2pr1 agecat3pr1 income, ///
   title("Predicted Probability of Female LFP") ///
   subtitle("(as predicted by logit model)") ///
   ytitle("Probability") xtitle("Family income, excluding wife's") ///
   caption("Data from 1976 PSID compiled by T Mroz") ///
   xlabel(10 20 30 40 50 60 70 80 90)
graph export rm3ch2-axes.emf, replace

graph twoway connected agecat1pr1 agecat2pr1 agecat3pr1 income, ///
   title("Predicted Probability of Female LFP") ///
   subtitle("(as predicted by logit model)") ///
   ytitle("Probability") xtitle("Family income, excluding wife's") ///
   caption("Data from 1976 PSID compiled by T Mroz") ///
   xlabel(10(10)90) name(graph1, replace)
graph export rm3ch2-names.emf, replace

* combining graphs

use gssclass4, replace
ologit class i.female i.white i.year i.educ c.age##c.age income, nolog

mgen, at(income=(0(25)250)) stub(CL_) atmeans
label var CL_pr1 "Lower"
label var CL_pr2 "Working"
label var CL_pr3 "Middle"
label var CL_pr4 "Upper"
label var CL_Cpr1 "Lower"
label var CL_Cpr2 "Lower/Working"
label var CL_Cpr3 "Lower/Working/Middle"

graph twoway connected CL_pr1 CL_pr2 CL_pr3 CL_pr4 CL_income, ///
     title("Panel A: Predicted Probabilities") ///
     xtitle("Household income (2012 dollars)") ///
     xlabel(0(50)250) ylabel(0(.25)1, grid gmin gmax) ///
     xline(68.1, lpattern(dash)) ytitle("") name(panelA, replace)

graph twoway connected CL_Cpr1 CL_Cpr2 CL_Cpr3 CL_income, ///
    title("Panel B: Cumulative Probabilities") ///
    xtitle("Household income (2012 dollars)") ///
    xlabel(0(50)250) ylabel(0(.25)1, grid gmin gmax) ///
    xline(68.2, lpattern(dash)) ytitle("") name(panelB, replace)

graph combine panelA panelB, xsize(8) ysize(4) ///
    caption("Example of combining horizontally.")
graph export rm3ch2-combine-horiz.emf, replace

graph combine panelA panelB, col(1) xsize(4) ysize(6) ///
    caption("Example of combining vertically.")
graph export rm3ch2-combine-vert.emf, replace

log close
exit
