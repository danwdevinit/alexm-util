capture log close
log using rm3ch5-ordinal, replace text

//  scott long & jeremy freese (jslong@indiana.edu & jfreese@northwestern.edu)
//  regression models for categorical outcomes using stata, 3rd
//  chapter 5: ordinal outcomes | 2014-02-20

version 13.1
clear all
macro drop _all
set linesize 80
set scheme s2manual
program drop _all

//  compare logit to ologit

use binlfp4, clear
logit lfp c.k5 c.k618 i.agecat i.wc i.hc c.lwg c.inc, nolog
estimates store logit
ologit lfp c.k5 c.k618 i.agecat i.wc i.hc c.lwg c.inc, nolog
estimates store ologit
estimates table logit ologit, b(%9.3f) t varlabel varwidth(30) equations(1:1)

// estimation of ordered regression model

use gssclass4, clear
ologit class i.female i.white i.year i.educ age income if year == 1, nolog

* ordered models do not require positive integers
recode class (1=-1.23) (2=0) (3=2.67) (4=35), gen(classdecimal)
ologit classdecimal female white i.year age income i.educ

* basic features of data
codebook class female white year ed age income, compact
tab class
summarize i.female i.white i.year i.educ age income, sep(15)

//  compare -ologit- and -oprobit-

ologit class i.female i.white i.year i.educ c.age##c.age income, nolog
estimates store ologit
oprobit class i.female i.white i.year i.educ c.age##c.age income, nolog
estimates store oprobit
estimates table ologit oprobit, b(%9.3f) t varlabel varwidth(30)

//  predicting perfectly with artifical data

capture drop college
gen college = (educ == 3 & class == 4)
label var college "Has college degree?"
label def college 0 no 1 yes
label val college college
tab class college
ologit class i.female i.white i.year i.college c.age##c.age income, nolog

//  nonlinearity: age squared improves fit

ologit class i.female i.white i.year i.educ age income, nolog
qui fitstat, ic save
ologit class i.female i.white i.year i.educ c.age##c.age income, nolog
fitstat, ic diff

//  wald tests

use gssclass4, clear
ologit class i.female i.white i.year i.educ c.age##c.age income, ///
    nolog sformat(%8.3f)
test 1.white
display "z*z=" 3.277*3.277

//  LR test

ologit class i.female i.white i.year i.educ c.age##c.age income, nolog
estimates store fullmodel
ologit class i.female i.year i.educ c.age##c.age income, nolog
estimates store dropwhite
lrtest fullmodel dropwhite

//  test multiple coefficients

ologit class i.female i.white i.year i.educ c.age##c.age income, nolog
test 1.white 1.female age age#age
testparm i.educ

ologit class i.female i.white i.year i.educ c.age##c.age income, nolog
estimates store fullmodel
ologit class i.year i.educ income, nolog
estimates store dropdemog
lrtest fullmodel dropdemog

//  scalar measures of fit

ologit class i.female i.white i.year i.educ age income, nolog
quietly fitstat, save
ologit class i.female i.white i.year i.educ c.age##c.age income, nolog
fitstat, diff

//  converting to a different parameterization (param)

ologit class i.female i.white i.year i.educ c.age##c.age income, nolog
lincom 0 - _b[/cut1] // intercept
lincom _b[/cut2] - _b[/cut1] // cutpoint 2
lincom _b[/cut3] - _b[/cut1] // cutpoint 3

// testing the parallel regression assumption

use gssclass4, clear
ologit class i.female i.white i.year i.educ c.age##c.age income, nolog
oparallel, ic
brant, detail

//  partial change in y*

ologit class i.female i.white i.year i.educ c.age##c.age income, nolog
listcoef, std help

// odds ratios using listcoef

ologit class i.female i.white i.year i.educ c.age##c.age income, nolog
listcoef white year income age, help
listcoef white year income, percent
listcoef year, reverse

//  in sample predicted probabilities with -predict-

use gssclass4, replace
ologit class i.female i.white i.year i.educ c.age##c.age income, nolog
predict prlower prworking prmiddle prupper
label var prlower "Pr(Lower)"
label var prworking "Pr(Working)"
label var prmiddle "Pr(Middle)"
label var prupper "Pr(Upper)"
dotplot prlower prworking prmiddle prupper, ///
    ylabel(0(.25)1, grid gmin gmax) ytitle("Probability")
graph export rm3ch5-predict-dotplot.emf, replace

//  individual predicted probabilities

* -  a 60-year old white woman with a college degree
*    and a household income of $150,000/year.

* -     a 25-year old nonwhite man without a high school diploma
*    and a household income of $30,000/year.

use gssclass4, replace
ologit class i.female i.white i.year i.educ c.age##c.age income, nolog

margins, at(female=0 white=0 year=3 educ=1 age=25 income=30)
mtable, at(female=0 white=0 year=3 educ=1 age=25 income=30) ci

mtable, atright norownum width(7) ///
    at(female=0 white=0 year=3 ed=1 age=25 income=30) ///
    at(female=1 white=1 year=3 ed=3 age=60 income=150)

mtable, atright norownum width(7) ///
    at(female=0 white=0 year=3 ed=1 age=25 income=30) ///
    at(female=1 white=1 year=3 ed=3 age=60 income=150)

qui mtable, at(female=0 white=0 year=3 ed=1 age=25 income=30)
mtable, at(female=1 white=1 year=3 ed=3 age=60 income=150) below

use gssclass4, replace
ologit class i.female i.white i.year i.educ c.age##c.age income, nolog
estimates store olm

*
mlincom, clear
forvalues iout = 1/4 { // start loop
    quietly {
        mtable, out(`iout') post ///
            at(female=0 white=0 year=3 ed=1 age=25 income=30) ///
            at(female=1 white=1 year=3 ed=3 age=60 income=150)
        mlincom 1 - 2, stats(est pvalue) rowname(outcome `iout') add
        estimates restore olm
    }
} // end loop
mlincom

//  tables of predicted probabilities

mtable, at(year=(1 2 3)) atmeans norownum
mtable, at(year=(1 2 3)) atmeans stat(ci)
mtable, at(year=(1 2 3) white=(0 1)) atmeans norownum
mtable, atmeans norownum ///
    at(year=1 white=(0 1)) ///
    at(year=2 white=(0 1)) ///
    at(year=3 white=(0 1))
mtable, over(year white) atmeans
mtable, over(year white) atmeans atright

//  graphing predicted probabilities

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

* probabilities
graph twoway connected CL_pr1 CL_pr2 CL_pr3 CL_pr4 CL_income, ///
     title("Panel A: Predicted Probabilities") ///
     xtitle("Household income (2012 dollars)") ///
     xlabel(0(50)250) ylabel(0(.25)1, grid gmin gmax) ///
     xline(68.1, lpattern(dash)) ytitle("") name(tmpprob, replace)
graph export rm3ch5-mgen-prob.emf, replace

* cumulative probabilities
graph twoway connected CL_Cpr1 CL_Cpr2 CL_Cpr3 CL_income, ///
    title("Panel B: Cumulative Probabilities") ///
    xtitle("Household income (2012 dollars)") ///
    xlabel(0(50)250) ylabel(0(.25)1, grid gmin gmax) ///
    xline(68.2, lpattern(dash)) ytitle("") name(tmpcprob, replace)
graph export rm3ch5-mgen-cumprob.emf, replace

* combined graphs
graph combine tmpprob tmpcprob, col(1) iscale(*.9) imargin(small) ///
    ysize(4.6) xsize(3.287) ///
    caption("Other variables held at their means")
graph export rm3ch5-mgen-combined.emf, replace

* shaded graph of cumulative probabilities
capture drop one
gen one = 1
label variable one "Upper"
label variable CL_Cpr3 "Middle"
label variable CL_Cpr2 "Working"
label variable CL_Cpr1 "Lower"
graph twoway ///
    (area one CL_income, fcolor(gs15)) ///
    (area CL_Cpr3 CL_income, fcolor(gs11)) ///
    (area CL_Cpr2 CL_income, fcolor(gs7)) ///
    (area CL_Cpr1 CL_income, fcolor(gs3)) ///
    , ///
    xtitle("Household income (2012 dollars)") ytitle("Probability") ///
    xlabel(0(50)250) ylabel(0(.25)1, grid gmin gmax)
graph export rm3ch5-mgen-rarea.emf, replace

//  changes in predicted probabilities with prchange

use gssclass4, clear
ologit class i.female i.white i.year i.educ c.age##c.age income, nolog

//  marginal change

mchange income, atmeans amount(marginal) dec(4)
mchange income, amount(marginal) dec(4)

//  discrete change

sum income
mchange income, atmeans brief
mchange income, atmeans delta(25) amount(delta) brief
mchange income, atmeans amount(range) brief
mchange income, atmeans amount(range) trim(5) brief

* age and income

mchange age income, amount(sd) brief
mchangeplot age income, symbols(L W M U) min(-.15) max(.15) gap(.05) aspect(.3) ///
  title("Social class: L=lower  W=working  M=middle  U=upper", size(medsmall))
graph export rm3ch5-mchangeplot-incage.emf, replace

* factor variables

mchange female white educ year, brief
mchangeplot year, ///
  sig(.05) leftmargin(5) ///
  symbols(L W M U) min(-.15) max(.15) gap(.05) aspect(.3) ///
  title("Social class: L=lower  W=working  M=middle  U=upper", size(medsmall))
graph export rm3ch5-mchangeplot-year.emf, replace

mchangeplot educ, ///
  leftmargin(5) symbols(L W M U) min(-.30) max(.30) gap(.1) aspect(.3) ///
  title("Social class: L=lower  W=working  M=middle  U=upper", size(medsmall))
graph export rm3ch5-mchangeplot-educ.emf, replace

* mchangeplot example using all variables

mchange, amount(sd) brief
mchangeplot, sig(.05) symbols(L W M U) leftmargin(5)
graph export rm3ch5-mchangeplot-all.emf, replace

//  gologit2

use gssclass4, clear
* gologit2 does not use factor variables
gen year1996 = (year==2) if year < .
gen year2012 = (year==3) if year < .
gen educ_hs = (educ==2) if educ < .
gen educ_col = (educ==3) if educ < .
gen agesq = age*age if age < .
gologit2 class female white year1996 year2012 educ_hs educ_col ///
    age agesq income, nolog or

mtable,  atright norownum width(7) ///
    at(female=0 white=0 year1996=0 year2012=1 educ_hs=0 educ_col=0 ///
       age=25 agesq=625 income=30) ///
    at(female=1 white=1 year1996=0 year2012=1 educ_hs=0 educ_col=1 ///
       age=60 agesq=3600 income=150)

* dc white
mchange white, amount(binary) atmeans
mchange white, amount(binary) at(agesq=2039) atmeans

summarize age
local mnagesq = r(mean)*r(mean)
mchange white, amount(binary) at(agesq=`mnagesq') atmeans

* dc year

* change from 1980 to 1996
mchange year1996, amount(binary) at(year2012=0 agesq=2039) atmeans brief
* change from 1980 to 2012
mchange year2012, amount(binary) at(year1996=0 agesq=2039) atmeans brief
* 1996 to 2012
estimates store golm
mlincom, clear
forvalues iout = 1/4 {
    mtable, atmeans post outcome(`iout') ///
        at(year1996=1 year2012=0 agesq=2039) /// 1996
        at(year1996=0 year2012=1 agesq=2039) // 2012
    mlincom 1 - 2, add rowname(outcome=`iout')
    estimates restore golm
}
mlincom

estimates store golm
mlincom, clear
forvalues iout = 1/4 {
    qui {
        mtable, atmeans post outcome(`iout') ///
            at(year1996=1 year2012=0 agesq=2039) /// 1996
            at(year1996=0 year2012=1 agesq=2039) // 2012
        mlincom 1 - 2, add rowname(outcome=`iout')
        estimates restore golm
    }
}
mlincom

* dc year for ologit
ologit class i.female i.white i.year i.educ c.age##c.age income, nolog
mchange year

//  sequential logit model

tab educ, miss
recode educ (1=0) (2 3=1), gen(gradhs)
label var gradhs "Graduate high school?"
recode educ (1=.) (2=0) (3=1), gen(gradcollege)
label var gradcollege "Graduate college?"

logit gradhs i.white i.female, or nolog
logit gradcollege i.white i.female, or nolog

seqlogit educ i.white i.female, tree(1: 2 3, 2 : 3) or nolog
estimates store seqlogit

constraint define 1 [_2_3v1]1.female=[_3v2]1.female
constraint define 2 [_2_3v1]1.white=[_3v2]1.white
seqlogit educ i.white i.female, tree(1: 2 3, 2 : 3) constraint(1 2) or nolog
estimates store contratio

lrtest contratio seqlogit

log close
exit
