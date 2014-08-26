capture log close
log using rm3ch6-nominal, replace text

//  scott long & jeremy freese (jslong@indiana.edu & jfreese@northwestern.edu)
//  regression models for categorical outcomes using stata, 3rd
//  chapter 6: nominal outcomes | 2014-02-20

version 13.1
clear all
macro drop _all
set linesize 80
set scheme s2manual
program drop _all

*? drop this?
//  intro to multinomial logit model

use partyid4, clear
tab party3
tab dem_rep, miss
logit dem_rep income, nolog

tab dem_ind, miss
logit dem_ind income, nolog

tab rep_ind, miss
logit rep_ind income, nolog

* corresponding multinomial logit

mlogit party3 income, nolog base(2)

* effect of different category values for outcome (referred to in text)

gen party999 = party3
recode party999 1=-3 2=0 3=999
mlogit party3 income, nolog b(3)
estimates store mnlmparty3
mlogit party999 income, nolog b(999)
estimates store mnlmparty999
estimates table mnlmparty3 mnlmparty999

//  party example with 5 categories

sum age income black female
tabulate party, miss
tabulate educ, miss

//  #2 MNLM: Multinomial logit model with base category StrRep

mlogit party age income i.black i.female i.educ, base(5) vsquish
listcoef female, help
listcoef female, pvalue(.15)
listcoef income age, lt adjacent

//  perfect prediction (described in text)

use gssclass4, clear
gen college = (educ == 3 & class == 4)
label var college "Has college degree?"
label def college 0 no 1 yes
label val college college
tab class college
ologit class i.female i.white i.year i.college c.age##c.age income, nolog
mlogit class i.female i.white i.year i.college c.age##c.age income, nolog

//  lrtest

use partyid4, clear
qui mlogit party age income i.black i.female i.educ, base(5) nolog
estimates store full_model
qui mlogit party income i.black i.female i.educ, base(5) nolog
estimates store drop_age
lrtest full_model drop_age

qui mlogit party age income i.black i.female i.educ, base(5) nolog
mlogtest, lr

//  wald test

qui mlogit party age income i.black i.female i.educ, base(5) nolog
test 1.female
mlogtest, wald

//  testing multiple coefficients

qui mlogit party age income i.black i.female i.educ, base(5) nolog
estimates store full_model
qui mlogit party i.black i.female i.educ, base(5) nolog
estimates store drop_ageinc
lrtest full_model drop_ageinc

qui mlogit party age income i.black i.female i.educ, base(5) nolog
mlogtest , lr set(age&inc=age income \ educ=2.educ 3.educ)

//  combining outcomes

qui mlogit party age income i.black i.female i.educ, base(5) nolog
mlogtest , combine
test [StrDem]
test [StrDem=Dem]

qui mlogit party age income i.black i.female i.educ, base(5) nolog
mlogtest , lrcombine

* constraints for combining outcomes

qui mlogit party age income i.black i.female i.educ, base(5) nolog
estimates store full_model
constraint define 999 [StrDem]

qui mlogit party age income i.black i.female i.educ, ///
    constraint(999) base(5) nolog vsquish
estimates store constraint999

lrtest full_model constraint999

//  IIA tests

qui mlogit party age income i.black i.female i.educ, base(5) vsquish
mlogtest, hausman

set seed 124386
mlogtest, smhsiao

set seed 254331
mlogtest, smhsiao

set seed 86721129
mlogtest, smhsiao

set seed 124386
mlogtest, smhsiao

//  predict to compare -mlogit- and -ologit-

qui mlogit party age income i.black i.female i.educ, base(5)
predict mnlmSD mnlmD mnlmI mnlmR mnlmSR
codebook mnlmSD mnlmD mnlmI mnlmR mnlmSR, compact

qui ologit party age income i.black i.female i.educ
predict olmSD olmD olmI olmR olmSR
codebook olm*, compact

label var olmI "ologit"
label var mnlmI "mlogit"
dotplot olmI mnlmI,  ylabel(0(.1).3, grid) ytitle(Pr(Independent))
graph export rm3ch6-predict-dotplot-olmVmnlm.emf, replace

pwcorr olmI mnlmI

* scatterplot
graph twoway scatter olmI mnlmI, ///
    xtitle("mlogit: Pr(Independent)") ytitle("ologit: Pr(Independent)") ///
    xlabel(0(.1).3, grid gmin gmax)   ylabel(0(.1).3, grid gmin gmax) ///
    sort aspect(1)
graph export rm3ch6-predict-scatter-olmVmnlm.emf, replace

//  tables of predictions using global means

qui mlogit party age income i.black i.female i.educ, base(5)
mtable, at(black=0 female=1) at(black=1 female=1) /// women
        at(black=0 female=0) at(black=1 female=0) /// men
        atmeans noci norownumbers

mtable, at(black=(0 1) female=(0 1)) atmeans noci norownumbers

mchange black, at(female=1) atmeans brief ///
    statistics(start end change pvalue) title(Effect of race for women)
mchange black, at(female=0) atmeans brief ///
    statistics(start end change pvalue) title(Effect of race for men)

//  second differences

qui mlogit party age income i.black i.female i.educ, base(5)
estimates store mymodel

margins, predict(outcome(1)) post atmeans noatlegend ///
    at(black=0 female=1) at(black=1 female=1) ///
    at(black=0 female=0) at(black=1 female=0)
mlistat
*  lincom _b[2._at] - _b[1._at] // 1st difference if female
lincom _b[2._at] - _b[1._at]
*  lincom _b[4._at] - _b[3._at] // 1st difference if male
lincom _b[4._at] - _b[3._at]
* second difference
lincom (_b[2._at] - _b[1._at]) - (_b[4._at] - _b[3._at])
est restore mymodel

forvalues iout = 1/5 {
    margins, predict(outcome(`iout')) post atmeans ///
        at(black=0 female=1) at(black=1 female=1) ///
        at(black=0 female=0) at(black=1 female=0)
    lincom (_b[2._at] - _b[1._at]) - (_b[4._at] - _b[3._at])
    est restore mymodel
}

mlincom, clear
forvalues iout = 1/5 {
    qui {
        margins, predict(outcome(`iout')) post atmeans ///
            at(black=0 female=1) at(black=1 female=1) ///
            at(black=0 female=0) at(black=1 female=0)
        mlincom (2-1)-(4-3), save label(Outcome `iout')
        est restore mymodel
    }
}

mlincom

//  tables of predictions with local means

use partyid4, clear
qui mlogit party age income i.black i.female i.educ, base(5)
estimates store mymodel
qui mtable if black==0 & female==1, atmeans noci rowname(White Women) clear
qui mtable if black==1 & female==1, atmeans noci rowname(Black Women) below
qui mtable if black==0 & female==0, atmeans noci rowname(White Men) below
    mtable if black==1 & female==0, atmeans noci rowname(Black Men) below

*matrix blkfem_local = _mtab_display
*matrix blkfem_localat = _mtab_atdisplay

* global means
qui mtable, at(black=0 female=1) atmeans noci rowname(White Women) clear
qui mtable, at(black=1 female=1) atmeans noci rowname(Black Women) below
qui mtable, at(black=0 female=0) atmeans noci rowname(White Men) below
    mtable, at(black=1 female=0) atmeans noci rowname(Black Men) below

*matrix blkfem_global = _mtab_display
*matrix blkfem_globalat = _mtab_atdisplay
*matmath dif = blkfem_global - blkfem_local
*matlist dif, format(%8.2f) title(Global minus local predictions)

mtable, at(age=47.1 income=35.2 black=0 female=1 ///
    1b.educ=.16 2.educ=.62 3.educ=.22)

* local means -- to save atspecs to local macros

qui mtable if black==0 & female==1, atmeans noci rowname(White Women) clear
    local atWF "`r(atspec)'"
qui mtable if black==1 & female==1, atmeans noci rowname(Black Women) below
    local atBF "`r(atspec)'"
qui mtable if black==0 & female==0, atmeans noci rowname(White Men) below
    local atWM "`r(atspec)'"
qui mtable if black==1 & female==0, atmeans noci rowname(Black Men) below
    local atBM "`r(atspec)'"

mlincom, clear
forvalues iout = 1/5 {
    quietly {
        margins, predict(outcome(`iout')) post atmeans ///
            at(`atWF') at(`atBF') at(`atWM') at(`atBM')
        mlincom (2-1)-(4-3), save label(Outcome `iout')
        est restore mymodel
    } // end of quietly
} // end of forvalues
mlincom

**  HARD WAY without using r(atspec)
**
**        capture matrix drop changes
**        capture matrix drop probs
**
**        * within group means
**        foreach v in age income highschool college {
**            qui sum `v' if female==1 & black==1 // fem black
**            local Mfb`v' = r(mean)
**            qui sum `v' if female==0 & black==1 // men black
**            local Mmb`v' = r(mean)
**            qui sum `v' if female==1 & black==0 // fem white
**            local Mfw`v' = r(mean)
**            qui sum `v' if female==0 & black==0 // men white
**            local Mmw`v' = r(mean)
**        }
**
**        capture drop changes
**        forvalues iout = 1/5 {
**            qui margins, predict(outcome(`iout')) ///
**                at(black=0 female=0 age=`Mmwage' income=`Mmwincome' ///
**                    highschool=`Mmwhighschool' college=`Mmwcollege') ///
**                at(black=0 female=1 age=`Mfwage' income=`Mfwincome' ///
**                    highschool=`Mfwhighschool' college=`Mfwcollege') ///
**                at(black=1 female=0 age=`Mmbage' income=`Mmbincome' ///
**                    highschool=`Mmbhighschool' college=`Mmbcollege') ///
**                at(black=1 female=1 age=`Mfbage' income=`Mfbincome' ///
**                    highschool=`Mfbhighschool' college=`Mfbcollege') ///
**                post
**            qui mlincom, clear
**            qui mlincom 4-2, save label(BKwo-WTwo)
**            qui mlincom 3-1, save label(BKmn-WTmn) title(`mdl' Outcome=`iout')
**            qui mlincom (4-2)-(3-1), save label(SecDif)
**            matrix temp = _mlincom[1...,1] \ _mlincom[1...,2]
**            matrix rownames temp = ///
**                BKwo-WTwo BKmn-WTmn 2nd pBKwo-WTwo pBKmn-WTmn 2ndp
**            matrix colnames temp = Cat`iout'
**            matrix changes = nullmat(changes) , temp
**            qui est restore mymodel
**        }
**        matrix changes = changes[1,1...] \ changes[4,1...] \ ///
**                         changes[2,1...] \ changes[5,1...] \ ///
**                         changes[3,1...] \ changes[6,1...]
**
**        * black women
**        qui mtable, at(black=1 female=1 age=`Mfbage' income=`Mfbincome' ///
**                   highschool=`Mfbhighschool' college=`Mfbcollege') noci ///
**                   rowname(BlkWomen) clear
**        * white women
**        qui mtable, at(black=0 female=1 age=`Mfwage' income=`Mfwincome' ///
**                   highschool=`Mfwhighschool' college=`Mfwcollege') noci ///
**                   rowname(WhtWomen) below
**        * black men
**        qui mtable, at(black=1 female=0 age=`Mmbage' income=`Mmbincome' ///
**                   highschool=`Mmbhighschool' college=`Mmbcollege') noci ///
**                   rowname(BlkMen) below
**        * white men
**        mtable, at(black=0 female=0 age=`Mmwage' income=`Mmwincome' ///
**               highschool=`Mmwhighschool' college=`Mmwcollege') noci ///
**                   rowname(WhtMen) below
**
**        matlist changes, format(%10.3f) title(Table 8 MNLM with group means)

//  plotting probabilities by age and income: mnlm

use partyid4, clear
qui mlogit party age income i.black i.female i.educ, base(5) vsquish
mgen, atmeans at(age=(20(5)85)) stub(mnlmA) replace
mgen, atmeans at(income=(0(10)100)) stub(mnlmI) replace

label var mnlmApr1 "Strong Dem"
label var mnlmApr2 "Democrat"
label var mnlmApr3 "Independent"
label var mnlmApr4 "Republican"
label var mnlmApr5 "Strong Rep"
graph twoway connected ///
  mnlmApr1 mnlmApr2 mnlmApr3 mnlmApr4 mnlmApr5 mnlmAage, ///
  title("Multinomial logit model", pos(11) size(medium)) ///
  ytitle(Probability of party affiliation) ylab(0(.1).4, grid gmax gmin) ///
  msym(O Oh dh sh s) msiz(*1.5 *1.5 *1.7 *1.8 *1.8) mcol(gs1 gs5 gs8 gs5 gs1) ///
  lpat(solid dash shortdash dash solid) lcol(gs1 gs5 gs8 gs5 gs1) ///
  legend(rows(2))
graph export rm3ch6-mnlm-prob-age.emf,  replace

label var mnlmIpr1 "Strong Dem"
label var mnlmIpr2 "Democrat"
label var mnlmIpr3 "Independent"
label var mnlmIpr4 "Republican"
label var mnlmIpr5 "Strong Rep"
graph twoway connected ///
  mnlmIpr1 mnlmIpr2 mnlmIpr3 mnlmIpr4 mnlmIpr5 mnlmIincome, ///
  title("Multinomial logit model", pos(11) size(medium)) ///
  ytitle(Probability of party affiliation) ylab(0(.1).4, grid gmax gmin) ///
  msym(O Oh dh sh s) msiz(*1.5 *1.5 *1.7 *1.8 *1.8) mcol(gs1 gs5 gs8 gs5 gs1) ///
  lpat(solid dash shortdash dash solid) lcol(gs1 gs5 gs8 gs5 gs1) ///
  legend(rows(2))
graph export rm3ch6-mnlm-prob-income.emf, replace

//  plotting probabilities by age and income: olm

qui ologit party age income i.black i.female i.educ, vsquish
mgen, atmeans at(age=(20(5)85)) stub(olmA) replace
label var olmApr1 "Strong Dem"
label var olmApr2 "Democrat"
label var olmApr3 "Independent"
label var olmApr4 "Republican"
label var olmApr5 "Strong Rep"

mgen, atmeans at(income=(0(10)100)) stub(olmI) replace
label var olmIpr1 "Strong Dem"
label var olmIpr2 "Democrat"
label var olmIpr3 "Independent"
label var olmIpr4 "Republican"
label var olmIpr5 "Strong Rep"

graph twoway connected ///
    olmApr1 olmApr2 olmApr3 olmApr4 olmApr5 olmAage, ///
    title("Ordered logit model", pos(11) size(medium)) ///
    ytitle(Probability of party affiliation) ylab(0(.1).4, grid gmax gmin) ///
    msym(O Oh dh sh s) msiz(*1.5 *1.5 *1.7 *1.8 *1.8) mcol(gs1 gs5 gs8 gs5 gs1) ///
    lpat(solid dash shortdash dash solid) lcol(gs1 gs5 gs8 gs5 gs1) ///
    legend(rows(2))
graph export rm3ch6-olm-prob-age.emf,  replace

graph twoway connected ///
    olmIpr1 olmIpr2 olmIpr3 olmIpr4 olmIpr5 olmIincome, ///
    title("Ordered logit model", pos(11) size(medium)) ///
    ytitle(Probability of party affiliation) ylab(0(.1).4, grid gmax gmin) ///
    msym(O Oh dh sh s) msiz(*1.5 *1.5 *1.7 *1.8 *1.8) mcol(gs1 gs5 gs8 gs5 gs1) ///
    lpat(solid dash shortdash dash solid) lcol(gs1 gs5 gs8 gs5 gs1) ///
    legend(rows(2))
graph export rm3ch6-olm-prob-income.emf, replace

//  plotting probabilities by age and income: olm vs mnlm

label var olmApr1 "OLM: Strong Dem"
label var olmApr5 "OLM: Strong Rep"
label var mnlmApr1 "MNLM: Strong Dem"
label var mnlmApr5 "MNLM: Strong Rep"
graph twoway connected ///
    mnlmApr1 mnlmApr5 olmApr1 olmApr5 mnlmAage, ///
    title("MNLM cmpared to OLM", pos(11) size(medium)) ///
    ytitle(Probability of party affiliation) ylab(0(.1).4, grid gmax gmin) ///
    msym(O s Oh sh) msiz(*1.5 *1.8 *1.5 *1.8) mcol(gs4 gs4 gs1 gs1) ///
    lpat(solid soldi shortdash shortdash) lcol(gs4 gs4 gs1 gs1) ///
    legend(rows(2))
graph export rm3ch6-mnlmVolm-prob-age.emf,  replace

//  average marginal effects

use partyid4, clear
qui mlogit party age income i.black i.female i.educ, base(5) vsquish

mchange, amount(sd) brief

sum age income
mchangeplot age income, symbols(D d I r R) min(-.05) max(.05) gap(.02) ///
    aspect(.2) xtitle(Average Discrete Change) sig(.05)
graph export rm3ch6-marginal-ageincome.emf, replace

* black female

mchangeplot black female, symbols(D d I r R) min(-.3) max(.3) gap(.1) ///
    aspect(.2) xtitle(Average Discrete Change) sig(.05)
graph export rm3ch6-discrete-blackfemale.emf, replace

* educ

mchangeplot educ, symbols(D d I r R) min(-.1) max(.1) gap(.02) ///
    aspect(.3) xtitle(Average Discrete Change) sig(.05) leftmargin(5)
graph export rm3ch6-discrete-educ.emf, replace

//  distribution of discrete changes
//  ! this is commented out since it takes a long time to run

/*
use partyid4, clear
mlogit party age income i.black i.female i.educ, base(1)
estimates store mymodel

* distribution of DC for income
gen incomedc = .
label var incomedc ///
    "Effect of a one standard deviation change in income on Pr(Dem)"

sum income
local sd2 = r(sd)/2
local nobs = _N
forvalues i = 1/`nobs' {
    qui {
        margins in `i', nose predict(outcome(2)) /// Dem
            at(income=gen(income-`sd2')) at(income=gen(income+`sd2'))
        local prstart = el(r(b),1,1)
        local prend = el(r(b),1,2)
        local dc = `prend' - `prstart'
        replace incomedc = `dc' in `i'
    }
}

* to test the computations
sum incomedc
mchange income, amount(sd)

histogram incomedc, xlabel(-.05(.01).05) fraction ///
    width(.005) color(gs10) fcolor(gs12) ylab(0(.1).2) xline(0, lpat(dash))
graph export rm3ch6-dcdist-income.emf, replace

* distribution of DC for age
gen agedc = .
label var agedc "Effect of a one standard deviation change in age on Pr(StrRep)"

sum age
local sd2 = r(sd)/2
local nobs = _N
forvalues i = 1/`nobs' {
    qui {
        margins in `i', nose predict(outcome(5)) /// StrRep
            at(age=gen(age-`sd2')) at(age=gen(age+`sd2'))
        local prstart = el(r(b),1,1)
        local prend = el(r(b),1,2)
        local dc = `prend' - `prstart'
        replace agedc = `dc' in `i'
    }
}

* test comutations
sum agedc
mchange age, amount(sd)

histogram agedc, xlabel(0(.02).1) fraction width(.005) ///
    color(gs10) fcolor(gs12) ylab(0(.1).2)
graph export rm3ch6-dcdist-age.emf, replace
*/

//  odds ratio plots

use partyid4, clear
qui mlogit party age income i.black i.female i.educ, base(5) vsquish
listcoef black, help
listcoef, help

orplot, symbols(D d I r R) base(3) linepvalue(1) leftmargin(2)
graph export rm3ch6-orplot-allnolines3.emf, replace

orplot, symbols(D d I r R) base(3) min(-2.5) max(2) gap(.5) leftmargin(2) ///
    linepvalue(.10)
graph export rm3ch6-orplot-all3rng.emf, replace

* age inc

orplot age income, symbols(D d I r R) base(3) ///
    ormin(.5) ormax(2) ntics(5) aspect(.3) ///
    offsetlist(2 -2 0 2 -2   2 -2 0 2 -2)
graph export rm3ch6-orplot-ageinc.emf, replace

* arrange offsets

orplot age income, symbols(D d I r R) base(3) ///
    ormin(.5) ormax(2) ntics(5) aspect(.3) ///
    offsetlist(2 -2 0 2 -2   2 -2 2 2 -2)
graph export rm3ch6-orplot-ageinc-offset.emf, replace

* add AME

mchange age income, amount(sd)
orplot age income, meffect amount(sd sd) msizefactor(2) ///
    subtitle(Average discrete of SD, position(11)) ///
    symbols(D d I r R) base(3) ///
    ormin(.5) ormax(2) ntics(5) aspect(.3) ///
    offsetlist(2 -2 0 2 -2   2 -2 2 2 -2)
graph export rm3ch6-orplot-me-ageinc.emf, replace

* educ and race

mchange educ black
orplot black educ, meffect ///
    symbols(D d I r R) base(3) ormin(.1) ormax(10) ntics(5) aspect(.5) ///
    offsetlist(2 -2 0 3 -3   2 -2 0 2 -2   2 -2 0 2 -2) leftmargin(2)
graph export rm3ch6-orplot-educblack-me.emf, replace

//  stereotype logit model

* mlogit used for comparison with slogit

use partyid4, clear
mlogit party age income i.black i.female i.educ, base(5) nolog vsquish
predict mnlm_1 mnlm_2 mnlm_3 mnlm_4 mnlm_5
mchange age income, amount(sd)
mchangeplot age income, min(-.06) max(.06) gap(.02) symbols(D d I r R) ///
    aspect(.3) title("Multinomial logit model", position(11))
graph export rm3ch6-slogit-mlogit-change.emf, replace

* slm 1

slogit party age income i.black i.female i.educ, vsquish
predict slm1_1 slm1_2 slm1_3 slm1_4 slm1_5
mchange age income, amount(sd)
mchangeplot age income, min(-.06) max(.06) gap(.02) symbols(D d I r R) ///
    aspect(.3) title("Stereotype logit model with one dimension", position(11))
graph export rm3ch6-slogit1-change.emf, replace
list party slm1_* in 1/4

* slogit compared to mlogit

pwcorr slm1_1 mnlm_1
pwcorr slm1_2 mnlm_2
pwcorr slm1_3 mnlm_3
pwcorr slm1_4 mnlm_4
pwcorr slm1_5 mnlm_5

* odds ratios

listcoef, expand
listcoef income age, expand

* slm 2

slogit party age income i.black i.female i.educ, dim(2)
mchange age income, amount(sd)
mchangeplot age income, min(-.06) max(.06) gap(.02) symbols(D d I r R) ///
    aspect(.3) title("Stereotype logit model with two dimensions", position(11))
graph export rm3ch6-slogit2-change.emf, replace

slogit party age income i.black i.female i.educ, dim(2)
predict slm2_1 slm2_2 slm2_3 slm2_4 slm2_5

* slm2 compared to mlogit

pwcorr slm2_1 mnlm_1
pwcorr slm2_2 mnlm_2
pwcorr slm2_3 mnlm_3
pwcorr slm2_4 mnlm_4
pwcorr slm2_5 mnlm_5

//  conditional logit model

* reshaping the data

use travel4.dta, clear
list id mode choice time in 1/6, nolabel sepby(id)

use travel4case.dta, clear
list id choice time1 time2 time3 in 1/2, nolabel

reshape long time, i(id) j(mode)
list id mode choice time in 1/6, nolabel sepby(id)

replace choice = (choice == mode) if choice < . & mode < .
list id mode choice time in 1/6, nolabel sepby(id)

* fitting the model

use travel4.dta, clear
asclogit choice time, alt(mode) case(id) nolog

* interpreting results

asclogit choice time, alt(mode) case(id) nolog or
estat mfx
estat mfx, at(Car: time=420 Bus: time=660 Train: time=600)

* case-specific variable

use travel4.dta, clear
asclogit choice time, alt(mode) case(id) casevars(hinc psize) or nolog

* estimate clogit model as multinomial logit

use partyid4.dta, clear
mlogit party age income i.black i.female i.educ, base(1)

* re-arrange partyid data in row-per-alternative format
use partyid4.dta, clear
gen hsonly = (educ==2) if educ < .
gen college = (educ==3) if educ < .
gen _tmp1 = .
gen _tmp2 = .
gen _tmp3 = .
gen _tmp4 = .
gen _tmp5 = .
reshape long _tmp, i(caseid) j(partyalt)
gen choice = (party==partyalt) if partyalt < .

list caseid choice party partyalt age female hsonly college in 1/10, ///
	nolabel sepby(caseid)

asclogit choice, case(caseid) alternatives(partyalt) ///
	casevars(age income black female hsonly college) nolog

//  multinomial probit with IIA

* compare mprobit coefficients to probit coefficients

use binlfp4, clear
probit lfp k5 k618 age i.wc i.hc lwg inc, nolog

mprobit lfp k5 k618 age i.wc i.hc lwg inc, nolog baseoutcome(0)
mprobit lfp k5 k618 age i.wc i.hc lwg inc, nolog base(0) probitparam

* compare mprobit and probit predictions

use binlfp4, clear
probit lfp k5 k618 age i.wc i.hc lwg inc, nolog
predict bpm_1

mprobit lfp k5 k618 age i.wc i.hc lwg inc, nolog base(0)
predict mnpm_0 mnpm_1

mprobit lfp k5 k618 age i.wc i.hc lwg inc, nolog base(0) probitparam
predict mnpm_0p mnpm_1p

pwcorr bpm_1 mnpm_0 mnpm_1

* compare mprobit and mlogit

use partyid4, clear
mprobit party age income i.black i.female i.educ, base(1)
predict mnpm_1 mnpm_2 mnpm_3 mnpm_4 mnpm_5

mlogit party age income i.black i.female i.educ, base(1)
predict mnlm_1 mnlm_2 mnlm_3 mnlm_4 mnlm_5

correlate mnpm_1 mnlm_1 mnpm_2 mnlm_2

//  alternative specific multinomial probit: MNPM withotu IIA

use travel4.dta, clear
asclogit choice time, case(id) alternatives(mode) casevars(hinc psize) nolog

asmprobit choice time, case(id) alternatives(mode) casevars(hinc psize) ///
	correlation(independent) stddev(homoskedastic) nolog
estat covariance

asmprobit choice time, case(id) alternatives(mode) casevars(hinc psize) nolog
estat covariance
estat correlation

// rank-ordered regression models

use wlsrank4, clear
rologit rank high low ib4.jobchar##(c.score female), ///
	group(id) reverse noomitted nolog

log close
exit
