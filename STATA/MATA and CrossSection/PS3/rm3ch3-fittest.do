capture log close
log using rm3ch3-fittest, replace text

//  scott long & jeremy freese (jslong@indiana.edu & jfreese@northwestern.edu)
//  regression models for categorical outcomes using stata, 3rd
//  chapter 3: estimation and testing | 2014-02-20

version 13.1
clear all
macro drop _all
program drop _all
set linesize 80
set scheme s2manual

//  ml estimation

use binlfp4, clear
logit lfp k5 k618 agecat wc hc lwg inc
logit lfp k5 k618 age wc lwg
logit lfp k5 k618 age wc lwg if hc==1
logit lfp k5 k618 age wc lwg if hc==1, level(90)

* creating indicators

drop age3039 age4049 age50plus
tabulate agecat, missing

generate age3039 = (agecat==1) if agecat < .
label var age3039 "Age 30 to 39?"
generate age4049 = (agecat==2) if agecat < .
label var age4049 "Age 40 to 49?"
generate age50plus = (agecat==3) & agecat < .
label var age50plus "Age 50 or older?"

logit lfp k5 k618 age4049 age50plus wc hc lwg inc, nolog
logit lfp k5 k618 i.agecat wc hc lwg inc, nolog
logit lfp k5 k618 ib3.agecat wc hc lwg inc, nolog
logit lfp k5 k618 agecat wc hc lwg inc, nolog
logit lfp c.k5 c.k618 i.agecat i.wc i.hc c.lwg c.inc, nolog

* interactions w/o fv's

generate wcXk5 = wc*k5
logit lfp wc k5 wcXk5 k618 age hc lwg inc, nolog
logit lfp wc k5 wc#k5 k618 age hc lwg inc, nolog

* interactions w fv's
logit lfp i.wc c.k5 i.wc#c.k5 k618 age hc lwg inc , nolog
logit lfp i.wc##c.k5 k618 age i.wc hc lwg inc, nolog
logit lfp c.age##c.age k5 k618 wc hc lwg inc, nolog
logit lfp k5 k618 c.age##c.age##c.age wc hc lwg inc, nolog
logit lfp i.wc##(c.k5 c.k618 c.age i.hc c.lwg c.inc), nolog
drop if agecat==2
logit lfp i.agecat c.inc##c.inc
sum i.agecat c.inc##c.inc if e(sample)
logit lfp i.agecat c.age##c.age, nolog
logit, coeflegend

* fvs
use binlfp4, clear
summarize i.wc##(i.hc c.k5 c.age)
summarize i.agecat c.inc##c.inc, nofvlabel

version 12
logit lfp i.agecat i.wc i.hc k5 k618 lwg inc, nolog nofvlabel

version 13
logit lfp i.agecat i.wc i.hc k5 k618 lwg inc, nolog

// missing values

use binlfp4-missing, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
logit lfp k5 i.agecat i.wc i.hc lwg inc, nolog

mark nomiss
markout nomiss lfp k5 k618 agecat wc hc lwg inc
tab nomiss

logit lfp k5 k618 i.agecat i.wc i.hc lwg inc if nomiss==1, nolog
logit lfp k5 i.agecat i.wc i.hc lwg inc if nomiss==1, nolog

use gsskidvalue4, clear
egen missing = rowmiss(age anykids black degree female kidvalue othrrace ///
    year income91 income)
tab missing

gen nomiss = (missing==0)
drop nomiss

* misstable

misstable summarize age anykids black degree female kidvalue othrrace ///
    year income91 income, all showzero

misstable patterns age anykids black degree female kidvalue othrrace ///
    year income91 income, freq

misstable summarize age anykids black degree female kidvalue othrrace ///
    year income91 income, gen(m_)

logit m_income female black othrrace age, nolog

logit m_income female black othrrace age i.degree, nolog
gen included = e(sample)
label var included "Cases included in logit on m_income"
tab included

//  weights and svy commands

use svyhrs4, clear
svyset secu [pweight=kwgtr], strata(stratum)

svy: logit arthritis male ib3.ed3cat age

svyset [pweight=kwgtr]
svy: logit arthritis male ib3.ed3cat age
est store svwt
logit arthritis male ib3.ed3cat age [pweight=kwgtr]
est store nowt

svyset secu
svy: logit arthritis male ib3.ed3cat age
est store svcl
logit arthritis male ib3.ed3cat age, cluster(secu)
est store nocl

svyset secu [pweight=kwgtr]
svy: logit arthritis male ib3.ed3cat age
est store svclwt
logit arthritis male ib3.ed3cat age [pweight=kwgtr], cluster(secu)
est store noclwt

//  storing estimates

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
estimates store logit1

use binlfp4, clear
logit lfp k5 if wc==1, nolog
estat summarize
estimates save model1, replace

clear all
estimates use model1
estimates replay
estimates describe

use binlfp4, clear
estimates esample: lfp k5 if wc==1
estat summarize

//  reformatting output with estimates table

use binlfp4, clear
logit lfp k5 i.agecat i.wc, nolog
estimates store logit_model

probit lfp k5 i.agecat i.wc, nolog
estimates store probit_model

estimates table logit_model probit_model, b(%12.3f) t varlabel
ereturn list

//  alternative output with listcoef

use science4, clear
regress job i.female i.phdclass mentcit3yr fellow pub1 cit1
listcoef, help
listcoef female phdclass pub1

//  wald tests

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog

test k5
test k5 k618
test k5 k618 2.agecat 3.agecat 1.wc 1.hc lwg inc
test 2.agecat 3.agecat
testparm i.agecat
test k5=k618
test 2.agecat=3.agecat
test k5=k618
test 1.wc=1.hc, accumulate
test (k5=k618) (1.wc=1.hc)

//  LR tests

logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
estimates store full

logit lfp i.agecat i.wc i.hc lwg inc, nolog
estimates store nokidvars

lrtest full nokidvars

//  estat command

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
estat summarize

//  measures of fit

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
fitstat, saving(basemodel)
quietly fitstat, saving(basemodel) // since sjlog destroys returns

gen kids = k5 + k618
label var kids "Number of kids 18 or younger"
logit lfp kids i.agecat i.wc i.hc lwg inc, nolog
fitstat, using(basemodel)

logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
estat classification

logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
fitstat, ic

//  predictions for each observation

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
predict prob
summarize prob

//  margins

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
margins
margins, atmeans
margins, at(k5=1 k618=1 agecat=1 wc=0 hc=0 lwg=1 inc=20)
margins, at(k5=1 k618=1 agecat=1 wc=0 hc=0) atmeans
margins, at(k5=1 k618=1 agecat=1 wc=0 hc=0 (mean) lwg inc)
margins, at(k5=1 k618=1 agecat=1 wc=0 hc=0 lwg=1.097 inc=20.13)
margins, at(k5=1 k618=1 agecat=1 wc=0 hc=0 (mean) lwg (median) inc)
margins, at(k5=1 k618=1 agecat=1 wc=0 hc=0 lwg=1)

* as observed using replace
replace k5 = 1
replace k618 = 1
replace agecat = 1
replace wc = 0
replace hc = 0
replace lwg = 1

predict prob
label var prob "predict with fixing values of all but inc"
sum prob

use binlfp4, clear
logit lfp k5 k618 age i.wc i.hc lwg inc, nolog
margins, at(wc=0 wc=1) atmeans
margins, at(wc=1 wc=0) atmeans
margins, at(age=(30(10)60)) atmeans
margins, at(age=(30 40 50 60)) atmeans

logit lfp k5 k618 age i.wc i.hc lwg inc, nolog
margins, at(age=(30(10)60) wc=(0 1)) atmeans
margins, at(wc=(0 1) age=(30(10)60)) atmeans
margins, at(wc=0 age=(30(10)60)) at(wc=1 age=(30(10)60)) atmeans

logit lfp k5 k618 age i.wc i.hc lwg inc, nolog
margins, at(age=(30(10)60) wc=(0 1)) atmeans noatlegend
mlistat

logit lfp k5 k618 age i.wc i.hc lwg inc, nolog
margins, at(wc=0 age=(30(10)60)) at(wc=1 age=(30(10)60)) atmeans noatlegend
mlistat, noconstant

//  predictions for groups

logit lfp k5 age i.wc i.hc, nolog
margins, at(wc=(0 1)) atmeans
margins wc, atmeans
margins, over(wc) atmeans

margins if wc==0, atmeans
margins if wc==1, atmeans

margins if wc==0, atmeans
margins if wc==1, atmeans

margins, at((omean) k5 (mean) age hc) over(wc)

//  margins predict() & expression()

use gssclass4, clear
ologit class i.fem i.white i.year i.ed age inc, nolog
margins, predict(outcome(#2))
margins, expression(1-predict(outcome(#1)))

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
margins, expression(1-predict(pr))
margins, predict(pr)
margins
margins, expression(predict(pr)-.5)

//  predictions using mtable

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
mtable, at(wc=(0 1) hc=(0 1)) atmeans
margins, at(wc=(0 1) hc=(0 1)) atmeans

* ci with mtable
mtable, at(wc=(0 1) hc=(0 1)) atmeans estname(Pr_LFP) ///
    statistics(ci)

* atvars with mtable
mtable, at(k5=0 k618=0 wc=(0 1)) atmeans atvars(k5 k618 wc) brief
mtable, at(wc=(0 1) hc=(0 1)) atmeans estname(Pr_LFP) atvars(_none)

use gssclass4, replace
ologit class i.fem i.white i.year i.ed age inc, nolog
mtable, at(fem=(0 1) white=(0 1)) atmeans stat(ci) dec(2)
mtable, at(fem=(0 1) white=(0 1)) atmeans stat(ci) dec(2) long
mtable, at(fem=(0 1) white=(0 1)) outcome(1 4) atmeans brief

use couart4.dta, clear
nbreg art i.fem i.mar i.phdcat kid5 ment
mtable, atmeans
mtable, pr(0) atmeans
mtable, pr(0/5) atmeans

//  combining tables with mtable

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
mtable, at(k5=(0 1 2 3)) coleqnm(1st)
mtable, at(k5=(0 1 2 3)) atmeans right brief coleqnm(2nd)
mtable, at(k5=(0 1 2 3)) coleqnm(1st) clear
mtable, at(k5=(0 1 2 3)) atmeans right coleqnm(2nd)
quietly mtable, at(k5=(0 1 2 3)) estname(asobserved)
mtable, at(k5=(0 1 2 3)) atmeans atvars(_none) estname(atmeans) right brief

quietly mtable, at(k5=(0 1 2 3)) estname(asobserved)
mtable, at(k5=(0 1 2 3)) atmeans atvars(_none) estname(atmeans) ///
    title(Predicted probability of labor force participation) ///
    right brief norownum

//  marginal effects margins

use binlfp4, clear
logit lfp k5 i.agecat i.wc inc, nolog
margins, dydx(*)

quietly mtable, at(wc=1) rowname(wc=1) statistics(ci) estname(Pr_LFP)
mtable, at(wc=0) rowname(wc=0) statistics(ci) estname(Pr_LFP) below
mtable, dydx(wc) rowname(wc=1 - wc=0) statistics(ci) estname(Pr_LFP) ///
    below brief

mtable, at(wc=(1 0)) post coeflegend details
matlist e(b)
lincom _b[1._at] - _b[2._at]
mlincom 1 - 2
mlincom 1 - 2, statistics(est pvalue)
mlincom 1 - 2, statistics(all)

//  mchange

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
mchange
mchange inc, amount(range) trim(25)
mchange inc, amount(sd) delta(5) uncentered
mchange inc, amount(all) statistics(all)
mchange wc, statistics(start end est pvalue)

// plotting predictions

use binlfp4, clear

logit lfp k5 k618 age i.wc i.hc lwg inc, nolog
margins, at(age=(20(10)80) wc=(0 1)) atmeans
margins, at(age=(20(10)80) wc=(0 1)) atmeans
marginsplot
graph export rm3ch3-marginsplot-intro.emf, replace

qui margins, at(age=(20(10)80) wc=(0 1)) atmeans
marginsplot, plotdim(wc, label("No College" "College"))
graph export rm3ch3-marginsplot-bywc.emf, replace

marginsplot, recast(line) plotopts(lwidth(medthick)) ///
    plot1opt(lcolor(green)) plot2opt(lcolor(blue))
graph export rm3ch3-marginsplot-line.emf, replace

qui margins, at(age=(20(10)80) wc=(0 1)) atmeans
marginsplot, recastci(rarea) ciopts(lcolor(gs11) fcolor(gs13))
graph export rm3ch3-marginsplot-rarea.emf, replace

* mgen

logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
mgen, at(inc=(0(10)100)) stub(A) atmeans
list Apr Ainc in 1/12, clean
graph twoway connected Apr Ainc
graph export rm3ch3-mgen-twoway.emf, replace

* mgen multi level
quietly mgen, at(inc=(0(10)100) agecat=1) atmeans stub(A30)
quietly mgen, at(inc=(0(10)100) agecat=2) atmeans stub(A40)
quietly mgen, at(inc=(0(10)100) agecat=3) atmeans stub(A50)
graph twoway connected A30pr A40pr A50pr A50inc, ///
    ytitle("Pr(In Labor Force)") xtitle("Income") ///
    legend(label(1 "Age 30-39") label(2 "40-49") label(3 "50+") cols(3))
graph export rm3ch3-mgen-multilevel.emf, replace

use gssclass4, clear
ologit class i.fem i.white i.year i.ed age inc, nolog
mgen, at(age=(20(10)80)) stub(B)
list Bage Bpr1 Bpr2 Bpr3 Bpr4 in 1/8, clean
twoway line Bpr1 Bpr2 Bpr3 Bpr4 Bage
graph export rm3ch3-mgen-ologit.emf, replace

capture log close
exit
