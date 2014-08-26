capture log close
log using rm3ch7-count, replace text

//  scott long & jeremy freese (jslong@indiana.edu & jfreese@northwestern.edu)
//  regression models for categorical outcomes using stata, 3rd
//  chapter 7: count outcomes | 2014-02-20

version 13.1
clear all
macro drop _all
set linesize 80
set scheme s2manual
program drop _all

//  fitting the poisson distribution with poisson

use couart4, clear
codebook art female married kid5 mentor phd, compact
tabulate art, missing
sum art
return  list
poisson art, nolog

//  plotting the poisson distribution with mgen

use couart4, clear
poisson art, nolog
mgen, pr(0/9) meanpred stub(psn)
list psnval psnobeq psnpreq in 1/10
label var psnobeq "Observed Proportion"
label var psnpreq "Poisson Prediction"
label var psnval "# of Articles"
graph twoway connected psnobeq psnpreq psnval, ///
    ytitle("Probability") ylabel(0(.1).4) xlabel(0/9) msym(O Th) ///
    lwid(thin thin) msize(*1.4 *1.2) mcol(gs1 gs10) // tweaks to remove from text
graph export rm3ch7-psn-dist.emf, replace

//  poisson regression model

use couart4, clear
gen artx = art + .3
poisson artx i.female i.married phd kid5 mentor, nolog
poisson art i.married phd kid5 mentor if female==1, nolog

//  prm estimation

use couart4, clear
poisson art i.female i.married kid5 phd mentor, nolog

//  prm factor change in rate

listcoef female mentor, help
listcoef female mentor, percent help

//  prm marginal change in rate

mchange kid5 phd mentor, amount(marginal)
mchange kid5 phd mentor, atmeans amount(marginal)
mchange mentor, at(female=1 kid5==0 married==1 phd==2 mentor==5) ///
    amount(marginal)
mchange mentor, at(female=0 kid5==0 married==1 phd==2 mentor==5) ///
    amount(marginal)

mchange mentor, at(female=1 kid5==0 married==1 phd==2 mentor==5) ///
    amount(marginal)
scalar me1 = el(r(table),1,1)
scalar b1 = el(r(basepred),1,1)
mchange mentor, at(female=0 kid5==0 married==1 phd==2 mentor==5) ///
    amount(marginal)
scalar me2 = el(r(table),1,1)
scalar b2 = el(r(basepred),1,1)
di "ME: men/women: " me2/me1
di "BASE: men/women: " b2/b1

margins, dydx(kid5 phd mentor)

//  prm discrete change in rate

mchange female mentor
mchange female, stat(ci)
mchange kid5, uncentered amount(one)
margins, dydx(female married)

//  prm interpretation using predicted probabilities

use couart4, clear
poisson art i.female i.married kid5 phd mentor, nolog
predict prob3, pr(3)
predict cprob3, pr(0,3)
sum prob3 cprob3

capture drop pr*
forvalues icount = 0/5 {
	predict prob`icount', pr(`icount')
	label var prob`icount' "Pr(y=`icount')"
}
dotplot prob0-prob5, ylabel(0(.1).5, grid gmin gmax)
graph export rm3ch7-prm-predict-dotplot.emf, replace

margins, predict(pr(0,3))
mtable, predict(pr(0,3)) ci

forvalues i = 0/5 {
    margins, predict(pr(`i'))
}

mtable, pr(0/5) stat(ci)

mtable, at(married=(0 1) female=1 kid5=0) atmeans pr(0/5)

mtable, rowname(Married) at(married=1 female=1 kid5=0) atmeans pr(0/5)
mtable, rowname(Single) at(married=0 female=1 kid5=0) atmeans pr(0/5) below
mtable, rowname(Difference) dydx(married) at(female=1 kid5=0) atmeans ///
    pr(0/5) stats(est p) below width(7)

mchange married, at(female=1 kid5=0) atmeans pr(0/5) ///
    stat(from to change p) width(7)

qui mtable, at(married=1 female=1 kid5=(0 1 2 3)) long
mtable, at(married=1 female=1 kid5=(0 1 2 3)) pr(0/5) ///
    atvars(_none) right norownumbers brief

//  assessing fit

qui poisson art i.female i.married kid5 phd mentor, nolog
qui fitstat, ic save
qui poisson art i.female i.married i.kid5 phd mentor, nolog
fitstat, ic diff

qui poisson art i.female i.married i.kid5 phd mentor, nolog
fitstat, ic diff

qui poisson art i.female i.married kid5 phd mentor, nolog
fitstat, ic save
qui mtable, at(married=1 female=1 kid5=(0 1 2 3)) long
mtable, at(married=1 female=1 kid5=(0 1 2 3)) pr(0/1) ///
    atvars(_none) right norownumbers brief

qui poisson art i.female i.married i.kid5 phd mentor, nolog
fitstat, ic diff
qui mtable, at(married=1 female=1 kid5=(0 1 2 3)) long
mtable, at(married=1 female=1 kid5=(0 1 2 3)) pr(0/1) ///
    atvars(_none) right norownumbers brief

qui poisson art i.female i.married kid5 phd mentor, nolog
qui mtable, at(married=1 female=1 kid5=(0 1 2 3)) long
mtable, at(married=1 female=1 kid5=(0 1 2 3)) pr(0/1) ///
    atvars(_none) right norownumbers brief

qui poisson art i.female i.married i.kid5 phd mentor, nolog
qui mtable, at(married=1 female=1 kid5=(0 1 2 3)) long
mtable, at(married=1 female=1 kid5=(0 1 2 3)) pr(0/1) ///
    atvars(_none) right norownumbers brief

//  plots of probabilities

qui poisson art i.female i.married kid5 phd mentor, nolog
mgen, at(female=1 married=1 kid5=(0/3)) stub(Fprm) pr(0)
mgen, at(female=0 married=1 kid5=(0/3)) stub(Mprm) pr(0)
label var Fprmpr0 "Married women"
label var Mprmpr0 "Married men"
label var Mprmkid5  "Number of children"
graph twoway connected Fprmpr0 Mprmpr0 Mprmkid5, ///
    msiz(*1.6 *1.265) lwid(thin thin) mcol(black gs8) ///
    ylabel(0(.1).4) yline(.1 .2 .3) xlabel(0/3) msym(O D) ///
    ytitle("Probability of No Articles")
graph export rm3ch7-prm-prob-menwomen.emf, replace

//  prm mean prediction

poisson art, nolog
mgen, stub(PDF) pr(0/9) meanpred
poisson art i.female i.married kid5 phd mentor, nolog
mgen, stub(PRM) pr(0/9) meanpred
label var PDFobeq "Observed"
label var PDFpreq "Poisson PDF"
label var PRMpreq "PRM"
graph twoway connected PDFobeq PDFpreq PRMpreq PRMval, ///
    msym(O Th Sh) msiz(*1.4 *1.4 *1.17) lwid(thin thin thin) ///
    ytitle("Probability of Count") ylabel(0(.1).4) xlabel(0/9)
graph export rm3ch7-prm-meanpred.emf, replace

//  prm - exposure time

use couexposure4, clear
poisson totalarts kid5 mentor, nolog irr
estimates store NoExposure

poisson totalarts kid5 mentor, nolog irr exposure(profage)
estimates store Exposure

estimates table NoExposure Exposure, b(%11.2f) t(%11.2f) eform

constraint define 1 lnprofage=1
poisson totalarts lnprofage kid5 mentor, nolog constraint(1)

poisson totalarts kid5 mentor, nolog irr offset(lnprofage)

//  negative binomial model

use couart4, clear

nbreg art i.female i.married kid5 phd mentor, nolog

//  comparing PRM and NBRM

qui poisson art i.female i.married kid5 phd mentor
estimates store PRM
qui nbreg art i.female i.married kid5 phd mentor
estimates store NBRM
estimates table PRM NBRM, b(%9.3f) t p(%9.3f) varlabel ///
    drop(lnalpha:_cons) stats(alpha N) eform vsquish

//  robust SE's

qui poisson art i.female i.married kid5 phd mentor
estimates store PRM
qui poisson art i.female i.married kid5 phd mentor, vce(robust)
estimates store PRMrobust
qui nbreg art i.female i.married kid5 phd mentor
estimates store NBRM
qui nbreg art i.female i.married kid5 phd mentor, vce(robust)
estimates store NBRMrobust
estimates table PRM PRMrobust NBRM NBRMrobust, b(%9.3f) p(%9.3f) t varlabel ///
    drop(lnalpha:_cons) stats(alpha N) eform vsquish modelwidth(10)

//  factor and precent change

estimates restore NBRMrobust
listcoef female mentor
listcoef female mentor, percent

estimates restore PRM
qui mtable, estname(PRM_mu) ci at(mentor=(0 10 25 50) female=1 phd=4) ///
    atmeans clear dec(2)
estimates restore NBRM
mtable, estname(NBRM_mu) ci at(mentor=(0 10 25 50) female=1 phd=4) ///
    atmeans right dec(2) noatvars norownumbers brief

estimates restore PRMrobust
qui mtable, estname(PRM_mu) ci at(mentor=(0 10 25 50) female=1 phd=4) ///
    atmeans clear dec(2)
estimates restore NBRMrobust
mtable, estname(NBRM_mu) ci at(mentor=(0 10 25 50) female=1 phd=4) ///
    atmeans right dec(2) noatvars norownumbers brief

estimates restore PRMrobust
qui mtable, rowname(PRM) pr(0/7) atmeans
estimates restore NBRMrobust
mtable, rowname(NBRM) pr(0/7) atmeans below brief decimals(3) width(6)

estimates restore PRMrobust
mgen, atmeans at(mentor=(0(2.5)50)) stub(PRM) pr(0)
estimates restore NBRMrobust
mgen, atmeans at(mentor=(0(2.5)50)) stub(NBRM) pr(0)

label var PRMpr0 "PRM"
label var NBRMpr0 "NBRM"
graph twoway connected PRMpr0 NBRMpr0 NBRMmentor, ///
    ylabel(0(.1).4, grid gmin gmax) ytitle("Probability of a zero count")
graph export rm3ch7-nbrm-prm-prob0.emf, replace

label var PRMpr0 "PRM"
label var NBRMpr0 "NBRM"
graph twoway ///
    (rarea PRMll0 PRMul0 NBRMmentor, color(gs14)) ///
    (rarea NBRMll0 NBRMul0 NBRMmentor, color(gs14)) ///
    (connected PRMpr0 NBRMmentor, lpattern(dash) msize(zero)) ///
    (connected NBRMpr0 NBRMmentor, lpattern(solid) msize(zero)), ///
    legend(on order(3 4)) ylabel(0(.1).4, grid gmin gmax) ///
    ytitle("Probability of a zero count")
graph export rm3ch7-nbrm-prm-prob0ci.emf, replace

estimates restore PRM
mgen, atmeans at(mentor=(0(2.5)50)) stub(PRM) pr(0) replace
estimates restore NBRM
mgen, atmeans at(mentor=(0(2.5)50)) stub(NBRM) pr(0) replace
label var PRMpr0 "PRM"
label var NBRMpr0 "NBRM"
graph twoway ///
    (rarea PRMll0 PRMul0 NBRMmentor, color(gs14)) ///
    (rarea NBRMll0 NBRMul0 NBRMmentor, color(gs14)) ///
    (connected PRMpr0 NBRMmentor, lpattern(dash) msize(zero)) ///
    (connected NBRMpr0 NBRMmentor, lpattern(solid) msize(zero)), ///
    legend(on order(3 4)) ylabel(0(.1).4, grid gmin gmax) ///
    subtitle(Not using robust standard errors,pos(11)) ///
    ytitle("Probability of a zero count")
graph export rm3ch7-nbrm-prm-prob0ci-notrobust.emf, replace

//  fitting zero truncated models

use couart4, clear
nbreg art i.female i.married kid5 phd mentor, nolog vce(robust)
estimates store NBRM

drop if art==0
tnbreg art i.female i.married kid5 phd mentor, nolog vce(robust)
estimates store ZTNBRM

estimates table NBRM ZTNBRM, b(%9.3f) stats(N) t eform

estimates restore ZTNBRM
listcoef female mentor

predict rate
label var rate "Unconditional rate of productivity"
predict crate, cm
label var rate "Conditional rate of productivity given at least 1 article"
summarize rate crate art

qui mtable, rowname(Unconditional) at(female=1 married=1 kid5=0) atmeans ci
mtable, rowname(Conditional) at(female=1 married=1 kid5=0) atmeans ci ///
    predict(cm) below brief

mtable, at(female=1 married=1 kid5=0) atmeans pr(0/5) ci brief

qui mtable, rowname(Unconditional) at(female=1 married=1 kid5=0) atmeans ///
    pr(1/5)
mtable, rowname(Conditional) at(female=1 married=1 kid5=0) atmeans ///
    cpr(1/5) below brief

//  nbrm hurdle model

use couart4, clear
logit art i.female i.married kid5 phd mentor, nolog or vce(robust)
predict prlogit
est store Hlogit

probit art i.female i.married kid5 phd mentor, nolog vce(robust)
predict prprobit
gen is1 = 1 - (art==0)
sum is1 pr*

tnbreg art i.female i.married kid5 phd mentor if art>0, nolog irr vce(robust)
est store Hztnb

est table Hlogit Hztnb, b(%9.3f) p(%9.3f) eform

estimates restore Hlogit
predict Hprobgt0
label variable Hprobgt0 "Pr(y>0)"
gen Hprob0 = 1 - Hprobgt0
label variable Hprob0 "Pr(y=0)"
summarize Hprob0 Hprobgt0

estimates restore Hztnb
predict Hcrate, cm
label variable Hcrate "Conditional rate"
gen Hrate = Hcrate * Hprobgt0
label variable Hrate "Unconditional rate"
sum Hcrate Hrate

forvalues icount = 1/8 {
    predict Hcprob`icount', cpr(`icount')
    label variable Hcprob`icount' "Pr(y=`icount'|y>0)"
    gen Hprob`icount' = Hcprob`icount' * Hprobgt0
    label variable Hprob`icount' "Pr(y=`icount')"
}
sum Hprob*

//  predictions for user specified values

est restore Hlogit
mtable, at(female=0 phd=1 mentor=0) atmeans ci

mtable, at(female=0 phd=1 mentor=0) atmeans
matlist r(table)
local prgt0 = el(r(table),1,1)
display `prgt0'

est restore Hztnb
mtable, at(female=0 phd=1 mentor=0) atmeans noesample cpr(1/8)

est restore Hlogit
mtable, at(female=0 phd=1 mentor=0) atmeans
local prgt0 = el(r(table),1,1)
est restore Hztnb
mtable, at(female=0 phd=1 mentor=0) atmeans noesample cpr(1/8)
matrix Huncond = `prgt0' * r(table)
matlist Huncond, format(%8.3f) title(Unconditional probabilties) names(col)

//  zinb - publication example

sysuse couart4, clear
zinb art i.female i.married kid5 phd mentor, ///
    inflate(i.female i.married kid5 phd mentor) nolog vce(robust)
estimates store zinb
listcoef, help

qui mtable, at(female=0 married=1 kid5=3 phd=3 mentor=10) ///
    atvars(female phd mentor) pr(0/5)
mtable, at(female=1 married=1 kid5=3 phd=1 mentor=0) ///
    atvars(female phd mentor) pr(0/5) below width(7) norownumbers

qui mtable, at(female=0 married=1 kid5=3 phd=3 mentor=10) ///
    atvars(female phd mentor) predict(pr)
mtable, at(female=1 married=1 kid5=3 phd=1 mentor=0) ///
    atvars(female phd mentor) predict(pr) below norownumbers decimals(4)

mgen, at(mentor=(0/6)) atmeans pr(0) stub(ZINB) replace
mgen, at(mentor=(0/6)) atmeans predict(pr) stub(ZINB) replace
gen ZINBprcount0 = ZINBprany0 - ZINBprall0
label var ZINBprall0 "Always Zero from Binary Equation"
label var ZINBprcount0 "Sometimes Zero from Count Equation"
label var ZINBprany0 "Zeroes from Both Equations"
label var ZINBmentor "Mentor's Publications"

graph twoway connected ZINBprall0 ZINBprcount0 ZINBprany0 ZINBmentor, ///
    xlabel(0/6) ylabel(0(.1).5, grid gmin gmax) ///
    ytitle(Probability of Zero) msymbol(Sh Dh O) ///
    legend(rows(3)) ///
    msize(*1.2 *1.1 *1.5) lwidth(thin thin thin) mcolor(gs4 gs4 gs4)
graph export rm3ch7-zinb-mentor-zeros.emf, replace

//  comparing models

sysuse couart4, clear

poisson art i.female i.married kid5 phd mentor, nolog
mgen, stub(PRM) pr(0/9) meanpred
nbreg art i.female i.married kid5 phd mentor, nolog
mgen, stub(NBRM) pr(0/9) meanpred
zip art i.female i.married kid5 phd mentor, ///
    inflate(i.female i.married kid5 phd mentor) nolog
mgen, stub(ZIP) pr(0/9) meanpred
zinb art i.female i.married kid5 phd mentor, ///
    inflate(i.female i.married kid5 phd mentor) nolog
mgen, stub(ZINB) pr(0/9) meanpred

label variable PRMobeq  "Observed"
label variable PRMpreq  "PRM"
label variable NBRMpreq "NBRM"
label variable ZIPpreq  "ZIP"
label variable ZINBpreq "ZINB"
graph twoway connected PRMobeq PRMpreq NBRMpreq ZIPpreq ZINBpreq PRMval, ///
    ytitle(Average Predicted Probability) ///
    xlabel(0/9) msymbol(Th Oh Sh O S) ///
    legend(col(3) holes(4)) ///
    msiz(*1.2 *1.4 *1.1 *1.4 *1.1) lwid(thin thin thin thin thin)
graph export rm3ch7-compare-simple.emf, replace

gen obs = PRMobeq
gen dPRM = obs - PRMpreq
label var dPRM "PRM"
gen dNBRM = obs - NBRMpreq
label var dNBRM "NBRM"
gen dZIP = obs - ZIPpreq
label var dZIP "ZIP"
gen dZINB = obs - ZINBpreq
label var dZINB "ZINB"
graph twoway connected dPRM dNBRM dZIP dZINB PRMval, ///
    ytitle(Observed-Predicted) ylabel(-.10(.05).10, grid gmin gmax) ///
    xlabel(0/9) msymbol(Oh Sh O S) ///
    subtitle(Positive deviations indicate more observed counts than predicted, ///
    pos(11)) ///
    msiz(*1.4 *1.1 *1.4 *1.1) lwid(thin thin thin thin thin)
graph export rm3ch7-compare-lambert.emf, replace

//  tests to compare count models

sysuse couart4, clear
quietly zip art i.female i.married kid5 phd mentor, ///
    inflate(i.female i.married kid5 phd mentor)
estimates store zip
quietly zinb art i.female i.married kid5 phd mentor, ///
    inflate(i.female i.married kid5 phd mentor)
estimates store zinb
lrtest zip zinb, force

zip art i.female i.married kid5 phd mentor, ///
    inflate(i.female i.married kid5 phd mentor) vuong nolog
listcoef, help

zinb art i.female i.married kid5 phd mentor, ///
    inflate(i.female i.married kid5 phd mentor) vuong nolog
listcoef, help

// vuong test to compare hurdle models

capture drop ZINB*
capture drop HRM*
qui zinb art i.female i.married kid5 phd mentor, ///
    inflate(i.female i.married kid5 phd mentor)
forvalues icount = 0/19 {
    predict ZINB`icount', pr(`icount')
}

qui logit art i.female i.married kid5 phd mentor
estimates store Hlogit
qui tnbreg art i.female i.married kid5 phd mentor if art > 0
estimates store Hztnb
estimates restore Hlogit
predict HRMnot0
label var HRMnot0 "HRM prob of non-zero count"
gen HRM0 = 1 - HRMnot0
label var HRM0 "HRM prob of zero count"

estimates restore Hztnb
forvalues icount = 1/19 {
    predict HRM`icount', cpr(`icount')
    qui replace HRM`icount' = HRM`icount' * HRMnot0
    label var HRM`icount' "HRM unconditional prob(`icount')"
}

gen ZINBprobs = .
label var ZINBprobs "ZINB prob of count that was observed"
gen HRMprobs = .
label var HRMprobs "HRM prob of count that was observed"
forvalues icount = 0/19 {
    qui replace ZINBprobs = ZINB`icount' if art == `icount'
    qui replace HRMprobs = HRM`icount' if art == `icount'
}

sum ZINBprob HRMprobs

gen mZINB_HRM = ln(ZINBprobs/HRMprobs)
sum mZINB_HRM
display (r(mean)*sqrt(r(N)))/r(sd)

//  ZINB v NB vuong: not shown in text

capture drop ZINB*
zinb art i.female i.married kid5 phd mentor, ///
    inflate(i.female i.married kid5 phd mentor) vuong
forvalues icount = 0/19 {
    predict ZINB`icount', pr(`icount')
}
nbreg art i.female i.married kid5 phd mentor
forvalues icount = 0/19 {
    predict NBRM`icount', pr(`icount')
}
gen ZINBprobs = .
gen NBRMprobs = .
forvalues icount = 0/19 {
    qui replace ZINBprobs = ZINB`icount' if art == `icount'
    qui replace NBRMprobs = NBRM`icount' if art == `icount'
}
sum ZINBprob NBRMprobs
gen mZINB_NBRM = ln(ZINBprobs/NBRMprobs)
sum mZINB_NBRM
display "ZINB vs NBRM: " (r(mean)*sqrt(r(N)))/r(sd)

//  ZIP and ZINB

capture drop ZIP* mZ*
zip art i.female i.married kid5 phd mentor, ///
    inflate(i.female i.married kid5 phd mentor) vuong
forvalues icount = 0/19 {
    predict ZIP`icount', pr(`icount')
}
gen ZIPprobs = .
forvalues icount = 0/19 {
    qui replace ZIPprobs = ZIP`icount' if art == `icount'
}
sum ZINBprob ZIPprobs
gen mZINB_ZIP = ln(ZINBprobs/ZIPprobs)
sum mZINB_ZIP
display "ZINB vs ZIP: " (r(mean)*sqrt(r(N)))/r(sd)

//  countfit to compare count models

sysuse couart4, clear
countfit art i.female i.married kid5 phd mentor, ///
    inflate(i.mentor i.female)
graph export rm3ch7-countfit-lambert.emf, replace

log close
exit

DROPPED =============================================================

use couart4, clear
describe art female married kid5 mentor phdcat
summarize art female married kid5 mentor
tabulate phdcat

tabulate phdcat, missing

sum art
display "mean = " %6.3f `r(mean)' " var  = " %6.3f `r(Var)'

* categorical variable
mchange phdcat

* rate by phd
mtable, at(phd=(1/4))

* plot mu for mentor
mgen, at(mentor=(0(5)40)) atmeans replace

twoway ///
  (rarea _ul _ll _mentor, color(gs12)) ///
  (connected _mu _mentor, msymbol(i)) ///
 , title("Other variables held at their means", position(11)) ///
   ytitle(Mean number of articles) ylabel(0/5, grid gmin gmax) legend(off)

* margins for dc with factor variables
margins, dydx(female married phdcat)

* predicted rate for prm and nbrm - using margins
estimates restore PRM
margins, atmeans
estimates restore NBRM
margins, atmeans noatlegend

// truncated
tpoisson art i.female i.married kid5 phd mentor, nolog irr

* unconditional prob 0 to 5 for married, women w/o kids
qui mtable, at(female=1 married=1 kid5=0) atmeans pr(1/6) ///
    rowname(Unconditional)
mtable, at(female=1 married=1 kid5=0) atmeans cpr(1/6) ///
    rowname(Conditional) below

* effects of kids
estimates restore ZTNBRM
qui mtable, at(female=0 married=1 kid5=(0 1)) atvar(kid5) estname(uncond)
mtable, at(female=0 married=0 kid5=(0 1)) predict(cm) noatvars right estname(cond)

qui mtable, at(female=1 married=1 kid5=0) atmeans rowname(Unconditional)
mtable, at(female=1 married=1 kid5=0) atmeans predict(cm) ///
    rowname(Conditional) below

estimates restore ZTNBRM
qui mtable, atmeans rowname(Unconditional)
mtable, atmeans predict(cm) rowname(Conditional) below brief

* NOT THIS!
mtable, at(female=0 phd=1 mentor=0) atmeans noesample pr(1/8)
