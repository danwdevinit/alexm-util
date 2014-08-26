capture log close
log using rm3ch4-binary, replace text

//  scott long & jeremy freese (jslong@indiana.edu & jfreese@northwestern.edu)
//  regression models for categorical outcomes using stata, 3rd
//  chapter 3: binary outcomes | 2014-02-20

version 13.1
clear all
macro drop _all
set linesize 80
set scheme s2manual
program drop _all

//  estimation using logit and probit

use binlfp4, clear
codebook lfp k5 k618 agecat wc hc lwg inc, compact
tabulate agecat, missing
logit lfp k5 k618 agecat wc hc lwg inc
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog

* estimates store
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
estimates store m1logit
probit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
estimates store m1probit
estimates table m1logit m1probit, b(%9.3f) t varlabel varwidth(30)

//  perfect prediction

use binlfp4, clear
tab lfp k5

** * exact estimation: very slow so do not run each time
** exlogistic lfp k5_1 k5_2 k5_3, memory(2gb)

logit lfp k5_1 k5_2 k5_3, or
estimates store logit

logit lfp k5_1 k5_2 k5_3, or asis
estimates store asis
estimates table logit asis, b(%15.3f) t varlabel varwidth(20) eform

//  testing individual coefficients

use binlfp4, clear
logit lfp k5 i.wc i.hc k618 i.agecat lwg inc, nolog
test k5
di sqrt(r(chi2))
display sqrt(52.57)

test 1.wc

logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
estimates store Mfull
logit lfp k618 i.agecat i.wc i.hc lwg inc, nolog
estimates store Mnok5
lrtest Mfull Mnok5

lrtest Mfull .

//  testing multiple coefficients

estimates restore Mfull
test 1.hc 1.wc
test 1.hc = 1.wc
test 2.agecat 3.agecat
testparm i.agecat
test k5 k618 2.agecat 3.agecat 1.wc 1.hc lwg inc

logit lfp k5 k618 i.agecat lwg inc, nolog
estimates store Mnowchc
lrtest Mfull Mnowchc
logit lfp, nolog
estimates store Mconstant
lrtest Mfull Mconstant

//  predicted probabilities with predict

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog

predict prlogit
summarize prlogit
codebook prlogit, compact
label var prlogit "Logit: Pr(lfp | X)"
dotplot prlogit, ylabel(0(.2)1, grid gmin gmax)
graph export rm3ch4-predict-dotplot.emf, replace

* logit and probit

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
predict prlogit
label var prlogit "Logit: Pr(lfp | X)"
probit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
predict prprobit
label var prprobit "Probit: Pr(lfp | X)"
pwcorr prlogit prprobit
scatter prlogit prprobit, xlabel(0(.25)1, grid) ylabel(0(.25)1, grid) ///
    msymbol(Oh) aspect(1)
graph export rm3ch4-predict-logitprobit.emf, replace

//  residuals and influence

use binlfp4, clear
* stable keeps order within covariate sets stable
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
predict rstd, rs
label var rstd "Standardized Residual"
sort inc, stable
generate index = _n
label var index "Observation Number"

graph twoway scatter rstd index, msymbol(Oh) mcolor(black) ///
    xlabel(0(200)800) ylabel(-4(2)4, grid gmin gmax) yline(0, lcolor(black))
graph export rm3ch4-resids.emf, replace

graph twoway scatter rstd index, ///
    msymbol(none) mlabel(index) mlabposition(0) xlabel(0(200)800) ///
    ylabel(-4(2)4, grid gmin gmax) yline(0, lcolor(black))
graph export rm3ch4-resid-index.emf, replace

list lfp k5 k618 2.agecat 3.agecat wc hc lwg inc in 142, clean
list rstd index if rstd>2.5 | rstd<-2.5, clean

graph twoway scatter rstd index if (rstd>1.7) | (rstd<-1.7), ///
    msymbol(none) mlabel(k5) mlabposition(0) xlabel(0(200)800) ///
    ylabel(-4(2)4, grid gmin gmax) yline(0, lcolor(black)) ///
    caption("Values indicate # of young children")
graph export rm3ch4-resid-large.emf , replace

predict cook, dbeta
label var cook "Cook's Statistic"
graph twoway scatter cook index, ///
    msymbol(none) mlabel(index) mlabposition(0) ///
    xlabel(0(200)800) xtitle("Observation Number") ///
    ylabel(0(.1).3, grid gmin gmax)
graph export rm3ch4-influence-cook.emf , replace

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
leastlikely k5 k618 wc agecat

//  scalar measures of fit

use binlfp4, clear
quietly logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
estimates store Model1
quietly fitstat, save
quietly logit lfp k5 i.agecat i.wc c.inc##c.inc, nolog
estimates store Model2

estimates table Model1 Model2, b(%9.3f) p(%9.3f) stats(N ll r2_p bic aic)

fitstat, diff

estimates restore Model1
quietly fitstat, save ic
estimates restore Model2
fitstat, diff ic

estimates restore Model2
estat ic

//  hosmer-lemeshow test

use binlfp4, clear
quietly logit lfp k5 k618 c.age i.wc i.hc lwg c.inc, nolog
estat gof, group(10)

quietly logit lfp k5 k618 i.agecat i.wc i.hc lwg c.inc, nolog
estat gof, group(10)

quietly logit lfp k5 k618 i.agecat i.wc i.hc lwg c.inc, nolog
capture matrix drop hl
forvalues numgroups = 5(1)15 {
    quietly estat gof, group(`numgroups')
	local rm = r(m)
    matrix r =  r(chi2), r(df), chi2tail(r(df),r(chi2))
	matrix rownames r = "`rm' groups"
    matrix hl = nullmat(hl) \ r
}
matrix colnames hl = chi2 df prob
matlist hl, format(%8.3f)

//	interpreting coefficients for y*

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
listcoef, std

//  odds ratios

estimates store base
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, or nolog
pwcompare agecat, effects eform
listcoef, help
listcoef, reverse
listcoef, help percent
listcoef, help std

//  marginal effects =========================================================

clear all
macro drop _all

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
mchange, details

logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
estimate store base
estat summarize, labels

//  AME

mchange k5 k618 lwg inc, amount(range) trim(5)
centile k5 k618 lwg inc, centile(0 5 95 100)

mchange k618 lwg inc, amount(range) trim(5) ///
    statistics(change from to pvalue) decimals(2)
mchange k5, amount(one) uncentered
mchange lwg inc
mchange hc wc, stat(change from to pvalue)
mchange agecat, stat(change from to pvalue)
mchange
mchange, amount(one sd) decimals(2)

//  MEM

mchange, statistics(ci) atmeans
mchange, at(wc=1 hc=1) atmeans

//  MEM vs AME

mchange, asobserved amount(sd)
mchange, atmeans amount(sd)

//  margins

margins, at(k5=0) at(k5=3) post
mlincom 2 - 1
lincom _b[2._at] - _b[1bn._at]

estimates restore base
mchange k5, amount(range) statistics(change pvalue ll ul)
margins, at(inc=gen(inc)) at(inc=gen(inc+1)) post
mlincom 2 - 1
estimates restore base

//  nonstandard changes

mchange inc, delta(5)

//  second difference

estimates restore base
mchange wc if hc==1
mchange wc if hc==0

mtable, dydx(wc) over(hc) post
test _b[1.wc:0.hc]=_b[1.wc:1.hc]
mlincom 1 - 2

logit lfp k5 k618 i.agecat wc##hc lwg inc, nolog
mtable, dydx(wc) over(hc) post
mlincom 1 - 2
estimates restore base

//  quadratic term

logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
mchange inc, atmeans

use binlfp4, clear
logit lfp c.inc##c.inc k5 k618 i.agecat i.wc i.hc lwg, nolog
mchange inc, atmeans

//  distribution of ME's

probit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
predict double pbtxb, xb
label var pbtxb "xb"
gen double pbtpdf = normalden(pbtxb)
label var pbtpdf "normal pdf at xb"
gen double pbtmcinc = pbtpdf * _b[inc]
label var  pbtmcinc "Marginal change of inc from probit"

logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
predict double prhat if e(sample)
gen double mcinc = prhat * (1-prhat) * _b[inc]
label var mcinc "Marginal change of inc on Pr(LFP)"

pwcorr pbtmcinc mcinc

* mc AME and MEM for inc

quietly mchange inc
local ame = el(r(table),3,1)
quietly mchange inc, atmeans
local mem = el(r(table),3,1)

histogram mcinc, xlab(-.008(.002)0) ylab(0(.1).4,grid) ///
    fraction bin(25) col(gs10) fcol(gs12) ///
    text(.015 `ame' "AME", place(center)) ///
    text(.000 `ame' "|", place(center)) ///
    text(.015 `mem' "MEM", place(center)) ///
    text(.000 `mem' "|", place(center))
graph export rm3ch4-marginal-dist-inc.emf,  replace

* discrete change computed using predict
gen wc_orig = wc
replace wc = 0
predict double prhat_wc0
replace wc = 1
predict double prhat_wc1
replace wc = wc_orig
drop wc_orig
gen double dc_wc = prhat_wc1 - prhat_wc0
label var dc_wc "Discrete change of wc on Pr(LFP)"

* dc wc get AME and MEM
quietly mchange wc
local ame = el(r(table),1,1)
quietly mchange wc, atmeans
local mem = el(r(table),1,1)

* dc wc histogram
histogram dc_wc, xlab(0(.05).20) ylab(0(.1).3,grid) ///
    fraction bin(25) col(gs10) fcol(gs12) ///
    text(.015 `ame' "AME", place(center)) ///
    text(.000 `ame' "|", place(center)) ///
    text(.015 `mem' "MEM", place(center)) ///
    text(.000 `mem' "|", place(center))
graph export rm3ch4-discrete-dist-wc.emf,  replace

//  ideal types

* an average person
* young, lower class, 2 young kids
* young, college, 2 young kids
* middle age, college, 2 teen kids
* older, college, 2 adult kids

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
estimates store base

mtable, rowname(Average Family) ci clear atmeans

mtable, rowname(Young w Low SES Young Kids) ci below ///
    at(agecat=1 k5=2 k618=0 inc=10 lwg=.75 hc=0 wc=0)

qui mtable, rowname(Young w Low SES Young Kids) ci clear ///
    at(agecat=1 k5=2 k618=0 inc=10 lwg=.75 hc=0 wc=0)
qui mtable, rowname(Young w ColDeg Young Kids) ci below ///
    at(agecat==1 k5==2 k618==0 wc==1 hc==1) atmeans
qui mtable, rowname(Midage w ColDeg Teen Kids) ci below ///
    at(agecat==2 k5==0 k618==2 wc==1 hc==1) atmeans
mtable, rowname(Older w ColDeg Adult Kids) ci below ///
    at(agecat==3 k5==0 k618==0 wc==1 hc==1) atmeans

* selection variables

capture drop _sel*
gen _selYC = agecat==1 & k5==2 & k618==0 & wc==1 & hc==1
label var _selYC "Select type Young w ColDeg Young Kids"
gen _selMC = agecat==2 & k5==0 & k618==2 & wc==1 & hc==1
label var _selMC "Select type Midage w ColDeg Teen Kids"
gen _selOC = agecat==3 & k5==0 & k618==0 & wc==1 & hc==1
label var _selOC "Select type Older w ColDeg Adult Kids"

qui mtable, rowname(Young w Low SES Young Kids) ci clear ///
    at(agecat=1 k5=2 k618=0 inc=10 lwg=.75 hc=0 wc=0)
qui mtable if _selYC==1, rowname(Young w ColDeg Young Kids) ci below ///
    at(agecat==1 k5==2 k618==0 wc==1 hc==1) atmeans
qui mtable if _selMC==1, rowname(Midage w ColDeg Teen Kids) ci below ///
    at(agecat==2 k5==0 k618==2 wc==1 hc==1) atmeans
mtable if _selOC==1, rowname(Older w ColDeg Adult Kids) ci below ///
    at(agecat==3 k5==0 k618==0 wc==1 hc==1) atmeans

qui mtable, rowname(Young w Low SES Young Kids) ci clear ///
    at(agecat=1 k5=2 k618=0 inc=10 lwg=.75 hc=0 wc=0)
qui mtable if _selYC==1, rowname(Young w ColDeg Young Kids) atmeans ci below
qui mtable if _selMC==1, rowname(Midage w ColDeg Teen Kids) atmeans ci below
mtable if _selOC==1, rowname(Older w ColDeg Adult Kids) atmeans ci below

qui mtable, rowname(Young w Low SES Young Kids) ci clear ///
    at(agecat=1 k5=2 k618=0 inc=10 lwg=.75 hc=0 wc=0)

qui mtable if _selYC==1, rowname(Young w ColDeg Young Kids) atmeans ci below

qui mtable if _selMC==1, rowname(Midage w ColDeg Teen Kids) atmeans ci below
mtable if _selOC==1, rowname(Older w ColDeg Adult Kids) atmeans ci below

//  testing profiles

mtable, atmeans post ///
    at(agecat=1 k5=2 k618=0 wc=0 hc=0 lwg=.75  inc=10) /// ideal type #1
    at(agecat=1 k5=2 k618=0 wc=1 hc=1 lwg=1.62 inc=16.64) // ideal type #2
mlincom 1 - 2
estimates restore base

mtable, atmeans at(agecat=1 k5=2 k618=0 wc=0 hc=0 lwg=.75  inc=10)

mtable, atmeans at(agecat=1 k5=2 k618=0 wc=0 hc=0 lwg=.75  inc=10)
display "`r(atspec)'"
local myatspec `r(atspec)'

mtable, atmeans at(agecat=1 k5=2 k618=0 wc=0 hc=0 lwg=.75  inc=10)
local myatspec `r(atspec)'
mtable, atmeans at(`myatspec')

qui mtable, atmeans
local Average `r(atspec)'
qui mtable, atmeans at(agecat=1 k5=2 k618=0 inc=10 lwg=.75 hc=0 wc=0)
local YngLow `r(atspec)'
qui mtable if _selYC == 1, atmeans
local YngCol `r(atspec)'
qui mtable if _selMC == 1, atmeans
local MidCol `r(atspec)'
qui mtable if _selOC == 1, atmeans
local OldCol `r(atspec)'

mtable, at(`Average') at(`YngLow') at(`YngCol') ///
    at(`MidCol') at(`OldCol') post
mlincom 2 - 3

estimates restore base
margins, at(`Average') at(`YngLow') at(`YngCol') ///
    at(`MidCol') at(`OldCol') noatlegend pwcompare(effects)

//  mchange

mchange k5 wc inc lwg, atmeans amount(one sd) at(`Average')
matrix mAverage = r(table)
mchange k5 wc inc lwg, atmeans amount(one sd) at(`YngLow')
matrix mYngLow = r(table)
mchange k5 wc inc lwg, atmeans amount(one sd) at(`YngCol')
matrix mYngCol = r(table)
mchange k5 wc inc lwg, atmeans amount(one sd) at(`MidCol')
matrix mMidCol = r(table)
mchange k5 wc inc lwg, atmeans amount(one sd) at(`OldCol')
matrix mOldCol = r(table)

* matrix of effects
matrix colnames mAverage = "Average" "P"
matrix colnames mYngLow = "YngLow" "P"
matrix colnames mYngCol = "YngCol" "P"
matrix colnames mMidCol = "MidCol" "P"
matrix colnames mOldCol = "OldCol" "P"
matrix MER = mAverage[1...,1], mYngLow[1...,1], ///
    mYngCol[1...,1], mMidCol[1...,1], mOldCol[1...,1]
matlist MER, format(%9.2f)

//  tables of predictions

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
estimates store base

mtable, at(wc=(0 1) k5=(0 1 2 3)) atmeans

margins, at(wc=(0 1) k5=(0 1 2 3)) atmeans

mtable, at(wc=0 k5=(0 1 2 3)) at(wc=1 k5=(0 1 2 3)) atmeans ///
    atvars(wc k5) names(columns)

qui mtable, estname(Wife:NoCol)   at(wc=0 k5=(0 1 2 3)) atmeans
qui mtable, estname(Wife:College) at(wc=1 k5=(0 1 2 3)) atmeans  ///
    atvars(_none) right
mtable, estname(Change) dydx(wc) at(k5=(0 1 2 3)) atmeans ///
    atvars(_none) right stats(estimate p) names(columns)
matrix k5wc_global = _mtab_display

qui mtable, estname(Wife:NoCol)   at(wc=0 k5=(0 1 2 3)) atmeans

qui mtable, estname(Wife:College) at(wc=1 k5=(0 1 2 3)) atmeans  ///
    atvars(_none) right

mtable, estname(Change) dydx(wc) at(k5=(0 1 2 3)) atmeans ///
    atvars(_none) right stats(estimate p) names(columns)
matrix k5wc_global = _mtab_display

qui mtable, estname(Wife:NoCol)   at(wc=0 k5=(0 1 2 3)) atmeans
qui mtable, estname(Wife:College) at(wc=1 k5=(0 1 2 3)) atmeans  ///
    atvars(_none) right
mtable, estname(Change) dydx(wc) at(k5=(0 1 2 3)) atmeans ///
    atvars(_none) right stats(estimate p) ///
    title(Note: Other variables held at their means) brief names(columns)

mtable, at(wc=(0 1)) atmeans over(k5)

qui mtable, estname(Wife:NoCol)   at(wc=0) over(k5) atmeans atvars(k5)
qui mtable, estname(Wife:College) at(wc=1) over(k5) atmeans atvars(_none) right
mtable, estname(Change) dydx(wc) over(k5) atmeans ///
    atvars(_none) right stats(est p) names(columns)
matrix k5wc_localk5 = _mtab_display

/* global - local comparison table
matrix mk5 = k5wc_global[1...,1]
matrix mg = k5wc_global
matrix ml = k5wc_localk5
matrix coleq mg = Global
matrix coleq ml = Local
matcol mg keep 2 3 4
matcol ml keep 2 3 4
matmath mdif = mg - ml
matrix coleq mdif = "G - L"
matrix colnames mdif = NoCol    College       Chng
matrix mcompare = mg, ml, mdif
matrix colnames mcompare = NoCol Col Chng NoCol Col Chng NoCol Col Chng
matrix mcompare = mk5, mcompare
local sjis mtable_local_compareEDIT
matlist mcompare, names(col) format(%5.2f)
*/

//  graphs ==================================================================

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
estimates store base

//  marginsplot

margins, at(inc=(0(10)100)) atmeans
marginsplot
graph export rm3ch4-marginsplot-inc.emf, replace

//  mgen plots

capture drop PLT*
mgen, atmeans at(inc=(0(10)100)) stub(PLT) predlabel(Pr(LFP))
list PLTinc PLTpr PLTll PLTul lfp in 1/13, clean
scatter PLTpr PLTinc
graph export rm3ch4-mgen-inc-simple.emf, replace

twoway ///
  (rarea PLTul PLTll PLTinc, color(gs12)) ///
  (connected PLTpr PLTinc, msymbol(i)) ///
 , title("Adjusted Predictions") ///
   caption("Other variables held at their means") ///
   ytitle(Pr(LFP)) ylabel(0(.25)1, grid gmin gmax) legend(off)
graph export rm3ch4-mgen-inc-ciarea.emf, replace

//  multiple predictions: income by agecat

margins agecat, at(inc=(0(10)100)) atmeans
marginsplot, noci
graph export rm3ch4-marginsplot-inc-noci.emf, replace

mgen, atmeans at(inc=(0(10)100) agecat=1) stub(PLT1) predlab(30 to 39)
mgen, atmeans at(inc=(0(10)100) agecat=2) stub(PLT2) predlab(40 to 49)
mgen, atmeans at(inc=(0(10)100) agecat=3) stub(PLT3) predlab(50 plus)
graph twoway connected PLT1pr PLT2pr PLT3pr PLT1inc, ///
	title("Adjusted predictions by age group") ///
    caption("Other variables at their means") ///
    msym(Oh Dh Sh) msiz(*1.5 *1 *1) mcol(black black black) ///
    lpat(solid solid solid) ///
    ytitle("Pr(In Labor Force)") ylab(0(.25)1, grid gmin gmax)
graph export rm3ch4-mgen-incXage.emf, replace

//  overlapping ci's

mgen, atmeans at(inc=(0(5)100) wc=0) stub(PLTWC0) predlab(NoCollege)
mgen, atmeans at(inc=(0(5)100) wc=1) stub(PLTWC1) predlab(College)
twoway ///
   (rarea PLTWC0ul PLTWC0ll PLTWC0inc, col(gs12)) ///
   (rarea PLTWC1ul PLTWC1ll PLTWC0inc, col(gs12)) ///
   (connected PLTWC0pr PLTWC1pr PLTWC1inc, msym(i i)) ///
  , ytitle(Pr(In Labor Force)) legend(order(4 3))
graph export rm3ch4-mgen-incXwc-prob.emf, replace

mgen, dydx(wc) atmeans at(inc=(0(5)100)) stub(PLTWCDC) ///
    predlab(Discrete change in LFP by attending college)
twoway ///
   (rarea PLTWC0ul PLTWC0ll PLTWC0inc, col(gs12)) ///
   (rarea PLTWC1ul PLTWC1ll PLTWC0inc, col(gs12)) ///
   (connected PLTWC0pr PLTWC1pr PLTWC1inc, msym(i i)) ///
  , ytitle(Pr(LFP)) /// xlin(2 39) ///
    legend(order(4 3)) saving(temppr, replace)
graph export rm3ch4-mgen-incXwc-X.emf, replace

twoway ///
   (rarea PLTWCDCll PLTWCDCul PLTWCDCinc, col(gs12)) ///
   (connected PLTWCDCd_pr1 PLTWCDCinc, msym(i i)) ///
  , ytitle(Discrete change) ylin(0) /// xlin(90) ///
    legend(off) saving(tempdc, replace)
graph export rm3ch4-mgen-incXwc-Y.emf, replace

graph combine temppr.gph tempdc.gph , row(2) xcommon ysize(8) xsize(5)
graph export rm3ch4-incXwc-probdc.emf, replace
graph export rm3ch4-graph_incXwc_twoway_probdc.emf, replace

//  quadratics: inc squared

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
qui fitstat, ic save
logit lfp k5 k618 i.agecat i.wc i.hc lwg c.inc##c.inc, nolog
fitstat, ic dif

logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
mgen, predlabel(linear) atmeans at(inc=(0(10)100)) stub(_lin)

logit lfp k5 k618 i.agecat i.wc i.hc lwg c.inc##c.inc, nolog
mgen, predlabel(quadratic) atmeans at(inc=(0(10)100)) stub(_quad)

graph twoway connected _linpr _quadpr _lininc, ///
	title("Comparing income specifications") ///
    caption("Other variables at their means") ///
    msym(Oh Dh) msiz(*1.5 *1) mcol(black black) lpat(solid dash) ///
    ytitle("Pr(In Labor Force)") ///
    ylabel(0(.25)1, grid gmin gmax)
graph export rm3ch4-mgen-incsq.emf, replace

//  local and global means

use binlfp4, clear
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog

* predictions at global means
mgen, at(inc=(0(10)100)) atmeans stub(GLOBAL) predlabel(Global means)
twoway connected GLOBALpr GLOBALinc, ///
    clcol(black) clpat(solid) msym(i) ///
  , ytitle("Pr(In Labor Force)") ylab(0(.25)1, grid gmin gmax) ///
    title("Predictions using global means", position(11))
graph export rm3ch4-mgen-inc-global.emf, replace

* inc grouping bar
local sjis
gen inc10k = trunc(inc/10)
label var inc10k "income in 10K categories"
tabulate inc10k, miss

* local means
mtable, over(inc10k) atmeans stat(est ll ul)
matrix localpred = r(table)
matrix localpred = localpred[1...,"inc".."ul"]
matrix colnames localpred = LOCALinc LOCALpr LOCALll LOCALul
svmat localpred, names(col)
label var LOCALpr  "Local means"
twoway ///
    (connected GLOBALpr GLOBALinc, ///
        clcol(black) clpat(solid) msym(i)) ///
    (connected LOCALpr LOCALinc, ///
        clcol(black) clpat(dash) msym(i)) ///
    , ytitle("Pr(In Labor Force)") ylab(0(.25)1, grid gmin gmax)
graph export rm3ch4-mgen-inc-glocal-local.emf, replace

log close
exit
