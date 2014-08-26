***Originally Created by Alex Miller on 04/22/2014
***filename: PS5.do
***Purpose: Complete problem set 5

clear

cd "/Users/Alex/Documents/Current Courses/Cross Section Analysis/PS5"

webuse set "http://rlhick.people.wm.edu/econ407/data"
webuse deb_munkin_trivedi

**Histograms & Sum-stats
*hist ambexp, normal
*hist lambexp, normal
sum *

**Cross-Tab selection with insurance
tab dambexp ins, nofreq column
*More likely to have positive costs if you don't have insurance,
*leaving out zeroes would bias effect of insurance.


**OLS
reg lambexp ins totchr age female educ blhisp
est store ols

**Recasting for Tobit
quietly sum ambexp if ambexp>0
scalar lbound=r(min)
display lbound-0.00001
display log(lbound-0.00001)
replace lambexp = log(lbound-0.00001) if lambexp==.
*Assumes that all unobserved costs must be below 1 (otherwise Heckman)
*hist lambexp
*histogram lambexp, kdensity kdenopts(gaussian)

**Tobit
tobit lambexp ins totchr age female educ blhisp, ll(-0.00001)
est store tobit
gen sigma = [sigma]_b[_cons]
gen logLike=e(ll)

predict ycond, e(-0.00001,.)
predict probnotcensored, pr(-0.00001,.)
predict y, ystar(-0.00001,.)
predict ystar, xb

gen b = (-0.00001-ystar)/sigma
gen probnotcensored_m = 1-normal(b)
gen inversemills = sigma*(normalden(b)/(1-normal(b)))
gen ycond_m = ystar + inversemills
list ambexp lambexp inversemills in 1/10

**MFX
*E[y|y observed] - truncated mean
mfx compute, predict(e(-0.00001,.)) eyex
est store tobit1
*Prob(observed)
mfx compute, predict(pr(-0.00001,.)) eyex
est store tobit2
*E[y] censored (observed) value
mfx compute, predict(ystar(-0.00001,.)) eyex
est store tobit3
*y* (if we could observe full distribution of y)
mfx, eyex
est store tobit4

esttab tobit1 tobit2 tobit3 tobit4 using tobitmfx.csv, margin se replace

**LL
gen d_censored = (lambexp<=-0.00001)
gen contrib_censored = (1-probnotcensored_m)^(d_censored)
gen contrib_not_censored = ((1/sigma)*normalden((lambexp-ystar)/sigma))^(1-d_censored)
gen Like_i_m = contrib_censored*contrib_not_censored
gen logLike_i_m = log(Like_i_m)
egen logLike_m = sum(logLike_i_m)
sum logLike_m logLike
