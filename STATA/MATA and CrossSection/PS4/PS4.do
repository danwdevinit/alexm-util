***Originally Created by Alex Miller on 04/01/2014
***filename: PS4.do
***Purpose: Complete problem set 4

clear

cd "/Users/Alex/Documents/Current Courses/Cross Section Analysis/PS4"

webuse set "http://rlhick.people.wm.edu/econ407/data"
webuse southplatte

**A
**Scale Water Bill and Bid by income, log it to fit normal distribution
gen logwaterbilpercent = log(waterbil/hhinc)
gen logbidpercent = log(bid/hhinc)

*hist waterbil
*hist logwaterbilpercent
*hist bid
*hist logbidpercent

reg ypay environ urban logwaterbilpercent logbidpercent
mfx compute
estimates store ols

probit ypay environ urban logwaterbilpercent logbidpercent
mfx compute
estimates store probit
fitstat

logit ypay environ urban logwaterbilpercent logbidpercent
mfx compute
estimates store logit
fitstat

esttab ols probit logit using parameters.csv, replace se stats(r2 N)
esttab ols probit logit using marginal.csv, replace margin se stats(r2 N)

**B
**Lstat for logit
gen xb = _b[environ]*environ + _b[urban]*urban + _b[logwaterbilpercent]*logwaterbilpercent + _b[logbidpercent]*logbidpercent + _b[_cons]*_cons
gen probyes = (exp(xb))/(1+exp(xb))
gen ypayhat = probyes>=0.5
gen countr2 = ypay*(probyes>=0.5)+(1-ypay)*(probyes<0.5)

list xb probyes ypayhat ypay in 1/10

gen lstat_sensitivity = ypay*(probyes>=0.5)
replace lstat_sensitivity = . if ypay==0
gen lstat_specificity = (1-ypay)*(probyes<0.5)
replace lstat_specificity = . if ypay==1
gen lstat_pospredictval = ypay*(probyes>=0.5)
replace lstat_pospredictval = . if probyes<0.5
gen lstat_negpredictval = (1-ypay)*(probyes<0.5)
replace lstat_negpredictval = . if probyes>=0.5
gen lstat_falseposfortrueneg = (1-ypay)*(probyes>=0.5)
replace lstat_falseposfortrueneg = . if ypay==1
gen lstat_falsenegfortruepos = ypay*(probyes<0.5)
replace lstat_falsenegfortruepos = . if ypay==0
gen lstat_falseposforclasspos = (1-ypay)*(probyes>=0.5)
replace lstat_falseposforclasspos = . if probyes<0.5
gen lstat_falsenegforclassneg = ypay*(probyes<0.5)
replace lstat_falsenegforclassneg = . if probyes>=0.5
gen lstat_correctclass = countr2

*ssc install fsum
*for formatted summary stats
fsum lstat*, stats(mean) format(%9.4f)

**E
**$30 higher water bill with original params
gen logwaterbilpercent30 = log((waterbil+30)/hhinc)
gen xb30 = _b[environ]*environ + _b[urban]*urban + _b[logwaterbilpercent]*logwaterbilpercent30 + _b[logbidpercent]*logbidpercent + _b[_cons]*_cons
gen probyes30 = (exp(xb30))/(1+exp(xb30))
gen ypayhat30 = probyes30>=0.5

list ypay probyes probyes30 ypayhat ypayhat30 in 1/10

fsum ypayhat*, stats(mean) format(%9.4f)
