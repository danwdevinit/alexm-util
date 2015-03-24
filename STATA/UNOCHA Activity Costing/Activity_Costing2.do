/*
File name: Activity_Costing.do
Created: 2013/09/24 by Alex Miller
Purpose: Test cluster-based activity costing for structural breaks and Granger-causality
Note: Modify base directory prior to running
*/

clear
log using activity_costing2.log, replace
cd "/Users/Alex/Documents/UNOCHA/Activity Costing"

import excel "/Users/Alex/Documents/UNOCHA/Activity Costing/costing_data.xlsx", sheet("data") firstrow
***Generate dummies***
quietly tab country, gen(countrydum)
quietly tab sector, gen(sectordum)
quietly tab period, gen(perioddum)

***Generate log vars***
gen lnreq = log(requirements)
gen lnben = log(beneficiaries)

***Generate time-unique codes***
gen ctry_sec = country + sector
encode ctry_sec, gen(ctryseccode)

***Set time-series***
tsset period ctryseccode

***Pooled-panel OLS controlling variation across countries & across sectors***
reg lnreq lnben countrydum2-countrydum6 sectordum2-sectordum12

testparm countrydum2-countrydum6
testparm sectordum2-sectordum12

***White test for heteroskedasticity***
*generates the residuals
predict e, resid
*generates the predicted values
predict y, xb
*generates yhat squared
gen y2=y^2
*generates the residuals squared
gen e2=e^2
*generates the log of the residuals squared
gen le2=log(e2)
*White test
reg e2 y y2
scalar white = (e(mss)/2)/(e(rss)/e(df_r))
scalar f_white = Ftail(2,e(df_r),white)
scalar list

drop e y2 e2 le2
gen expy = exp(y)


***Pooled-panel OLS controlling variation across countries & across periods***
reg lnreq lnben countrydum2-countrydum6 perioddum2-perioddum8

testparm countrydum2-countrydum6
testparm perioddum2-perioddum8

***Pooled-panel OLS controlling variation across sectors & across periods***
reg lnreq lnben sectordum2-sectordum12 perioddum2-perioddum8

testparm sectordum2-sectordum12
testparm perioddum2-perioddum8

***Level-level test of best fitting equation***
reg requirements beneficiaries countrydum2-countrydum6 sectordum2-sectordum12

testparm countrydum2-countrydum6
testparm sectordum2-sectordum12

***Chow test***
gen chow1 = countrydum2*lnben
gen chow2 = countrydum3*lnben
gen chow3 = countrydum4*lnben
gen chow4 = countrydum5*lnben
gen chow5 = countrydum6*lnben
gen chow6 = sectordum2*lnben
gen chow7 = sectordum3*lnben
gen chow8 = sectordum4*lnben
gen chow9 = sectordum5*lnben
gen chow10 = sectordum6*lnben
gen chow11 = sectordum7*lnben
gen chow12 = sectordum8*lnben
gen chow13 = sectordum9*lnben
gen chow14 = sectordum10*lnben
gen chow15 = sectordum11*lnben
gen chow16 = sectordum11*lnben

label variable chow1  "country==Central African Republic"
label variable chow2  "country==Chad"
label variable chow3  "country==Somalia"
label variable chow4  "country==Yemen"
label variable chow5  "country==oPt"
label variable chow6  "sector==Early recovery"
label variable chow7  "sector==Education"
label variable chow8  "sector==Food"
label variable chow9  "sector==Food and agriculture"
label variable chow10  "sector==Health"
label variable chow11  "sector==Health and nutrition"
label variable chow12  "sector==Multisector"
label variable chow13  "sector==Nutrition"
label variable chow14  "sector==Protection"
label variable chow15  "sector==Shelter/NFI"
label variable chow16  "sector==WASH"

reg lnreq lnben countrydum2-countrydum6 sectordum2-sectordum12 chow1-chow16

estimates store chow

test countrydum2 chow1
test countrydum3 chow2
test countrydum4 chow3
test countrydum5 chow4
test countrydum6 chow5
test sectordum2 chow6
test sectordum3 chow7
test sectordum4 chow8
test sectordum5 chow9
test sectordum6 chow10
test sectordum7 chow11
test sectordum8 chow12
test sectordum9 chow13
test sectordum10 chow14
test sectordum11 chow15
test sectordum12 chow16


predict chowy, xb
gen expchowy = exp(chowy)

***Granger causality test
reg requirements L.requirements L2.requirements L3.requirements beneficiaries L.beneficiaries L2.beneficiaries L3.beneficiaries
test L.requirements L2.requirements L3.requirements
test L.beneficiaries L2.beneficiaries L3.beneficiaries

reg beneficiaries L.beneficiaries L2.beneficiaries L3.beneficiaries requirements L.requirements L2.requirements L3.requirements
test L.requirements L2.requirements L3.requirements
test L.beneficiaries L2.beneficiaries L3.beneficiaries

esttab chow using results.csv, replace se stats(r2 N df_r) legend
log close




