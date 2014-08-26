/*
File name: finalpaper2.do
Created: 2013/04/09 by Alex Miller
Purpose: Construct dataset and examine the relationship between conditionality, 
sectors, and accountability on the viability of authoritarian regimes
Note: Modify base directory prior to running
*/
cd "/Users/Alex/Documents/Current Courses/Quantitative Methods II/ALCID/"

***Import prior action sheet
import excel "/Users/Alex/Documents/Current Courses/Quantitative Methods II/ALCID/DPAD-FY12.xlsx", sheet("All Prior Actions") firstrow clear

*Fix an error
replace Country="Malawi" if ProjectID=="P117238"

***Count prior actions per project
gen PAcount = 0
collapse (count) PAcount, by(ProjectID)
save "/Users/Alex/Documents/Current Courses/Quantitative Methods II/ALCID/PAcount.dta", replace

***Import benchmark sheet
import excel "/Users/Alex/Documents/Current Courses/Quantitative Methods II/ALCID/DPAD-FY12.xlsx", sheet("All Benchmarks") firstrow clear

***Count benchmarks per project
gen BEcount = 0
collapse (count) BEcount, by(ProjectID)

***Merge with prior action sheet
merge 1:1 ProjectID using PAcount.dta
drop _merge
rename ProjectID projid
label variable BEcount "Number of informal WB benchmarks"
label variable PAcount "Number of formal WB prior actions"
save "/Users/Alex/Documents/Current Courses/Quantitative Methods II/ALCID/PriorActionBenchmark.dta", replace
clear

***Import individual project data
import excel "/Users/Alex/Documents/Current Courses/Quantitative Methods II/ALCID/projectdata.xlsx", sheet("Sheet1") firstrow

***Merge with prior actions and benchmarks and adjust commitment amounts
merge 1:1 projid using PriorActionBenchmark.dta
drop _merge
replace PAcount=0 if missing(PAcount)
replace BEcount=0 if missing(BEcount)
drop if totalamount==0 & grantamount==0 & PAcount==0 & BEcount==0
gen year=year(approvedate)
replace totalamount=totalamount*1000000
replace grantamount=grantamount*1000000
*Deflate amounts
merge m:1 year using usddeflator.dta
replace totalamount=. if _merge==1
replace grantamount=. if _merge==1
drop _merge
*Making base year 2000 based on calculated GDP price index
replace totalamount=(totalamount*466.2314399)/priceindex
replace grantamount=(grantamount*466.2314399)/priceindex

***Count number of projects, finance, prior actions, and benchmarks per country per year
gen projcount=0
collapse (count) projcount (sum) loanamount=totalamount grantamount PAcount BEcount, by(country year)
save "/Users/Alex/Documents/Current Courses/Quantitative Methods II/ALCID/conditionality.dta", replace

cd "/Users/Alex/Documents/Current Courses/Quantitative Methods II/Paperwork"
clear

***Create Gleditsch GDP estimates dataset for later
insheet using "/Users/Alex/Documents/Current Courses/Quantitative Methods II/expgdp/expgdp_v5.0.asc", names delimit(" ") clear
rename stateid code
gen realgdp=pop*rgdpch
save "/Users/Alex/Documents/Current Courses/Quantitative Methods II/expgdp/expgdp.dta", replace

***Add Licht and Archigos data source to WB projects
use "/Users/Alex/Documents/Current Courses/Quantitative Methods II/Licht/Licht_edited.dta", clear
rename ccname country
merge m:1 country year using "/Users/Alex/Documents/Current Courses/Quantitative Methods II/ALCID/conditionality.dta"

*No WB record indicates no projects
replace projcount=0 if missing(projcount) & _merge==1replace loanamount=0 if missing(loanamount) & _merge==1replace grantamount=0 if missing(grantamount) & _merge==1replace PAcount=0 if missing(PAcount) & _merge==1replace BEcount=0 if missing(BEcount) & _merge==1
drop if _merge==2
drop _merge

***Merge GDP data
merge m:1 code year using "/Users/Alex/Documents/Current Courses/Quantitative Methods II/expgdp/expgdp.dta"
drop if _merge==2
drop _merge origin statenum

***Merge Polity IV
merge m:1 code year using "/Users/Alex/Documents/Current Courses/Quantitative Methods II/Polity/polity.dta"
drop if _merge==2
drop _merge dup

***New variables
gen wbpergdp = (grantamount+loanamount)/realgdp
gen lntwbpergdp = lnt*wbpergdp
gen wbigwbpergdp = Wbig*wbpergdp
gen twbigwbpergdp = lntwbpergdp*Wbig
gen getwb = 0
replace getwb=1 if (grantamount+loanamount)>0
gen grantpct = 0
replace grantpct= grantamount/(grantamount+loanamount) if grantamount>0 | loanamount>0
gen dem = .
replace dem = 0 if polity2<0
replace dem=1 if polity2>=0
gen demPAcount = PAcount*dem
gen demBEcount= BEcount*dem
gen demgrantpct = grantpct*dem
gen demwbpergdp = dem*wbpergdp
gen tdemwbpergdp=lnt*demwbpergdp

*Gen lagged variables with atypical data structure
sort country year
by country: gen getwbl1= getwb[_n-1] if year==year[_n-1]+1
harmby getwbl1, by(country year) /* Had to download this tool, it harmonizes missing values */
by country: gen getwbl2= getwbl1[_n-1] if year==year[_n-1]+1
harmby getwbl2, by(country year)
by country: gen getwbl3= getwbl2[_n-1] if year==year[_n-1]+1
harmby getwbl3, by(country year)

***LPM with heteroskedasticity test
reg wfail2 wbpergdp PAcount BEcount grantpct demPAcount demBEcount demgrantpct dem lntwbpergdp demwbpergdp tdemwbpergdp lnt growth_lag lntrade_lag civlev SOUTHAM SUBAFRICA SOUTHASIA
estimates store lpm
predict elpm, resid
predict ylpm, xb
gen ylpm2=ylpm^2
gen elpm2=elpm^2
reg elpm2 ylpm ylpm2
scalar whitelpm = (e(mss)/2)/(e(rss)/e(df_r))
scalar f_whitelpm = Ftail(2,e(df_r),whitelpm)
scalar list

***Generate instrumental variable
probit wfail2 wbpergdp PAcount BEcount grantpct demPAcount demBEcount demgrantpct dem lntwbpergdp demwbpergdp tdemwbpergdp lnt growth_lag lntrade_lag civlev SOUTHAM SUBAFRICA SOUTHASIA, cluster(leadid)
mfx
estimates store prob1
predict double pfail, p
predict double se, stdp
predict double xb, xb
gen Z = xb/se
drop if Z==.
gen correct=.
gen correcttrue=.
gen correctfalse=.
replace correct=1 if pfail>=.5 & wfail2==1
replace correct=1 if pfail<.5 & wfail2==0
replace correct=0 if pfail<.5 & wfail2==1
replace correct=0 if pfail>=.5 & wfail2==0
replace correcttrue=1 if pfail>=.5 & wfail2==1
replace correcttrue=0 if pfail<.5 & wfail2==1
replace correctfalse=1 if pfail<.5 & wfail2==0
replace correctfalse=0 if pfail>=.5 & wfail2==0
sum correct correcttrue correctfalse

***Test selection model
probit getwb Z getwbl1 getwbl2 getwbl3 popgrowth_lag ldonorexp_lag lnpop growth_lag polity2 W formercol strongally oil intcivconflict, cluster(leadid)
mfx
estimates store prob2
predict double pget, p
gen correctsel=.
gen correctseltrue=.
gen correctselfalse=.
replace correctsel=1 if pget>=.5 & getwb==1
replace correctsel=1 if pget<.5 & getwb==0
replace correctsel=0 if pget<.5 & getwb==1
replace correctsel=0 if pget>=.5 & getwb==0
replace correctseltrue=1 if pget>=.5 & getwb==1
replace correctseltrue=0 if pget<.5 & getwb==1
replace correctselfalse=1 if pget<.5 & getwb==0
replace correctselfalse=0 if pget>=.5 & getwb==0
sum correctsel correctseltrue correctselfalse

***Probit with selection model
#delimit;
heckprob wfail2 wbpergdp PAcount BEcount grantpct demPAcount demBEcount demgrantpct dem lntwbpergdp demwbpergdp tdemwbpergdp lnt growth_lag lntrade_lag civlev 
	SOUTHAM SUBAFRICA SOUTHASIA, 
	sel(getwb = Z getwbl1 getwbl2 getwbl3 popgrowth_lag ldonorexp_lag lnpop growth_lag polity2 W formercol strongally oil intcivconflict) 
	cluster(leadid) difficult;
#delimit cr
predict double pfail2, p
gen correct2=.
gen correcttrue2=.
gen correctfalse2=.
replace correct2=1 if pfail2>=.5 & wfail2==1
replace correct2=1 if pfail2<.5 & wfail2==0
replace correct2=0 if pfail2<.5 & wfail2==1
replace correct2=0 if pfail2>=.5 & wfail2==0
replace correcttrue2=1 if pfail2>=.5 & wfail2==1
replace correcttrue2=0 if pfail2<.5 & wfail2==1
replace correctfalse2=1 if pfail2<.5 & wfail2==0
replace correctfalse2=0 if pfail2>=.5 & wfail2==0
sum correct2 correcttrue2 correctfalse2
test PAcount BEcount grantpct demPAcount demBEcount demgrantpct
mfx
estimates store prob3
sum pget if netgetlag==0
sum pget if netgetlag==1
sum pget if netgetlag==0&W<.75
sum pget if netgetlag==0&W>=.75
sum pfail2 if polity2<0
sum pfail2 if polity2>=0
margins, dydx(*)  atmeans post
*Marginal Partial Effect for BEcount, democracies
lincom BEcount+demBEcount
*Marginal Partial Effect for wbpergdp, democracies, mean tenure
lincom wbpergdp + demwbpergdp + (7.23)*lntwbpergdp + (7.23)*tdemwbpergdp
*Marginal Partial Effect for wbpergdp, autocracies, mean tenure
lincom wbpergdp + (7.23)*lntwbpergdp
*Marginal partial effect for PAcount, democracies
lincom PAcount+demPAcount
*Marginal partial effect for grantpct, democracies
lincom grantpct+demgrantpct



esttab lpm using lpm.csv, replace se stats(N r2)
scalar list
esttab prob1 prob3 using compareprobits.csv, replace se stats (N)
sum correct correcttrue correctfalse correctsel correctseltrue correctselfalse correct2 correcttrue2 correctfalse2
esttab prob3 prob2 using probit3margins.csv, replace se margin
sum correct2 correcttrue2 correctfalse2

