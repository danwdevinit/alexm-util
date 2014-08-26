/*
File name: Aggregate.do
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

***Merge with prior actions and benchmarks and fix commitment amounts
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
save "/Users/Alex/Documents/Current Courses/Quantitative Methods II/ALCID/tempmerge.dta", replace

***Count number of projects, finance, prior actions, and benchmarks per country per year
gen projcount=0
collapse (count) projcount (sum) loanamount=totalamount grantamount PAcount BEcount, by(country year)
save "/Users/Alex/Documents/Current Courses/Quantitative Methods II/ALCID/conditionality.dta", replace

***Count number of projects and finances by major theme

*Economic management
use tempmerge.dta, clear
egen econmanagecount=total(mjtheme1code==1), by(country year)
egen econmanageamt=total(totalamount+grantamount) if mjtheme1code==1, by(country year)
replace econmanageamt=0 if missing(econmanageamt)

*Environment and natural resources manag
egen envirocount=total(mjtheme1code==11), by(country year)
egen enviroamt=total(totalamount+grantamount) if mjtheme1code==11, by(country year)
replace enviroamt=0 if missing(enviroamt)

*Financial and private sector developmen
egen financialcount=total(mjtheme1code==4), by(country year)
egen financialamt=total(totalamount+grantamount) if mjtheme1code==4, by(country year)
replace financialamt=0 if missing(financialamt)

*Human development
egen humandevcount=total(mjtheme1code==8), by(country year)
egen humandevamt=total(totalamount+grantamount) if mjtheme1code==8, by(country year)
replace humandevamt=0 if missing(humandevamt)

*Public sector governance
egen pubgovcount=total(mjtheme1code==2), by(country year)
egen pubgovamt=total(totalamount+grantamount) if mjtheme1code==2, by(country year)
replace pubgovamt=0 if missing(pubgovamt)

*Rule of law
egen rulelawcount=total(mjtheme1code==3), by(country year)
egen rulelawamt=total(totalamount+grantamount) if mjtheme1code==3, by(country year)
replace rulelawamt=0 if missing(rulelawamt)

*Rural development
egen ruraldevcount=total(mjtheme1code==10), by(country year)
egen ruraldevamt=total(totalamount+grantamount) if mjtheme1code==10, by(country year)
replace ruraldevamt=0 if missing(ruraldevamt)

*Social dev/gender/inclusion
egen socialdevcount=total(mjtheme1code==7), by(country year)
egen socialdevamt=total(totalamount+grantamount) if mjtheme1code==7, by(country year)
replace socialdevamt=0 if missing(socialdevamt)

*Social protection and risk management
egen socialprotectcount=total(mjtheme1code==6), by(country year)
egen socialprotectamt=total(totalamount+grantamount) if mjtheme1code==6, by(country year)
replace socialprotectamt=0 if missing(socialprotectamt)

*Trade and integration
egen tradecount=total(mjtheme1code==5), by(country year)
egen tradeamt=total(totalamount+grantamount) if mjtheme1code==5, by(country year)
replace tradeamt=0 if missing(tradeamt)

*Urban development
egen urbandevcount=total(mjtheme1code==9), by(country year)
egen urbandevamt=total(totalamount+grantamount) if mjtheme1code==9, by(country year)
replace urbandevamt=0 if missing(urbandevamt)

*Collapse into country-years
collapse (firstnm) econmanagecount econmanageamt envirocount enviroamt financialcount /*
*/ financialamt humandevcount humandevamt pubgovcount pubgovamt rulelawcount rulelawamt /*
*/ ruraldevcount ruraldevamt socialdevcount socialdevamt socialprotectcount socialprotectamt /*
*/ tradecount tradeamt urbandevcount urbandevamt, by(country year)

***Produce final project dataset
merge 1:1 country year using conditionality.dta
drop _merge
gen avgPA= PAcount/projcount
gen avgBE= BEcount/projcount
save "/Users/Alex/Documents/Current Courses/Quantitative Methods II/ALCID/WBprojects.dta", replace

cd "/Users/Alex/Documents/Current Courses/Quantitative Methods II/Paperwork"
clear

***Create Gleditsch GDP estimates dataset for later
insheet using "/Users/Alex/Documents/Current Courses/Quantitative Methods II/expgdp/expgdp_v5.0.asc", names delimit(" ") clear
rename stateid code
gen realgdp=pop*rgdpch
save "/Users/Alex/Documents/Current Courses/Quantitative Methods II/expgdp/expgdp.dta", replace

***Add Licht and Archigos data source to WB projects
use "/Users/Alex/Documents/Current Courses/Quantitative Methods II/Licht/Licht_edited.dta"
rename ccname country
merge m:1 country year using "/Users/Alex/Documents/Current Courses/Quantitative Methods II/ALCID/WBprojects.dta"

*No WB record indicates no projects
replace econmanagecount=0 if missing(econmanagecount) & _merge==1replace econmanageamt=0 if missing(econmanageamt) & _merge==1replace envirocount=0 if missing(envirocount) & _merge==1replace enviroamt=0 if missing(enviroamt) & _merge==1replace financialcount=0 if missing(financialcount) & _merge==1replace financialamt=0 if missing(financialamt) & _merge==1replace humandevcount=0 if missing(humandevcount) & _merge==1replace humandevamt=0 if missing(humandevamt) & _merge==1replace pubgovcount=0 if missing(pubgovcount) & _merge==1replace pubgovamt=0 if missing(pubgovamt) & _merge==1replace rulelawcount=0 if missing(rulelawcount) & _merge==1replace rulelawamt=0 if missing(rulelawamt) & _merge==1replace ruraldevcount=0 if missing(ruraldevcount) & _merge==1replace ruraldevamt=0 if missing(ruraldevamt) & _merge==1replace socialdevcount=0 if missing(socialdevcount) & _merge==1replace socialdevamt=0 if missing(socialdevamt) & _merge==1replace socialprotectcount=0 if missing(socialprotectcount) & _merge==1replace socialprotectamt=0 if missing(socialprotectamt) & _merge==1replace tradecount=0 if missing(tradecount) & _merge==1replace tradeamt=0 if missing(tradeamt) & _merge==1replace urbandevcount=0 if missing(urbandevcount) & _merge==1replace urbandevamt=0 if missing(urbandevamt) & _merge==1replace projcount=0 if missing(projcount) & _merge==1replace loanamount=0 if missing(loanamount) & _merge==1replace grantamount=0 if missing(grantamount) & _merge==1replace PAcount=0 if missing(PAcount) & _merge==1replace BEcount=0 if missing(BEcount) & _merge==1replace avgPA=0 if missing(avgPA) & _merge==1replace avgBE=0 if missing(avgBE) & _merge==1
drop if _merge==2
drop _merge

***Merge GDP data
merge m:1 code year using "/Users/Alex/Documents/Current Courses/Quantitative Methods II/expgdp/expgdp.dta"
drop if _merge==2
drop _merge origin statenum

***Generate relevant vars
*For loans
gen lnetwbloan= log(loanamount/realgdp)
gen tlnetwbloan= lnt*lnetwbloan
gen wbiglnetwbloan = Wbig*lnetwbloan
gen twbiglnetwbloan = wbiglnetwbloan*lnt
gen lnetwbloanPA = lnetwbloan*PAcount
gen lnetwbloanBE = lnetwbloan*BEcount
gen netgetwbloan = .
replace netgetwbloan=0 if (loanamount)==0
replace netgetwbloan=1 if (loanamount)>0
*For grants
gen lnetwbgrant= log(grantamount/realgdp)
gen tlnetwbgrant= lnt*lnetwbgrant
gen wbiglnetwbgrant = Wbig*lnetwbgrant
gen twbiglnetwbgrant = wbiglnetwbgrant*lnt
gen lnetwbgrantPA = lnetwbgrant*PAcount
gen lnetwbgrantBE = lnetwbgrant*BEcount
gen netgetwbgrant = .
replace netgetwbgrant=0 if (grantamount)==0
replace netgetwbgrant=1 if (grantamount)>0
*Total WB projects
gen lnetwbtot= log((grantamount+loanamount)/realgdp)
gen tlnetwbtot= lnt*lnetwbtot
gen wbiglnetwbtot = Wbig*lnetwbtot
gen twbiglnetwbtot = wbiglnetwbtot*lnt
gen lnetwbtotPA = lnetwbtot*PAcount
gen lnetwbtotBE = lnetwbtot*BEcount
gen netgetwbtot = .
replace netgetwbtot=0 if (grantamount+loanamount)==0
replace netgetwbtot=1 if (grantamount+loanamount)>0
gen grantpct = grantamount/(grantamount+loanamount)

*Gen lagged variables with atypical data structure
sort country year
by country: gen netgetwbloan_lag= netgetwbloan[_n-1] if year==year[_n-1]+1
by country: gen netgetwbgrant_lag= netgetwbgrant[_n-1] if year==year[_n-1]+1
by country: gen netgetwbtot_lag= netgetwbtot[_n-1] if year==year[_n-1]+1
harmby netgetwbloan_lag netgetwbgrant_lag netgetwbtot_lag, by(country year) /* Had to download this tool, it harmonizes missing values */

***Generate probability of failure from Licht
#delimit;
probit wfail2 lognet3 tlognet3 wbiglognet3 twbiglognet3 Wbig lnt growth_lag lntrade_lag civlev 
	SOUTHAM SUBAFRICA SOUTHASIA, cluster(leadid);
#delimit cr
predict double pfail, p
lab var pfail "Probability of Failure from wfail2 Equation with W dummy"

***Total OLS regression
reg pfail lnetwbtot grantpct PAcount BEcount lnetwbtotPA lnetwbtotBE tlnetwbtot wbiglnetwbtot twbiglnetwbtot Wbig lnt growth_lag lntrade_lag intcivconflict, robust
estimates store m1
***Total 2SLS regression
ivregress 2sls pfail PAcount BEcount lnetwbtotPA lnetwbtotBE (lnetwbtot= netgetwbtot_lag popgrowth_lag lnpop ldonorexp_lag W formercol strongally intcivconflict oil) tlnetwbtot wbiglnetwbtot twbiglnetwbtot Wbig lnt growth_lag lntrade_lag intcivconflict, robust
estimates store m2

***Loan OLS regression
reg pfail lnetwbloan PAcount BEcount lnetwbloanPA lnetwbloanBE tlnetwbloan wbiglnetwbloan twbiglnetwbloan Wbig lnt growth_lag lntrade_lag intcivconflict, robust
estimates store m3
***Loan 2SLS regression
ivregress 2sls pfail PAcount BEcount lnetwbloanPA lnetwbloanBE (lnetwbloan= netgetwbloan_lag popgrowth_lag lnpop ldonorexp_lag W formercol strongally intcivconflict oil) tlnetwbloan wbiglnetwbloan twbiglnetwbloan Wbig lnt growth_lag lntrade_lag intcivconflict, robust
estimates store m4

***Grant OLS regression
reg pfail lnetwbgrant PAcount BEcount lnetwbgrantPA lnetwbgrantBE tlnetwbgrant wbiglnetwbgrant twbiglnetwbgrant Wbig lnt growth_lag lntrade_lag intcivconflict, robust
estimates store m5
***Grant 2SLS regression
ivregress 2sls pfail PAcount BEcount lnetwbgrantPA lnetwbgrantBE (lnetwbgrant= netgetwbgrant_lag popgrowth_lag lnpop ldonorexp_lag W formercol strongally intcivconflict oil) tlnetwbgrant wbiglnetwbgrant twbiglnetwbgrant Wbig lnt growth_lag lntrade_lag intcivconflict, robust
estimates store m6

esttab m1 m2 m3 m4 m5 m6 using table.csv, stat(r2 N) se

