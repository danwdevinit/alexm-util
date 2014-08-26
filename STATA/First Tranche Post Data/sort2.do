***Originally Cxreated: 12/4/2012 by Alex Miller
***Purpose: Import, Sort, prepare, and analyze Official Chinese Development Finance data
***Note: Change base directory prior to running
cd "/Users/Alex/Documents/AidData/MBDC/Spring 2013/Blog Post/Blog_Post"

clear

import excel "aiddata_china_1_0.xlsx", sheet("1) Official Flows") firstrow

drop if recipient_condensed == "Africa, regional"

keep year crs_sector_name flow usd_defl recipient_condensed recipient_iso3
rename recipient_condensed recipient
rename recipient_iso3 code

***Add SSD to SDN***
replace recipient="Sudan" if code=="SSD"
replace code="SDN" if code=="SSD"

***Intermediate labels***
label variable flow "Transaction type"

***Calculate Totals***
sort code year
collapse(sum) usd_defl, by(code year)
drop if usd_defl<=0
rename usd_defl cof

***Merge freedomhouse***
merge 1:1 code year using freedomhouse.dta
gen freecat = 2
replace freecat = 1 if avgfree<=2.5
replace freecat=3 if avgfree>=5.5
rename avgfree freedomhouse
drop _merge

***Merge hdi***
merge 1:1 code year using hdi.dta
drop _merge
egen id = group(code)
tsset id year
drop hdipercent
gen hdipercent = hdichange/L.hdi
drop id

***Merge world bank vars***
merge 1:1 code year using worldbank.dta
drop _merge

***Merge wto imports***
merge 1:1 code year using imports.dta
drop _merge

***Merge AidData US ODA***
merge 1:1 code year using USODA.dta
drop _merge
rename commitment_amount_usd_constant usoda

***Merge AidData DAC ODA***
merge 1:1 code year using DAC.dta
drop _merge

***Merge WB Gov't Effectiveness Measure***
sort code
merge m:1 code using govteffect.dta 
drop _merge

***Merge Polity***
sort code year
merge 1:1 code year using polity.dta
drop _merge

***Merge Arms***
sort code year
merge 1:1 code year using arms.dta
drop _merge

*Codebook states values in millions
replace arms = arms*1000000
replace armslag1 = armslag1*1000000

***Merge UNGA CHINA votes***
sort code year
merge 1:1 code year using ungavote.dta
drop _merge
rename agree3unga chinaagreeunga

***Merge UNGA USA votes***
sort code year
merge 1:1 code year using ungavoteUSA.dta
drop _merge

***Drop superfluous countries***
drop if african!=1
drop african

***Generate Taiwan Support var***
gen taiwansupport = 0
replace taiwansupport = 1 if code=="LBR" & year<=2003
replace taiwansupport = 1 if code=="MWI" & year<=2008
replace taiwansupport = 1 if code=="SEN" & year<=2005
replace taiwansupport = 1 if code=="TCD" & year<=2006
replace taiwansupport = 1 if code=="BFA" & year>=2000
replace taiwansupport = 1 if code=="GMB" & year>=2000
replace taiwansupport = 1 if code=="STP" & year>=2000
replace taiwansupport = 1 if code=="SWZ" & year>=2000
*Liberia 2000-2003
*Malawi 2000-2008
*Senegal 2000-2005
*Chad 2000-2006
*Burkina Faso 2000-2011
*The Gambia 2000-2011
*Sao tome 2000-2011
*Swaziland 2000-2011

***Generate Egypt Dummy***
gen egypt = 0
replace egypt = 1 if code=="EGY"

***Generate Region Dummy***
gen subsaharan = 1
replace subsaharan = 0 if (code=="DZA" | code=="TCD" | code=="EGY" | code=="MLI" | code=="LBY" | code=="MRT" | code=="MAR" | code=="NER" | code=="SDN" | code=="TUN" | code=="SOM")

***Add Chinese Imports and exports
merge 1:1 code year using chnexports.dta
drop if _merge==2
drop _merge

***Deflate gdp, arms, import, and export to 2009***
merge m:1 code using deflator.dta
drop if _merge!=3
drop _merge

*Constant 2000 gdp to constant 2009
gen defgdp = gdp*deflate2001*deflate2002*deflate2003*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009
drop gdp
rename defgdp gdp

*Constant 1990 Arm imports to constant 2009
gen defarms = arms*deflate1991*deflate1992*deflate1993*deflate1994*deflate1995*deflate1996*deflate1997*deflate1998*deflate1999*deflate2000*deflate2001*deflate2002*deflate2003*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009
gen defarmslag1 = armslag1*deflate1991*deflate1992*deflate1993*deflate1994*deflate1995*deflate1996*deflate1997*deflate1998*deflate1999*deflate2000*deflate2001*deflate2002*deflate2003*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009
drop arms armslag1
rename defarms arms
rename defarmslag1 armslag1

*Current priced imports and exports to constant 2009
gen defimport = .
replace defimport = import*deflate2001*deflate2002*deflate2003*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2000
replace defimport = import*deflate2002*deflate2003*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2001
replace defimport = import*deflate2003*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2002
replace defimport = import*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2003
replace defimport = import*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2004
replace defimport = import*deflate2006*deflate2007*deflate2008*deflate2009 if year==2005
replace defimport = import*deflate2007*deflate2008*deflate2009 if year==2006
replace defimport = import*deflate2008*deflate2009 if year==2007
replace defimport = import*deflate2009 if year==2008
replace defimport = import if year==2009
replace defimport = import/deflate2010 if year==2010
replace defimport = (import/deflate2011)/deflate2010 if year==2011
drop import
rename defimport import

gen defexport = .
replace defexport = export*deflate2001*deflate2002*deflate2003*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2000
replace defexport = export*deflate2002*deflate2003*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2001
replace defexport = export*deflate2003*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2002
replace defexport = export*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2003
replace defexport = export*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2004
replace defexport = export*deflate2006*deflate2007*deflate2008*deflate2009 if year==2005
replace defexport = export*deflate2007*deflate2008*deflate2009 if year==2006
replace defexport = export*deflate2008*deflate2009 if year==2007
replace defexport = export*deflate2009 if year==2008
replace defexport = export if year==2009
replace defexport = export/deflate2010 if year==2010
replace defexport = (export/deflate2011)/deflate2010 if year==2011
drop export
rename defexport export

*Current priced Chinese exports
gen defchnexportsto = .
replace defchnexportsto = chnexportsto*deflate2001*deflate2002*deflate2003*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2000
replace defchnexportsto = chnexportsto*deflate2002*deflate2003*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2001
replace defchnexportsto = chnexportsto*deflate2003*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2002
replace defchnexportsto = chnexportsto*deflate2004*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2003
replace defchnexportsto = chnexportsto*deflate2005*deflate2006*deflate2007*deflate2008*deflate2009 if year==2004
replace defchnexportsto = chnexportsto*deflate2006*deflate2007*deflate2008*deflate2009 if year==2005
replace defchnexportsto = chnexportsto*deflate2007*deflate2008*deflate2009 if year==2006
replace defchnexportsto = chnexportsto*deflate2008*deflate2009 if year==2007
replace defchnexportsto = chnexportsto*deflate2009 if year==2008
replace defchnexportsto = chnexportsto if year==2009
replace defchnexportsto = chnexportsto/deflate2010 if year==2010
replace defchnexportsto = (chnexportsto/deflate2011)/deflate2010 if year==2011
drop chnexportsto
rename defchnexportsto chnexportsto


drop deflate1989-deflate2011

***Calculate total natural resource rents from percent gdp***
gen totalnaturalrent = naturalrent*gdp
rename naturalrent naturalrentpercent
rename totalnaturalrent naturalrent

***Replace certain missing data with 0***
replace cof=0 if missing(cof)
replace usoda=0 if missing(usoda) & year!=2011
replace arms=0 if missing(arms)
replace armslag1=0 if missing(armslag1)

***Generate additional vars***
egen id = group(code)
tsset id year
gen logpop = log(pop)
gen cofpercentgdp = cof/gdp
gen cofgdpfree = cofpercentgdp*freedomhouse
gen cofgdppolity = cofpercentgdp*polity
gen usodapercentgdp = usoda/gdp
gen usodagdpfree = usodapercentgdp*freedomhouse
gen usodagdppolity = usodapercentgdp*polity
gen dacpercentgdp = dac/gdp
gen dacgdpfree = dacpercentgdp*freedomhouse
gen dacgdppolity = dacpercentgdp*polity
gen armspercent = arms/import
gen armspercentlag1 =armslag1/import
gen importpercentgdp = import/gdp
gen exportpercentgdp = export/gdp
gen getaid = 0
replace getaid = 1 if cof>0
gen lnchnexportsto = log(chnexportsto)
gen lncof = log(cof)
gen lncofpolity = lncof*polity
gen lnnaturalrent = log(naturalrent)
gen lngdp = log(gdp)
gen gdpgrowth = (gdp-L.gdp)/L.gdp

***Generate period dummy***
quietly tabulate year, gen(per)

***codebook labels***
label variable year "Commitment year"
label variable recipient "Recipient of Chinese Official Finance"
label variable cof "Chinese Official Finance, constant 2009 USD"
label variable code "Recipient's ISO3 code"
label variable freedomhouse "Freedom House's average freedom score for recipient in commitment year"
label variable polrights "Freedom House Political Rights score for recipient"
label variable civlib "Freedom House Civil Liberty score for recipient"
label variable freecat "Freedom House categorical variable where 1:free, 2:partly free, 3:not free"
label variable hdi "UN's Human development index in commitment year - Values for 1999 and 2001-2004 have been interpolated"
label variable hdichange "Change in HDI from previous year"
label variable hdipercent "Percent change in HDI from previous year"
label variable businessease "Doing Business' Ease of Doing Business Ranking (1-185) for entire period applied to each commitment year"
label variable gdp "Gross Domestic Product in commitment year in constant 2009 USD from World Bank"
label variable inflation "Inflation, GDP deflator (annual %) from World Bank"
label variable pop "Population, total from World Bank"
label variable naturalrent "Total natural resources rents from World Bank"
label variable naturalrentpercent "Total natural resources rents (% GDP) from World Bank"
label variable usoda "Total United States official development assistance in commitment year from AidData"
label variable dac "Total DAC Official Development assistance from AidData"
label variable govtefft "Composit index of govt effectiveness from World Bank Governance Indicators including stability, quality of bureaucracy, etc. Range 2.5 to -2.5, with 0 being world average"
label variable polity2 "Policy IV 2 score for time-series data in commitment year"
label variable chinaagreeunga "UN General Assembly agreement with China based on votes in commitment year. Abstaining counts as half an agreement. From Anton Strezhnev; Erik Voeten"
label variable taiwansupport "Dummy for supporting Taiwan in commitment year"
label variable subsaharan "Regional dummy by geography"
label variable arms "Total arm imports from SIPRI, constant 2009 USD"
label variable armslag1 "Total arm imports lagged one period, constant 2009 USD"
label variable armspercent "Arm imports as a percent of total imports"
label variable armspercentlag1 "Total bilateral arm imports from SIPRI lagged one period as a percent of total annual imports in commitment year"
label variable logpop "The natural log of total population"
label variable cofgdpfree "COF/GDP and freedom house score interaction effect"
label variable cofgdppolity "COF/GDP and polity IV interaction effect"
label variable export "Annual exports in constant 2009 USD"
label variable import "Annual imports in constant 2009 USD"
label variable cofpercentgdp "Chinese Official Finance as percent of GDP"
label variable usodapercentgdp "US ODA/GDP"
label variable usodagdpfree "US ODA/GDP and Freedom House interaction effect"
label variable usodagdppolity "US ODA/GDP and Polity IV interaction effect"
label variable dacpercentgdp "DAC ODA/GDP"
label variable dacgdpfree "DAC ODA/GDP and Freedom House interaction effect"
label variable dacgdppolity "DAC ODA/GDP and Polity IV interaction effect"
label variable usaagreeunga "UN General Assembly agreement with USA based on votes in commitment year. Abstaining counts as half an agreement. From Anton Strezhnev; Erik Voeten"
label variable egypt "1 if Egypt"
label variable getaid "Received Chinese Official Finance"
label variable chnexportsto "Net value of total commodity exports from China to recipient country from UN comtrade"
label variable lnchnexportsto "Log of net value of commodity exports from China"

***Time Series Setup***
tsset id year

***OLS regression
reg hdichange hdi naturalrentpercent inflation subsaharan polity2 cofgdppolity cofpercentgdp per2-per12, robust
estimates store OLS

***OLS regression without HDI interpolation
reg hdichange hdi naturalrentpercent inflation subsaharan polity2 cofgdppolity cofpercentgdp per8-per12 if year>2005, robust
estimates store OLS2

***2SLS Setup; aid affecting hdichange this year***
ivregress 2sls hdichange hdi naturalrentpercent inflation subsaharan polity2 cofgdppolity (cofpercentgdp = chinaagreeunga taiwan logpop lnchnexportsto per2-per12) per2-per12, first robust
estimates store TSLS
*See coefficients and 95% conf. intervals
lincom cofpercentgdp - 9*cofgdppolity
lincom cofpercentgdp
lincom cofpercentgdp + 10*cofgdppolity
*Test fixed time effect joint sig.
testparm per2-per12

***2SLS Setup; aid affecting hdichange this year without HDI interpolation***
ivregress 2sls hdichange hdi naturalrentpercent inflation subsaharan polity2 cofgdppolity (cofpercentgdp = chinaagreeunga taiwan logpop lnchnexportsto per8-per12) per8-per12 if year>2005, first robust
estimates store TSLS2
*See coefficients and 95% conf. intervals
lincom cofpercentgdp - 9*cofgdppolity
lincom cofpercentgdp
lincom cofpercentgdp + 10*cofgdppolity
*Test fixed time effect joint sig.
testparm per8-per12

esttab OLS OLS2 TSLS TSLS2 using finaltable.csv, drop(per*) replace se stat(N r2) star(* .10 ** .05 *** .01)
