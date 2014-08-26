***Originally Created by Alex Miller on 12/8/2012
***filename: finalpaper.do
***Purpose: Merge and analyze data necessary for final report
***Note: forciblydisplaced.dta and InternatDevelop_v08-22-12.dta must be in the working folder

***Make visualization 1 data***
clear

log using governancedisplaced.log, replace

use forciblydisplaced.dta
gen tdp64=(source64*1000)+(idp64*1000)
gen tdp65=(source65*1000)+(idp65*1000)
gen tdp66=(source66*1000)+(idp66*1000)
gen tdp67=(source67*1000)+(idp67*1000)
gen tdp68=(source68*1000)+(idp68*1000)
gen tdp69=(source69*1000)+(idp69*1000)
gen tdp70=(source70*1000)+(idp70*1000)
gen tdp71=(source71*1000)+(idp71*1000)
gen tdp72=(source72*1000)+(idp72*1000)
gen tdp73=(source73*1000)+(idp73*1000)
gen tdp74=(source74*1000)+(idp74*1000)
gen tdp75=(source75*1000)+(idp75*1000)
gen tdp76=(source76*1000)+(idp76*1000)
gen tdp77=(source77*1000)+(idp77*1000)
gen tdp78=(source78*1000)+(idp78*1000)
gen tdp79=(source79*1000)+(idp79*1000)
gen tdp80=(source80*1000)+(idp80*1000)
gen tdp81=(source81*1000)+(idp81*1000)
gen tdp82=(source82*1000)+(idp82*1000)
gen tdp83=(source83*1000)+(idp83*1000)
gen tdp84=(source84*1000)+(idp84*1000)
gen tdp85=(source85*1000)+(idp85*1000)
gen tdp86=(source86*1000)+(idp86*1000)
gen tdp87=(source87*1000)+(idp87*1000)
gen tdp88=(source88*1000)+(idp88*1000)
gen tdp89=(source89*1000)+(idp89*1000)
gen tdp90=(source90*1000)+(idp90*1000)
gen tdp91=(source91*1000)+(idp91*1000)
gen tdp92=(source92*1000)+(idp92*1000)
gen tdp93=(source93*1000)+(idp93*1000)
gen tdp94=(source94*1000)+(idp94*1000)
gen tdp95=(source95*1000)+(idp95*1000)
gen tdp96=(source96*1000)+(idp96*1000)
gen tdp97=(source97*1000)+(idp97*1000)
gen tdp98=(source98*1000)+(idp98*1000)
gen tdp99=(source99*1000)+(idp99*1000)
gen tdp00=(source00*1000)+(idp00*1000)
gen tdp01=(source01*1000)+(idp01*1000)
gen tdp02=(source02*1000)+(idp02*1000)
gen tdp03=(source03*1000)+(idp03*1000)
gen tdp04=(source04*1000)+(idp04*1000)
gen tdp05=(source05*1000)+(idp05*1000)
gen tdp06=(source06*1000)+(idp06*1000)
gen tdp07=(source07*1000)+(idp07*1000)
gen tdp08=(source08*1000)+(idp08*1000)

collapse(sum) tdp64 tdp65 tdp66 tdp67 tdp68 tdp69 tdp70 tdp71 tdp72 tdp73 tdp74 tdp75 /*
*/ tdp76 tdp77 tdp78 tdp79 tdp80 tdp81 tdp82 tdp83 tdp84 tdp85 tdp86 tdp87 tdp88 tdp89 /*
*/ tdp90 tdp91 tdp92 tdp93 tdp94 tdp95 tdp96 tdp97 tdp98 tdp99 tdp00 tdp01 tdp02 tdp03 /*
*/ tdp04 tdp05 tdp06 tdp07 tdp08 

format %12.0g _all

***Start making data for analysis***

clear

use "InternatDevelop_v08-22-12.dta"

***Merge forcible displacement dataset***
merge 1:1 countryname using forciblydisplaced.dta
sort _merge

***Drop (mostly non-existant) Countries and clean vars***
drop if _merge==2
keep countryname source00 idp00 host00 region fhpolrts00 ticorrind00

***Calculate displaced totals (codebook says original values are in 1000s)***
rename source00 sourcethou00
rename idp00 idpthou00
rename host00 hostthou00
gen source00 = .
replace source00=sourcethou00*1000
gen idp00 = .
replace idp00 = idpthou00*1000
gen host00 = .
replace host00=hostthou00*1000

***Calculate total displaced persons (int'l refugees plus internally displaced)***
gen tdp00 = source00+idp00

***Generate displaced dummy***
gen hasdisplaced =  .
replace hasdisplaced = 0 if tdp00==0
replace hasdisplaced = 1 if tdp00>0

***Generate African dummy***
gen subsaharan = .
replace subsaharan = 0 if region !=6
replace subsaharan = 1 if region ==6

***Generate Asia dummy***
gen asian = .
replace asian=0 if region !=3
replace asian=1 if region ==3 

***Keep relevant vars for analysis***
keep countryname subsaharan asian fhpolrts00 hasdisplaced ticorrind00 region source00 idp00 host00 tdp00

***Add labels***
label variable countryname "Name of country"
label variable ticorrind00 "Transparency International corruption index (high=less corrupt), 2000"
label variable region "Region of country"
label variable source00 "Refugees originating from country, 2000"
label variable idp00 "Internally displaced persons in country, 2000"
label variable host00 "Refugees hosted by country, 2000"
label variable tdp00 "Refugees and internally displaced persons from country, 2000"
label variable hasdisplaced "Country has displaced people, 2000"
label variable fhpolrts00 "Freedom House political rights score (low=more rts), 2000"
label define regionlabel 1 "Europe" 2 "Eastern Europe" 3 "Asia" 4 "Oceania" 5 "Middle East & N. Africa" 6 "Sub-Saharan Africa" 7 "South America" 8 "North America"
label values region regionlabel
label define dummy 0 "False" 1 "True"
label values hasdisplaced dummy
label variable subsaharan "Country is in Sub-Saharan Africa"
label values subsaharan dummy
label variable asian "Country is in Asia"
label values asian dummy

***Correcting an error, France!=Middle east***
replace region=1 if countryname=="France"

***Descriptive Statistics and Analysis***
summarize
*table 1
tabulate hasdisplaced region
ttest tdp00, by(subsaharan)
ttest tdp00, by(asian)
*table 2
regress tdp00 ticorrind00
regress tdp00 fhpolrts00
*table 3

***Make visualization 2 data***
collapse(mean) tdp00, by(fhpolrts00)

log close 




