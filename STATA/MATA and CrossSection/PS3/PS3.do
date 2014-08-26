***Originally Created by Alex Miller on 03/07/2014
***filename: PS3.do
***Purpose: Complete problem set 3

clear

webuse set "http://rlhick.people.wm.edu/econ407/data"
webuse Card_Kreuger_long

*2
quietly tab chain, gen(chaindum)
gen minwage = 4.25
replace minwage = 5.05 if state==1 & year==2
gen abovemin = wage_st-minwage
gen fte = empft+(emppt*0.5)

reg fte minwage abovemin hrsopen nregs chaindum2-chaindum4, robust
est store ols
mfx, dyex

*3
xtset sheet year
xtreg fte minwage abovemin hrsopen nregs chaindum2-chaindum4, re
est store re
mfx, dyex
xttest0

*4
*Dummy
quietly tab sheet, gen(sheetdum)
gen yeardum = 0
replace yeardum = 1 if year==2
set matsize 425
quietly reg fte minwage abovemin hrsopen nregs chaindum2-chaindum4 sheetdum2-sheetdum410 yeardum, robust
est store fedummy
esttab fedummy, se drop(*sheetdum*) stats(r2 N)
drop sheetdum*

*Demeaning (automatic)
xtreg fte minwage abovemin hrsopen nregs chaindum2-chaindum4 yeardum, fe
est store fedemean
mfx, dyex

esttab re fedummy fedemean using fixedeff.csv, replace se drop(*sheetdum*) stats(r2 N)

*First-Diff
clear

webuse Card_Kreuger_wide
quietly tab chain, gen(chaindum)
gen minwage1 = 4.25
gen minwage2 = 4.25
replace minwage2 = 5.05 if state==1
gen abovemin1 = wage_st1-minwage1
gen abovemin2 = wage_st2-minwage2
gen fdfte = (empft2+(0.5*emppt2))-(empft1+(0.5*emppt1))
gen fdminwage = minwage2-minwage1
gen fdabovemin = abovemin2-abovemin1
gen fdhrsopen = hrsopen2-hrsopen1
gen fdnregs = nregs2-nregs1
*Chain dummies are time invariant
reg fdfte fdminwage fdabovemin fdhrsopen fdnregs, robust
est store fefd
esttab fefd using fixedeff.csv, append se stats(r2 N)

