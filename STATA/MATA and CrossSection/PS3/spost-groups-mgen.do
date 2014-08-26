capture log close
log using spost-groups-mgen, replace text

version 13.1
clear all
set linesize 80
matrix drop _all
set scheme s2gcolor

// Group comparisons using predicted probabilities
// Reproduce analysis for ...
// Scott Long 2013-11-10
local pgm spost-groups-mgen
local dte 2013-11-10
local who scott long
local tag "`pgm' `who' `dte'"

// graph options

local Fcol "red"
local Mcol "green"
local legendFM "pos(11) ring(0) cols(1)"
local legendCI "pos(11) ring(0) cols(1) order(2 1)"
local lineFh "msym(Oh) clcol(`Fcol') mcol(`Fcol')"
local lineMh "msym(Th) clcol(`Mcol') mcol(`Mcol')"
local lineFs "msym(O) clcol(`Fcol') mcol(`Fcol')"
local lineMs "msym(T) clcol(`Mcol') mcol(`Mcol')"
local lineDC "msym(i) clcol(blue)   clpat(solid)"
local lineDCblue "msym(i) clcol(blue)"
local lineDCorange "msym(i) clcol(orange)"
local lineCI "msym(i) clcol(brown) clpat(dash)"
local lineCI2 "msym(i i) clcol(brown brown) clpat(dash dash)"
local line_artis "msym(D) clcol(none) mcol(magenta)"

// #1 descriptive statistics

spex tenure01, clear
keep if pdasample

sum tenure female year yearsq select articles prestige presthi
sum tenure female year yearsq select articles prestige presthi if female
sum tenure female year yearsq select articles prestige presthi if male

// #2 m1: female dummy and other variables

logit tenure male c.year c.year#c.year select articles presthi, nolog
listcoef, help

// #3 m1: Pr(tenure|articles, gender) in year 7

mgen, at(articles=(0(2)50) male=0 year=7) atmeans stub(m1F)
mgen, at(articles=(0(2)50) male=1 year=7) atmeans stub(m1M)
label var m1Fpr1 "Women"
label var m1Mpr1 "Men"

twoway  (connected m1Fpr1 m1Farticles, `lineFh') ///
        (connected m1Mpr1 m1Marticles, `lineMh') ///
        , ///
        subtitle("Model 1: For average scientists in year seven", pos(11)) ///
        xlabel(0(10)50) ylabel(0(.25)1, grid) yline(0 1, lcol(gs14)) ///
        ytitle(Probability of Tenure) xtitle(# of Articles) ///
        legend(`legendFM') caption("#3 `tag'",size(vsmall))
graph export `pgm'#3-m1-prob.emf, replace

//  #4 m2: female by article

logit tenure i.male##c.articles

// #5 probabilities and DC

* probabilities for men
mgen, at(male=1 articles=(0(2)50)) atmeans stub(m2M)
label var m2Mpr1 "Men"

* probabilities for women
mgen, at(male=0 articles=(0(2)50)) atmeans stub(m2F)
label var m2Fpr1 "Women" // for women

* gender difference in probability
mgen, dydx(male) at(articles=(0(2)50)) atmeans stub(m2DC)
label var m2DCd_pr1 "Pr(Men) - Pr(Women)"
label var m2DCul1 "95% confidence interval"
label var m2DCll1 "95% confidence interval"

* variable is missing if DC is not significant
gen m2DCsig = m2DCd_pr1 if m2DCll1>=0 & m2DCll1!=.
label var m2DCsig "Difference is significant"
* label used in plot
label var m2DCd_pr1 "Difference is not significant"

* highlight specific article to show on graph
mtable, at(male=0 articles=30) atmeans
gen m2F_atart = el(r(table),1,1) in 1
mtable, at(male=1 articles=30) atmeans
gen m2M_atart =  el(r(table),1,1) in 1
gen m2art_atart = 30 in 1
gen m2DC_atart = m2M_atart - m2F_atart

// #6 plot probabilities

twoway  /// female probabilities
        (connected m2Fpr1 m2Farticles, `lineFh') ///
        /// male probabilities
        (connected m2Mpr1 m2Farticles, `lineMh') ///
        /// highligh one point
        (scatter m2F_atart m2art_atart, msym(O) mcol(`Fcol')) ///
        (scatter m2M_atart m2art_atart, msym(T) mcol(`Mcol')) ///
        , ///
        subtitle("Model 2", pos(11)) ///
        xlabel(0(10)50) ylabel(0(.25)1, grid) yline(0 1, lcol(gs14)) ///
        ytitle(Probability of Tenure) xtitle(# of Articles) ///
        legend(pos(11) ring(0) cols(1) order(2 1)) ///
        caption("#6 `tag'",size(vsmall))
graph export `pgm'#6-m2-prob.emf, replace

// #7 DC with CI

twoway  /// DC
        (connected m2DCd_pr1 m2Farticles, `lineDC') ///
        /// confidence interval for DC
        (connected m2DCll m2DCul m2Farticles, `lineCI2') ///
        /// highlight one point
        (scatter m2DC_atart m2art_atart, msym(Sh) mcol(black)) ///
        , ///
        subtitle("Model 2", pos(11)) ///
        xlabel(0(10)50) ylabel(-.1(.1).8, grid) ///
        yline(0) yline(-.1 .8, lcol(gs14)) ///
        xtitle(# of Articles) ytitle("Pr(men) - Pr(women)") ///
        legend(`legendCI') caption("#7 `tag'",size(vsmall))
graph export `pgm'#7-m2-dcCI.emf, replace

//  #8 m2: DC with dashed line for non sig change

twoway  /// nonsignificant is dashed line
        (connected m2DCd_pr1 m2Farticles, `lineDCblue' clpat(dash)) ///
        /// significant DC is solid line
        (connected m2DCsig m2Farticles,`lineDCblue' clpat(solid)) ///
        /// mark specific location on curve
        (scatter m2DC_atart m2art_atart, msym(Sh) mcol(black)) ///
        , ///
        subtitle("Model 2", pos(11)) ///
        xlabel(0(10)50) ylabel(-.1(.1).8, grid) ///
        yline(0) yline(-.1 .8, lcol(gs14)) ///
        xtitle(# of Articles) ytitle("Pr(men) - Pr(women)") ///
        legend(`legendCI') caption("#8 `tag'",size(vsmall))
graph export `pgm'#8-m2-dcSIG.emf, replace

// #9 m3: interact female with article and high prestige

logit tenure i.male##(c.articles i.presthi)
estimates store m3

// #10 probabilities

mgen, at(male=0 articles=(0(2)50) presthi=0) atmeans stub(m3floF)
label var m3floFpr1 "Women - not distinguished"

mgen, at(male=0 articles=(0(2)50) presthi=1) atmeans stub(m3fhiF)
label var m3fhiFpr1 "Women - distinguished"

mgen, at(male=1 articles=(0(2)50) presthi=0) atmeans stub(m3floM)
label var m3floMpr1 "Men - not distinguished"

mgen, at(male=1 articles=(0(2)50) presthi=1) atmeans stub(m3fhiM)
label var m3fhiMpr1 "Men - distinguished"

capture drop plot_art
gen plot_art = m3floMarticles // simpler name for convenience
label var plot_art "Number of articles"

// #11 DC

mgen, dydx(male) at(articles=(0(2)50) presthi=0) atmeans stub(m3floDC)
label var m3floDCd_pr1 "Male-Female - not distinguished"
label var m3floDCul1 "95% confidence interval"
label var m3floDCll1 "95% confidence interval"

mgen, dydx(male) at(articles=(0(2)50) presthi=1) atmeans stub(m3fhiDC)
label var m3fhiDCd_pr1 "Male-Female - distinguished"
label var m3fhiDCul1 "95% confidence interval"
label var m3fhiDCll1 "95% confidence interval"

gen m3floDCsig = m3floDCd_pr1 if m3floDCll1>=0 & m3floDCll1!=.
label var m3floDCsig "Not distinguished"
label var m3floDCd_pr1 "if not significant"

gen m3fhiDCsig = m3fhiDCd_pr1 if m3fhiDCll1>=0 & m3fhiDCll1!=.
label var m3fhiDCsig "Not distinguished"
label var m3fhiDCd_pr1 "if not significant"

// #12 plot probabilities

twoway  /// female hi prestige
        (connected m3fhiFpr1 plot_art, `lineFs' clpat(solid)) ///
        /// female lo
        (connected m3floFpr1 plot_art, `lineFh' clpat(shortdash_dot)) ///
        /// male hi
        (connected m3fhiMpr1 plot_art, `lineMs' clpat(solid)) ///
        /// male lo
        (connected m3floMpr1 plot_art, `lineMh' clpat(shortdash_dot)) ///
        , ///
        subtitle("Model 3", pos(11)) ///
        xlabel(0(10)50) ylabel(0(.25)1, grid) yline(0 1, lcol(gs14)) ///
        ytitle(Probability of Tenure) xtitle(# of Articles) ///
        legend(pos(11) order(4 3 2 1) ring(0) cols(1) region(ls(none))) ///
        caption("#12 `tag'",size(vsmall))
graph export `pgm'#12-m3f-prob-prsthi.emf, replace

// #13 DC

twoway  /// DC sig lo prestige
        (connected m3floDCsig plot_art, ///
            msym(Sh) mcol(blue) clcol(blue) clpat(solid)) ///
        /// DC sig hi
        (connected m3fhiDCsig plot_art, ///
            msym(S) mcol(orange) clcol(orange) clpat(solid)) ///
        /// DC nonsig lo
        (connected m3floDCd_pr1 plot_art, `lineDCblue' clpat(dash)) ///
        /// DC nonsig hi
        (connected m3fhiDCd_pr1 plot_art, `lineDCorange' clpat(dash)) ///
        , ///
        subtitle("Model 3", pos(11)) ///
        ytitle("Pr(men) - Pr(women)") xtitle(# of Articles) ///
        xlabel(0(10)50) ylabel(-.1(.1).8, grid) ///
        yline(0) yline(-.1 .8, lcol(gs14)) ///
        legend(pos(11) order(2 4 1 3) ring(0) cols(1) region(ls(none))) ///
        caption("#13 `tag'",size(vsmall))
graph export `pgm'#13-m3f-dcSIG-prsthi.emf, replace

//  #14 m4 full model

logit tenure ///
    i.male##(c.year c.year#c.year c.select c.articles c.prestige)

//  #15 prob by articles for given prestige

forvalues p = 1/5 {

    mgen, at(articles=(0(2)50) prestige=`p' male=0 year=7) atmeans stub(m4Fp`p')
    label var m4Fp`p'pr1 "Women"

    mgen, at(articles=(0(2)50) prestige=`p' male=1 year=7) atmeans stub(m4Mp`p')
    label var m4Mp`p'pr "Men"

    mgen, dydx(male) at(articles=(0(2)50) prestige=`p' year=7) atmeans stub(m4DCp`p')
    label var m4Mp`p'pr "Men"

    label var m4DCp`p'd_pr1 "Male-Female difference"
    label var m4DCp`p'ul1 "Upper bound"
    label var m4DCp`p'll1 "Lower bound"
    gen m4DCp`p'sig = m4DCp`p'd_pr1 if m4DCp`p'll1>=0 & m4DCp`p'll1!=.
    label var m4DCp`p'sig "Prestige `p'"

}

capture drop plot_art
gen plot_art = m4Fp1articles // simpler variable name
label var plot_art "Number of articles"
label var m4DCp1sig "Weak (prestige=1)"
label var m4DCp1d_pr1 "  (not significant)"
label var m4DCp2sig "Adequate (prestige=2)"
label var m4DCp3sig "Good (prestige=3)"
label var m4DCp4sig "Strong (prestige=4)"
label var m4DCp5sig "Distinguished (prestige=5)"
label var m4DCp5d_pr1 "  (not significant)"

//  #16 plot prob by art for each prestige

forvalues p = 1/5 { // prestige level

    * probabilities
    twoway  (connected m4Fp`p'pr plot_art, `lineFh') ///
            (connected m4Mp`p'pr plot_art, `lineMh'), ///
            subtitle("Model 4: Plotted at prestige = `p'",pos(11)) ///
            legend(pos(11) order(2 1) ring(0) cols(1) region(ls(none))) ///
            xtitle(# of Articles) ytitle(Probability of Tenure) ///
            xlabel(0(10)50) ylabel(0(.25)1, grid) yline(0 1, lcol(gs14)) ///
            caption("#16 `tag'",size(vsmall))
    graph export `pgm'#16-m4-prob-prst`p'.emf, replace

    * discrete change with CI
    twoway  (connected m4DCp`p'd_pr1 plot_art, `lineDCorange') ///
            (connected m4DCp`p'ul1  plot_art, `lineCI') ///
            (connected m4DCp`p'll1  plot_art, `lineCI'), ///
            subtitle("Model 4: Plotted at prestige = `p'",pos(11)) ///
            xtitle(# of Articles) ytitle("Pr(men) - Pr(women)") xlabel(0(10)50) ///
            ylabel(-.2(.1).7, grid) ///
            yline(0) yline(-.2 .7, lcol(gs14)) ///
            legend(pos(11) order(2 1) ring(0) cols(1) region(ls(none))) ///
            caption("#16 `tag'",size(vsmall))
    graph export `pgm'#16-m4-dcCI-prst`p'.emf, replace

    * discrete change in probability using broken lines for bounds
    twoway ///
        (connected m4DCp`p'sig plot_art, `lineDCorange' clpat(solid)) ///
        (connected m4DCp`p'd_pr1 plot_art, `lineDCorange' clpat(dash)) ///
      , subtitle("Model 4: Plotted at prestige = `p'",pos(11)) ///
        xtitle(# of Articles) ytitle("Pr(men) - Pr(women)") xlabel(0(10)50) ///
        ylabel(-.2(.1).7, grid) legend(off) ///
        yline(0) yline(-.2 .7, lcol(gs14)) ///
        caption("#16 `tag'",size(vsmall))
    graph export `pgm'#16-m4-dcSIG-prst`p'.emf, replace

} // loop over prestige

//  #17 plot prob by art for multiple prestige levels

twoway ///
    (connected m4DCp1sig plot_art, clpat(solid) msym(i) clcol(red)) ///
    (connected m4DCp2sig plot_art, clpat(solid) msym(i) clcol(orange)) ///
    (connected m4DCp3sig plot_art, clpat(solid) msym(i) clcol(green)) ///
    (connected m4DCp4sig plot_art, clpat(solid) msym(i) clcol(blue)) ///
    (connected m4DCp5sig plot_art, clpat(solid) msym(i) clcol(purple)) ///
    (connected m4DCp1d_pr1 plot_art, clpat(dash) msym(i) clcol(red)) ///
    (connected m4DCp2d_pr1 plot_art, clpat(dash) msym(i) clcol(orange)) ///
    (connected m4DCp3d_pr1 plot_art, clpat(dash) msym(i) clcol(green)) ///
    (connected m4DCp4d_pr1 plot_art, clpat(dash) msym(i) clcol(blue)) ///
    (connected m4DCp5d_pr1 plot_art, clpat(dash) msym(i) clcol(purple)) ///
   , ///
    subtitle("Model 4",pos(11)) ///
    ytitle("Pr(men) - Pr(women)") xtitle(# of Articles) ///
    xlabel(0(10)50) ylabel(-.1(.1).5, grid) yline(0) ///
    yline(0) yline(-.1 .5, lcol(gs14)) ///
    legend(pos(11) order(1 2 3 4 5) ring(0) cols(1) region(ls(none))) ///
    caption("#17 `tag'",size(vsmall))
    graph export `pgm'#17-m4-dcSIG-prstall.emf, replace

//  #18 predicitons by prestige for given articles

forvalues a = 0(10)50 { // articles

    mgen, at(prestige=(1(.2)5) articles=`a' male=0 year=7) ///
        atmeans stub(m4Fa`a') replace
    label var m4Fa`a'pr1 "Women"

    mgen, at(prestige=(1(.2)5) articles=`a' male=1 year=7) ///
        atmeans stub(m4Ma`a') replace
    label var m4Ma`a'pr1 "Men"

    mgen, dydx(male) at(prestige=(1(.2)5) articles=`a' year=7) ///
        atmeans stub(m4DCa`a') replace
    label var m4DCa`a'd_pr1 "Male-Female difference"
    label var m4DCa`a'ul1 "Upper bound"
    label var m4DCa`a'll1 "Lower bound"
    capture drop m4DCa`a'sig
    gen m4DCa`a'sig = m4DCa`a'd_pr1 if m4DCa`a'll1>=0 & m4DCa`a'll1!=.
    label var m4DCa`a'sig "Prestige `p'"

}

capture drop plot_prst
gen plot_prst = m4Fa0prestige
label var plot_prst "Job Prestige"
label var m4DCa0sig "no articles"
label var m4DCa0d_pr1 "  (not significant)"
label var m4DCa10sig "10 articles"
label var m4DCa20sig "20 articles"
label var m4DCa30sig "30 articles"
label var m4DCa40sig "40 articles"
label var m4DCa50sig "50 articles"
label var m4DCa50d_pr1 "  (not significant)"

//  #19 prob by prestige for each article

twoway ///
    (connected m4DCa0sig  plot_prst, clpat(solid) msym(i) clcol(red)) ///
    (connected m4DCa10sig plot_prst, clpat(solid) msym(i) clcol(orange)) ///
    (connected m4DCa20sig plot_prst, clpat(solid) msym(i) clcol(brown)) ///
    (connected m4DCa30sig plot_prst, clpat(solid) msym(i) clcol(green)) ///
    (connected m4DCa40sig plot_prst, clpat(solid) msym(i) clcol(blue)) ///
    (connected m4DCa50sig plot_prst, clpat(solid) msym(i) clcol(purple)) ///
    (connected m4DCa0d_pr1 plot_prst, clpat(dash) msym(i) clcol(red)) ///
    (connected m4DCa10d_pr1 plot_prst, clpat(dash) msym(i) clcol(orange)) ///
    (connected m4DCa20d_pr1 plot_prst, clpat(dash) msym(i) clcol(brown)) ///
    (connected m4DCa30d_pr1 plot_prst, clpat(dash) msym(i) clcol(green)) ///
    (connected m4DCa40d_pr1 plot_prst, clpat(dash) msym(i) clcol(blue)) ///
    (connected m4DCa50d_pr1 plot_prst, clpat(dash) msym(i) clcol(purple)) ///
   , ///
    subtitle("Model 4",pos(11)) ///
    ytitle("Pr(men) - Pr(women)") xtitle(Job Prestige) ///
    xlabel(1(1)5) ylabel(-.1(.1).5, grid) ///
    yline(0) yline(-.1 .5, lcol(gs14)) ///
    legend(pos(11) order(1 2 3 4 5 6) ring(0) cols(2) region(ls(none))) ///
    caption("#19 `tag'",size(vsmall))
graph export `pgm'-m4-dcSIG-artall.emf, replace

log close
exit
