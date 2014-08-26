clear
capture log close


cd "/Users/Alex/Documents/Current Courses/Quantitative Methods II/Licht"


/********************************************************************************/
/*PURPOSE:  In this file, I run two-stage models for "Coming Into Money" using	*/
/*		a heckman probit model to account for selection and an endogenous		*/
/*		dummy.																	*/
/*AUTHOR:	Amanda A. Licht														*/
/*DATE:	May 18, 2009															*/
/*		August 18, 2009	- Update for RnR, using DV which right censors 			*/
/*					term-limited leaders, and instrumenting the 				*/
/*					probability of failure as recommended by Maddala			*/
/*	MODIFIED: August 31, 2009	-Strip down data for replication file			*/
/*DATA:	CIMvarsRnR_8_13_09.dta													*/
/*	SOURCES:																	*/
/*		Archigos_v2.8.2_tv.dta, Goemans and Gleiditsch							*/
/*			<http://mail.rochester.edu/~hgoemans/data>							*/
/*		GDP_v5.0, pwt_6.1 and trade_udd_COW, Gleditsch							*/
/*			<http://privatewww.essex.ac.uk/~ksg/exptradegdp.html>				*/
/*		GrossODA_monadic.dta, OECD DAC online									*/
/*		NetODA_mondadic.dta, OECD DAC online									*/
/*		MIDB_3.10_disppart.csv, Ghosn, Palmer and Bremer (2004)					*/
/*		<http://www.correlatesofwar.org/COW2%20Data/MIDs/MID310.html>			*/
/*		shatterbelt.csv, Hensel&Diehl (1994)									*/
/*			<http://www.paulhensel.org/dataintl.html>							*/
/*		F:/WDI/WDI_aidworthy.csv,World Development Indicators Online			*/
/*			WDI_econtrade.csv,													*/
/*			WDI_governance.csv													*/
/*			<http://www.worldbank.org>											*/
/*		F:/EIA_crudeprod_19802004.csv, U.S. Energy Info Admin					*/
/*			<http://tonto.eia.doe.gov/>											*/
/*		F:/Civil Conflict/UCDP_PRIO_SCD_v42008, Uppsala Data Project			*/
/*			<http://www.pcr.uu.se/research/UCDP/>								*/
/*		F:/ATOP/atop3_0dyEUGNNA.dta, 											*/
/*			Leeds,Long,Mattes,Mitchell,Ritter and Savun 2005					*/
/*			<http://atop.rice.edu/data>											*/
/*FILE:	CIM_JCR_rep.do															*/
/*LOG:	CIM_JCR_rep.log															*/
/*OUTPUT:	pfail.out,netget.out, heckprobwfail2.out, CIM_JCR_rep.dta,			*/
/*		CIM_JCR_rep_netgetinterp.dta,CIM_JCR_rep_Zinterp.dta,					*/
/*		CIM_JCR_rep_popinterp.dta, CIM_JCR_rep_fig1,CIM_JCR_rep_fig2,			*/
/*		CIM_JCR_rep_fig3,CIM_JCR_rep_firstdiff.dta, 							*/
/*		CIM_JCR_rep_heckinterp.dta, CIM_JCR_rep_firstdiff_cis.dta				*/
/*		CIM_JCR_rep_coldwarvnewage.out											*/
/********************************************************************************/


use CIM_JCR_rep.dta, clear

describe
sum


	/*************************************************************************/
	/*1. First, I will develop the instrument for probability of losing office*/
	/*************************************************************************/


#delimit;
probit wfail2 lognet3 tlognet3 wbiglognet3 twbiglognet3 Wbig lnt growth_lag lntrade_lag civlev 
	SOUTHAM SUBAFRICA SOUTHASIA, cluster(leadid);
*outreg using pfail.out, coefastr se 3aster bdec(4) replace;
#delimit cr

*fitstat

estat clas


*drop xb Z pfail se 	/*drop predicted values from prior run, if necessary*/

		predict double xb, xb
			
			lab var xb "Linear index of wfail2 equation with W dummy"
		
		predict double se, stdp
		
			lab var se "Standard Error of wfail2 equation with W dummy"
		
		predict double pfail, p
			
			lab var pfail "Probability of Failure from wfail2 Equation with W dummy"
		
		gen Z = xb/se
			
			lab var Z "Linear index of wfail2 equation adjusted by standard error with W dummy, Maddala style"
	

drop if Z==.


probit netget Z netgetlag popgrowth_lag ldonorexp_lag lnpop growth_lag W formercol strongally oil intcivconflict, cluster(leadid)
*outreg using netget.out, coefastr se 3aster bdec(4) replace

*fitstat

estat clas

*prchange


#delimit ;
scatter Z pfail, title("Figure 1. Relationship between Instrument" "and Probability of Failure") 
	xtitle("Predicted Probability of Failure") ytitle("XB/standard error of prediction") 
	note("NOTE:  Statistics calculated using coefficient estimates reported in Table 1.")
	saving(CIM_JRC_rep_fig1.gph, replace);
#delimit cr




	/********************************************************************************************************************************/
	/*2. Now I will run the selection model and run brief programs to interpret the role of baseline risk and popgrowth in selection*/
	/********************************************************************************************************************************/



#delimit;
heckprob wfail2 lognet tlognet wbiglognet twbiglognet Wbig lnt growth_lag lntrade_lag civlev 
	SOUTHAM SUBAFRICA SOUTHASIA, 
	sel(netget = Z netgetlag popgrowth_lag lnpop ldonorexp_lag W formercol strongally intcivconflict oil) 
	cluster(leadid) difficult;
*outreg using heckprobwfail2.out, coefastr se 3aster bdec(4) replace;
#delimit cr

mfx, predict(psel) 
mfx

predict psel, psel

sum psel if netgetlag==0
sum psel if netgetlag==1

sum psel if netgetlag==0&W<.75

sum psel if netgetlag==0&W>=.75


mat def B = e(b)

mat list B

mat def V = e(V)


compress
label data "Variables for revise and resubmit of Coming into Money at JCR"
save CIM_JCR_rep.dta, replace



			
collapse (mean) popgrowth_lag lnpop ldonorexp_lag W se Z				/*Make dataset of mean values for simulations*/

expand 100
save CIM_JCR_rep_netgetinterp.dta, replace								/*Make 100 obs of the mean values and save*/



		/***************************************************/
		/*Program for Interpreting Z's Effect on Allocation*/
		/***************************************************/

	program define zinterp
		syntax, num(real)

			use CIM_JCR_rep_netgetinterp.dta, clear

			drawnorm b1-b25, means(B) cov(V)

			#delimit ;
			gen P1 = normprob(b14*`num' +b16*popgrowth_lag +b17*lnpop +b18*ldonorexp_lag +b19*W +b20 +b21 +b23 + b24);
			#delimit cr
		
		keep P1 se
		gen Z = `num'

		gen xb = (Z*se)

		gen pfail = normprob(xb)

		append using CIM_JCR_rep_Zinterp.dta
		save CIM_JCR_rep_Zinterp.dta, replace
		
	end

clear
	save CIM_JCR_rep_Zinterp.dta, replace emptyok		/*Make empty datset in which simulation results will be saved*/



		/*******************************************************************/
		/*Program for Interpreting population growth's Effect on Allocation*/
		/*******************************************************************/


	program define popinterp
		syntax, num(real)

			use netgetinterp.dta, clear

			drawnorm b1-b25, means(B) cov(V)

			#delimit ;
			gen P1 = normprob(b14*Z+b16*`num' +b17*lnpop +b18*ldonorexp_lag +b19*W +b20 +b21 +b23 + b24);
			#delimit cr
		
		keep P1
		gen popgrowth_lag = `num'


		append using CIM_JCR_RNR_popinterp.dta
		save CIM_JCR_RNR_popinterp.dta, replace
		
	end

clear
	save CIM_JCR_rep_popinterp.dta, replace emptyok		/*Make empty datset in which simulation results will be saved*/



		/************************************************************/
		/*Simulation commands run the programs above at given values*/
		/************************************************************/


foreach num of numlist -33 -27 -23 -20 -17 -14 -8 -5 -1 {
	
	simulate, reps(100):  zinterp, num(`num')
}



forvalues num= -6(2)12 {
	
	simulate, reps(100):  popinterp, num(`num')
}



		/************************************************/
		/*Open results of Zinterp simulations and graph	*/
		/************************************************/

use CIM_JCR_rep_Zinterp.dta, clear


sum

collapse (mean) P1 pfail (sd) sd=P1, by(Z)


	gen P1_lo = P1 - 1.96*sd
	gen P1_hi = P1 + 1.96*sd



#delimit ;
line P1 P1_lo P1_hi pfail, sort lpattern(solid dash dash) lcolor(black gs10 gs10) 
	title("Probability of Losing Office")
	ytitle("Pr(Aid Allocation)") xtitle("Probability of Losing Office") name(Zint, replace)
	legend(off);
#delimit cr


		/******************************************************/
		/*Open results of popinterp simulations and graph	*/
		/******************************************************/



use CIM_JCR_RNR_popinterp.dta, clear


sum

collapse (mean) P1 (sd) sd=P1, by(popgrowth_lag)


	gen P1_lo = P1 - 1.96*sd
	gen P1_hi = P1 + 1.96*sd



#delimit ;
line P1 P1_lo P1_hi popgrowth_lag, sort lpattern(solid dash dash) lcolor(black gs10 gs10) 
	title("Rate of Population Growth")
	ytitle("Pr(Aid Allocation)") xtitle("Population Growth")
	legend(off) name(popint, replace);
#delimit cr


#delimit ;
graph combine Zint popint, ycommon title("Figure 1. Probability of Aid Allocation")
	note("NOTE:  Statistic is mean prediction from 100 simulations at each level of baseline risk"
	"using a draw of 100 beta coefficients from the variance-covariance matrix of the censored"
	"probit reported in Table ##. Dashed lines indicate 95% confidence interval.")
	saving(CIM_JCR_rep_fig2.gph, replace);
#delimit cr



	/******************************************************************************************************************/
	/*3. Next I perform tests of subs. significance for the interaction effects by calculating the fist diferences in	*/
	/*likelihood of failure given differing levels of key covariates. This requires some monte carlo simulations and 	*/
	/*Application of the likelihood functions. For the censored probit Timpone 2002 gives the probability of outcomes:*/
	/*								   P(y_sel=0) = 1-NORM(BsXs)							*/
	/*							P(y_sel=1&y_out=0) = BINORM[-BsXs, BoXo, -rho]					*/
	/*							  P(y_sel=1&y_out=1) = BINORM[BsXs, BoXo,rho]					*/
	/******************************************************************************************************************/



use CIM_JCR_rep.dta, clear


#delimit;
heckprob wfail2 lognet tlognet wbiglognet twbiglognet Wbig lnt growth_lag lntrade_lag civlev 
	SOUTHAM SUBAFRICA SOUTHASIA, 
	sel(netget = Z netgetlag popgrowth_lag lnpop ldonorexp_lag W formercol strongally intcivconflict oil) 
	cluster(leadid) difficult;
#delimit cr


mat def B = e(b)
mat def V = e(V)			/*save parameter estimates*/



#delimit ;
collapse 	(mean) lognet=lognet growth_lag ldonorexp_lag civlev popgrowth_lag lnpop lntrade_lag sumten 
		(min) lognet_min=lognet t_min=sumten 
		(max) lognet_max=lognet t_max=sumten
		(sd) lognet_sd = lognet;
#delimit cr
										/*Collapse to mean, min, max and standard deviation values of necessary variables*/

			expand 100		/*Create 100 identical observations*/
			
			
			
			
				gen t = t_min + (t_max - t_min)*((_n-1)/_N)
						/*Regain full range of tenure at even increments*/	
			
			
				gen lnt= ln(t)			/*Regain log of cumulative tenure*/					
			
			
				gen lognethi = lognet + lognet_sd		/*Calculate high value of aid*/


				gen tlognet = lognet*lnt
				gen tlognethi= lognethi*lnt

				sort t
				expand 100			/*Obtain 100 identical observations*/

compress
save CIM_JCR_rep_heckinterp.dta, replace			/*Save independent variables for simulations*/




#delimit ;
	program define heckdiffs;
		syntax, num(real);


			use CIM_JCR_rep_heckinterp.dta, clear;


				drawnorm b1-b25, means(B) cov(V);		
						/*Draw sample of betahats from the variance-covariance matrix*/



				gen XB_mean = 	b1*lognet 
							+ b2*tlognet 
							+ b3*`num'*lognet 
							+ b4*`num'*tlognet 
							+ b5*`num'
							+ b6*lnt 
							+ b7*growth_lag 
							+ b8*lntrade_lag 
							+ b9*civlev 
							+ b10*0 
							+ b11*0 
							+ b12*0
							+ b13;


				gen XB_hi =		b1*lognethi 
							+ b2*tlognethi 
							+ b3*`num'*lognethi 
							+ b4*`num'*tlognethi 
							+ b5*`num'
							+ b6*lnt 
							+ b7*growth_lag 
							+ b8*lntrade_lag 
							+ b9*civlev 
							+ b10*0 
							+ b11*0 
							+ b12*0
							+ b13;					/*Calculate linear indices for wfail2 equation*/

		
				egen sd_mean = 	sd(XB_mean);

				egen sd_hi = 	sd(XB_hi);						/*Calculate standard error of linear indices*/


 
				gen Zmean =		XB_mean/sd_mean; 

				gen Zhi =		XB_hi/sd_hi; 					/*Calculate xb/se*/




				gen XB_get_mean =	b14*Zmean 
								+ b15*1 
								+ b16*popgrowth_lag 
								+ b17*lnpop 
								+ b18*ldonorexp_lag 
								+ b19*`num' 
								+ b20*1 
								+ b21*1 
								+ b22*0 
								+ b23*1
								+ b24;					

				gen XB_get_hi =	b14*Zhi
								+ b15*1 
								+ b16*popgrowth_lag 
								+ b17*lnpop 
								+ b18*ldonorexp_lag 
								+ b19*`num'
								+ b20*1 
								+ b21*1 
								+ b22*0 
								+ b23*1
								+ b24;						/*Calculate linear indices of netget model*/


				

				gen rho = (exp(2*atanh(b25))-1)/(exp(2*atanh(b25))+1);		/*Restrict rho to the interval [-1, 1]*/


				gen pfail_mean =	binormal(XB_get_mean, XB_mean, rho);

				gen pfail_hi =	binormal(XB_get_hi, XB_hi, rho);			/*Calculate pr(failure)*/



				gen D =	((pfail_hi - pfail_mean)/pfail_mean)*100;			/*Generate Difference in pr(failure)*/


				gen Wbig = `num';									/*Generate indicator for regime type*/

				collapse (mean) 	pfail_mean  pfail_hi  D
					(sd)  semean=pfail_mean  sehi=pfail_hi seD=D, by(t Wbig );	/*Collapse to means and std errors of predictions*/


				append using CIM_JCR_rep_firstdiff.dta;					/*Attach to prior results*/

				save CIM_JCR_rep_firstdiff.dta, replace;					/*Save results*/
	
			end;

	clear;

	save CIM_JCR_rep_firstdiff.dta, replace emptyok;				/*make empty datasets for results*/


		/******************************/
		/*Run Loop over values of Wbig*/
		/******************************/

#delimit cr

set more off
		
	foreach num of numlist 0 1 {
		
				simulate, noisily seed(124) reps(100):  heckdiffs, num(`num') 
		}
		

		/******************************/
		/*Open simulation results	*/
		/******************************/


use CIM_JCR_rep_firstdiff.dta, clear			


sum D if Wbig==0

sum D if Wbig==1						/*Summarize findings*/


#delimit ;
collapse (mean) 	pfail_mean  pfail_hi  D
	(sd)  semean=pfail_mean  sehi=pfail_hi seD=D, by(t Wbig );	/*Collapse to means and std errors of predictions within the 100 simulations*/
#delimit cr

sum D if Wbig==0

sum D if Wbig==1						/*Summarize findings*/


twoway line D t if Wbig==0||line D t if Wbig==1		/*Sneak Peak*/



	gen Dlo= 	D - 1.96*seD
	gen Dhi = 	D + 1.96*seD	/*Generate Confidence Intervals*/



gen years = t/365
sum years				/*Shift time scale to years*/


				/************************************************************/
				/*Finally, I can graph the results to check for significance*/
				/************************************************************/



#delimit ;
twoway line Dlo Dhi D years if Wbig==1&years<20, sort lcolor(gs10 gs10 black) lpattern(dash dash solid ) name(dem, replace)
	title("Democratic Leaders") xtitle("Years in Office") ytitle("% Change in P(failure)") legend(off)
	xline(2.847) yline(0);

#delimit ;
twoway line Dlo Dhi D years if Wbig==0, sort lcolor(gs10 gs10 black) lpattern(dash dash solid ) name(aut, replace)
	title("Nondemocratic Leaders") xtitle("Years in Office") ytitle("% Change in P(failure)") legend(off)
	xline(6.877) yline(0);

#delimit ;
graph combine dem aut, 
	title("Fig.2 Change in Probability of Failure, Given" "Drop from One Standard Deviation Above to the Mean of Aid")
	note("NOTE: Reported Statistic is the mean of 10,000 draws from the variance-covariance matrix of the"
	"censored probit reported in Table##.  Dashed lines indicate 95% confidence intervals.  Democratic"
	"leaders defined by W scores greater or equal to .75; nondemocratic leaders, by W scores below .75."
	"Dashed vertical line markst the 50% mark in the distribution of failure times for leader type. Note"
	"that the Y-axis scales are not equivalent across panels.")
	saving(CIM_JCR_rep_fig3.gph, replace); 
#delimit cr	

save CIM_JCR_rep_firstdiff_cis.dta, replace






				/************************************************************************/
				/*	4. Run Full Model on Sub-Samples by Cold War vs. New Age		*/
				/************************************************************************/



use CIM_JCR_rep.dta, clear


	tab Wbig netget if ryear<1989, col

	tab Wbig netget if ryear>=1989, col		/*Determine appropriateness of reference category across sub-samples*/

#delimit ;
prob wfail2 lognet3 tlognet3 wbiglognet3 twbiglognet3 Wbig lnt growth_lag lntrade_lag civlev 
	SOUTHAM SUBAFRICA SOUTHASIA if ryear<1989, cluster(leadid); 
#delimit cr

	predict double xbcw, xb
			lab var xbcw "XB for coldwar probit"

	predict double secw, stdp
			lab var secw "standard error of prediction for coldwar probit"

	predict double pfailcw, p
			lab var pfailcw "probability of failure for coldwar probit"

	gen Zcw = xbcw/secw
			lab var Zcw "Maddala style instrument for coldwar probit"


probit netget Zcw netgetlag popgrowth_lag lnpop ldonorexp_lag W formercol strongally intcivconflict oil if ryear<1989, cluster(leadid)


#delimit;
heckprob wfail2 lognet tlognet wbiglognet twbiglognet Wbig lnt growth_lag lntrade_lag civlev 
	SOUTHAM SUBAFRICA SOUTHASIA if ryear<1989, 
	sel(netget = Zcw netgetlag popgrowth_lag lnpop ldonorexp_lag W formercol strongally intcivconflict oil) 
	cluster(leadid) difficult;
outreg using CIM_JCR_rep_coldwarvnewage.out, coefastr se 3aster bdec(4) replace;
#delimit cr


#delimit ;
prob wfail2 lognet3 tlognet3 wsmllognet3 twsmllognet3 Wsmall lnt growth_lag lntrade_lag civlev 
	SOUTHAM SUBAFRICA SOUTHASIA if ryear>=1989, cluster(leadid); 
#delimit cr

	predict double xbna, xb
			lab var xbna "XB for newage probit"

	predict double sena, stdp
			lab var sena "standard error of prediction for newage probit"

	predict double pfailna, p
			lab var pfailna "probability of failure for newage probit"

	gen Zna = xbna/sena
			lab var Zna "Maddala style instrument for newage probit"

probit netget Zna netgetlag popgrowth_lag lnpop ldonorexp_lag W formercol strongally intcivconflict oil if ryear>=1989, cluster(leadid)


#delimit;
heckprob wfail2 lognet tlognet wsmllognet twsmllognet Wsmall lnt growth_lag lntrade_lag civlev 
	SOUTHAM SUBAFRICA SOUTHASIA if ryear>=1989, 
	sel(netget =  Zna netgetlag popgrowth_lag lnpop ldonorexp_lag W formercol strongally intcivconflict oil) 
	cluster(leadid) difficult;
outreg using CIM_JCR_rep_coldwarvnewage.out, coefastr se 3aster bdec(4) append;
#delimit cr





log close
