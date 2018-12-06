**********************************************************
* THIS FILE REPLICATES ALL RESULTS FROM THE PAPER 		 *
* STATA SE v14.0										 *
* ANY QUESTIONS PLEASE CONTACT ZHANGDA@MIT.EDU			 *
**********************************************************

clear all
set more off
set matsize 1000
use BJ_403.dta // BJ_403.dta stores the raw data

/* Create a new variable "change" to record whether or not the firm has changed the verifier in the current year */
drop if year == 2009 | year == 2010 | year == 2011
bysort firm_code: gen change = 0
bysort firm_code: replace change = 1 if(verifier_name[_n] != verifier_name[_n-1])
bysort firm_code: replace change = . if(verifier_name[_n] == "" | verifier_name[_n-1] == "")
replace change = 0 if year == 2012
drop if verifier_name == ""

/* Define year dummy variable */
gen yr12 = cond(year == 2012, 1, 0)
gen yr13 = cond(year == 2013, 1, 0)
gen yr14 = cond(year == 2014, 1, 0)
gen yr15 = cond(year == 2015, 1, 0)
gen t = 1 if year == 2012
replace t = 2 if year == 2013
replace t = 3 if year == 2014
replace t = 4 if year == 2015

/* Definition of the dependent variable - adjusted discrepancies */
gen emis_ratio = emis_total_self_reported_ton / emis_total_verified_ton
gen emis_ratio_trans = tanh(log(emis_ratio)) 

/*----------------------------------------------------------------------------------------------------------------------------*/
/* Supplementary Table 1 - Descriptive statistics for adjusted discrepancies and verifier change from 2012 to 2015 in Beijing */
/*----------------------------------------------------------------------------------------------------------------------------*/
tabu year, su (emis_ratio_trans)
tabu year change
unique verifier_name, by(year) gen(num_verifier)

/*---------------------------------------------------------------------------------------------------------*/
/* Figure 3 - Fixed effects for adjusted discrepancies and numbers of firms verified by verifier in Beijing*/
/*---------------------------------------------------------------------------------------------------------*/
/* Sort the verifier by the number of firms they verified */
bysort verifier_name: gen firm_verified = _N
bysort firm_verified verifier_name: gen index = _n
bysort firm_verified verifier_name: gen verifierid = 1 if index == 1
replace verifierid = sum(verifierid)
replace verifierid = 23 - verifierid
drop index

/* Implement the likelihood test for the nested models to check the verifier fixed effects */
xtset firm_code year, yearly
replace emis_ratio_trans = (-1) * emis_ratio_trans if year == 2012		// Change the sign of 2012 to make the meaning of sign consistent when estimating fixed effects 
xtreg emis_ratio_trans i.year i.verifierid, fe
est store one
xtreg emis_ratio_trans i.year, fe
est store two
lrtest one two

/* Two-level fixed effects: Full Panel */
felsdvreg emis_ratio_trans yr13 yr14 yr15, ivar(firm_code) jvar(verifierid) feff(feffhat) peff(peffhat) xb(xb) res(res) mover(mover) group(group) mnum(mnum) pobs(pobs) cluster(firm_code) cons
table verifierid, contents(m feffhat)
tabu verifierid year

// Use xtreg here to extract standard errors of verifiers' fixed effects. Points estimates are identical to the results from Felsdvreg.
xtset firm_code year, yearly
xtreg emis_ratio_trans i.year i.verifierid, fe vce(cluster firm_code)
matrix v = e(V)
quiet gen stdff_v = .
forval g = 1/22{
  quiet replace stdff_v = sqrt(v[4 +`g', 4 +`g']) if verifierid == `g'
}
tabu verifierid, su(stdff_v)
replace emis_ratio_trans = (-1) * emis_ratio_trans if year == 2012
save fe_full.dta, replace

/*-----------------------------------------------------------------------------------------------------------------------------------------------------------*/
/* Supplementary Table 2 and Supplementary Figure 1 - Fixed effects and rank for adjusted discrepancies for the eight major verifiers with different samples */
/*-----------------------------------------------------------------------------------------------------------------------------------------------------------*/
/* (1) Panle A: Full sample analysis  */
use fe_full.dta, clear
replace emis_ratio_trans = (-1) * emis_ratio_trans if year == 2012		// Change the sign of 2012 to make the meaning of sign consistent when estimating fixed effects 
table verifierid, contents(m feffhat)
/* (2) Panle B: Largest 12 verifiers that verified more than 20 firms */
drop if verifier_num_firm_year <= 20
felsdvreg emis_ratio_trans yr13 yr14 yr15, ivar(firm_code) jvar(verifierid) feff(feffhat) peff(peffhat) xb(xb) res(res) mover(mover) group(group) mnum(mnum) pobs(pobs) cons
table verifierid, contents(m feffhat)
save fe_12.dta, replace
/* (3) Panle C: Largest 8 verifiers that verified more than 50 firms */
use fe_full.dta, clear
replace emis_ratio_trans = (-1) * emis_ratio_trans if year == 2012		// Change the sign of 2012 to make the meaning of sign consistent when estimating fixed effects 
drop if verifier_num_firm_year <= 50
felsdvreg emis_ratio_trans yr13 yr14 yr15, ivar(firm_code) jvar(verifierid) feff(feffhat) peff(peffhat) xb(xb) res(res) mover(mover) group(group) mnum(mnum) pobs(pobs) cons
table verifierid, contents(m feffhat)
save fe_8.dta, replace

/*----------------------------------------------------------------------------------------------------------*/
/* Supplementary Figure 2 - Correlation between verifier fixed effects and number of reporting errors found */
/*----------------------------------------------------------------------------------------------------------*/
use fe_full.dta, clear
collapse verifierid feffhat stdff_v (count) firm_code (mean) error_num recommendation_num verification_quality_score num_of_verifier_in_team, by(verifier_name)
correlate feffhat error_num verification_quality_score [aweight = 1/stdff_v/stdff_v]

/*---------------------------------------------------------------------------------------------------------------*/
/* Supplementary Table 3 - Correlation between lag adjusted discrepancies and verifier change by year in Beijing */
/*---------------------------------------------------------------------------------------------------------------*/
use fe_full.dta, clear
xtset firm_code year, yearly
gen emis_ratio_trans_l1 = L.emis_ratio_trans
gen verifierid_l1 = L.verifierid
// Column 1: Year 2013
logit change emis_ratio_trans_l1 if year == 2013, cluster(verifierid_l1) robust
preserve
keep if year == 2013
clusterbs logit change emis_ratio_trans_l1, cluster(verifierid_l1) reps(2000) seed(20202) // Pairs cluster bootstrap-t for few clusters
restore

// Column 2: Year 2014
logit change emis_ratio_trans_l1 if year == 2014, cluster(verifierid_l1) robust
preserve
keep if year == 2014
clusterbs logit change emis_ratio_trans_l1, cluster(verifierid_l1) reps(2000) seed(20202) // Pairs cluster bootstrap-t for few clusters
restore

// Column 3: Year 2015
logit change emis_ratio_trans_l1 if year == 2015, cluster(verifierid_l1) robust
preserve
keep if year == 2015
clusterbs logit change emis_ratio_trans_l1, cluster(verifierid_l1) reps(2000) seed(20202) // Pairs cluster bootstrap-t for few clusters
restore

/*------------------------------------------------------------*/
/* Supplementary Table 4 - Change behavior by year in Beijing */
/*------------------------------------------------------------*/
use fe_full.dta, clear
sort verifier_name
merge verifier_name using verifier_score_2016
keep if _merge == 3
drop _merge

xtset firm_code year, yearly

gen score_diff = mean_verifier_score_2016 - L.mean_verifier_score_2016
egen mean_verifier_score_2016_bl_med = xtile(mean_verifier_score_2016), n(2)
replace mean_verifier_score_2016_bl_med = 0 if mean_verifier_score_2016_bl_med > 1 // 1 - below median; 0 - above median
egen mean_verifier_score_2016_bl_trt = xtile(mean_verifier_score_2016), n(3)
replace mean_verifier_score_2016_bl_trt = 0 if mean_verifier_score_2016_bl_trt > 1 // 1 - below the first tertile; 0 - above the first tertile
egen mean_verifier_score_2016_bl_qrt = xtile(mean_verifier_score_2016), n(4)
replace mean_verifier_score_2016_bl_qrt = 0 if mean_verifier_score_2016_bl_qrt > 1 // 1 - below the first quartile; 0 - above the first quartile

gen feffhat_diff = feffhat - L.feffhat
gen quality_diff_med = mean_verifier_score_2016_bl_med - L.mean_verifier_score_2016_bl_med
gen quality_diff_trt = mean_verifier_score_2016_bl_trt - L.mean_verifier_score_2016_bl_trt
gen quality_diff_qrt = mean_verifier_score_2016_bl_qrt - L.mean_verifier_score_2016_bl_qrt

// Panel A: Verifier changes on verifier's strictness change
bysort year: reg feffhat_diff i.change, robust

// Panel B, C, and D: Verifier changes on verifier's quality change
bysort year: reg quality_diff_med i.change, robust
bysort year: reg quality_diff_trt i.change, robust
bysort year: reg quality_diff_qrt i.change, robust

/*------------------------------------------------------------------------------*/
/* Supplementary Table 5 - Verifier changes on adjusted discrepancies changes	*/
/*------------------------------------------------------------------------------*/
use fe_full.dta, clear
xtset firm_code year, yearly
replace emis_ratio_trans = (-1) * emis_ratio_trans if year == 2012		// Change the sign of 2012 to make the meaning of sign consistent when estimating fixed effects 
gen diff_emis_ratio_trans = emis_ratio_trans - L.emis_ratio_trans
bysort year: reg diff_emis_ratio_trans i.change, robust

/*-------------------------------------------------------------------------------------------------------*/
/* Table 1 - Absolute value of adjusted discrepancies and number of errors found by verifiers in Beijing */
/*-------------------------------------------------------------------------------------------------------*/
use fe_full.dta, clear
xtset firm_code year, yearly
gen abs_emis_ratio_trans = abs(emis_ratio_trans)

// Column 1:
xtreg abs_emis_ratio_trans i.verifierid i.year, fe vce(cluster firm_code)

// Column 2:
xtreg abs_emis_ratio_trans i.verifierid t, fe vce(cluster firm_code)

// Column 3:
gen abs_emis_ratio_trans_1 = abs(emis_ratio_trans)
replace abs_emis_ratio_trans_1 = 0 if emis_ratio_trans < 0 & year == 2012
replace abs_emis_ratio_trans_1 = 0 if emis_ratio_trans > 0 & year != 2012
xtreg abs_emis_ratio_trans_1 i.verifierid i.year, fe vce(cluster firm_code)

// Column 4:
xtreg abs_emis_ratio_trans i.verifierid error_num i.year, fe vce(cluster firm_code)

// Column 5:
xtreg error_num i.verifierid i.year, fe vce(cluster firm_code)

// Column 6:
xtreg abs_emis_ratio_trans error_type2_1_num error_type2_2_num error_type2_3_num i.verifierid i.year, fe vce(cluster firm_code)
*reghdfe abs_emis_ratio_trans error_type2_1_num error_type2_2_num error_type2_3_num i.year, absorb(verifierid firm_code) vce(cluster verifierid firm_code)
*clusterbs regress abs_emis_ratio_trans error_type2_1_num error_type2_2_num error_type2_3_num yr13 yr14 yr15, cluster(verifier_name) fe(external) reps(2000) seed(20202) festruc(firm_code) // Pairs cluster bootstrap-t for few clusters

/*--------------------------------------------------------------*/
/* Supplementary Table 6 - Effect of the tenure of verifiers    */
/*--------------------------------------------------------------*/
use fe_full.dta, clear
xtset firm_code year, yearly
bysort firm_code verifierid (year): gen year_same_verifier = _n-1
save fe_full.dta, replace
xtset firm_code year, yearly
xtreg emis_ratio_trans year_same_verifier i.year i.verifierid, fe vce(cluster firm_code)
gen same_verifier_four_year = cond(year_same_verifier == 3, 1, 0)
xtreg emis_ratio_trans same_verifier_four_year i.year i.verifierid, fe vce(cluster firm_code)

/*------------------------------------------------------------------------------*/
/* Supplementary Table 7 - Correlation between firm fe and firm characteristics */
/*------------------------------------------------------------------------------*/
// Column 1:
use fe_full.dta, clear
// sector_code 1 "火力发电企业"  2 "热力生产和供应企业" 3 "水泥制造企业" 4 "石化生产企业" 5 "其他工业企业" 6 "服务业企业（单位）"
// ownership_code 1 "行政机关"  2 "事业单位" 3 "国有企业" 4 "集体企业" 5 "私营企业" 6 "港澳台资企业" 7 "外资企业" 8 "合资企业" 
replace ownership = 1 if ownership == 2
gen log_emis_total_verified_ton = log(emis_total_verified_ton)
replace emis_ratio_trans = (-1) * emis_ratio_trans if year == 2012		// Change the sign of 2012 to make the meaning of sign consistent when estimating fixed effects 

bysort firm_code: gen index = _n
bysort firm_code: gen firm_code1 = 1 if index == 1
replace firm_code1 = sum(firm_code1)
drop index

xtset firm_code1 year, yearly
xtreg emis_ratio_trans i.year i.verifierid i.firm_code1, vce(cluster firm_code1)
matrix b1 = e(b)
matrix v1 = e(V)
quiet gen coef_firm = .
quiet gen std_firm = .
forval f = 1/403{
  quiet replace coef_firm = b1[1, 26 +`f'] if firm_code1 == `f'
  quiet replace std_firm = sqrt(v1[26 +`f', 26 +`f']) if firm_code1 == `f'
}
table firm_code1, contents(m coef_firm)
table firm_code1, contents(m std_firm)

collapse peffhat coef_firm std_firm (mean) output_value_10000_yuan log_emis_total_verified_ton emis_ratio_trans sector (min) ownership, by(firm_code)
*reg peffhat log_emis_total_verified_ton i.sector i.ownership, robust
reg coef_firm log_emis_total_verified_ton i.sector i.ownership [aweight=1/(std_firm*std_firm)], robust 

// Column 2:
use fe_full.dta, clear
replace ownership = 1 if ownership == 2
gen log_emis_total_verified_ton = log(emis_total_verified_ton)

preserve
gen abs_emis_ratio_trans = abs(emis_ratio_trans)
collapse (mean) abs_emis_ratio_trans output_value_10000_yuan log_emis_total_verified_ton emis_ratio_trans sector (min) ownership, by(firm_code)
reg abs_emis_ratio_trans log_emis_total_verified_ton i.sector i.ownership, robust
restore
