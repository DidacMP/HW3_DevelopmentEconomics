*------------------------------- Question 1 -----------------------------------*
* Author: Dídac Martí Pinto
* CEMFI
* Date: 01/03/2019
*------------------------------------------------------------------------------*

clear
cd "C:\Users\Didac\Desktop\development_hw3\"
use "data\dataUGA.dta"

// 0. Summary useful variables and data preparation ////////////////////////////

* hh: identifier
* year: year
* lninctotal_trans: log income
* lnc: log consumption
* age: age
* age_sq: age squared
* familysize: family size
* ethnic: ethnic group
* female: female indicator
* urban: urban/rural indicator

*** Keep only urban households
keep if urban==0

*** Solve repeated years problem (as Javier mentioned in the mail)

* Repeated 2011's
bysort year hh: gen yeardups = _N // 262 repeated years
replace year = 2010 if wave=="2010-2011" & year==2011 & yeardups==2
drop yeardups
 
* Repeated 2010's
bysort year hh: gen yeardups = _N // 218 repeated years
replace year = 2009 if wave=="2009-2010" & year==2010 & yeardups==2
drop yeardups

 
// 1. Obtain residuals /////////////////////////////////////////////////////////

 *** Residuals for consumption
 reg lnc age age_sq familysize i.year i.ethnic i.sex i.urban
 predict res
 rename res res_c
 
 *** Construct residuals for income
 reg lninctotal_trans age age_sq familysize i.year i.ethnic i.sex i.urban
 predict res
 rename res res_inc
 rename lninctotal_trans income


// 2. Anualize growth rates linearly ///////////////////////////////////////////

*** Aggregate consumption
egen agg_c = sum(lnc), by(year)
 
*** Transform data into a panel
keep res_c res_inc agg_c hh year income
xtset hh year
reshape wide res_c res_inc agg_c income, i(hh) j(year)

*** Aggregate values of consumption
* 2010-2014
forvalues y = 10(1)14 {
egen agg_c20`y'_t = mean(agg_c20`y')
drop agg_c20`y'
rename agg_c20`y'_t agg_c20`y'
}
* 2009
egen agg_c2009_t = mean(agg_c2009)
drop agg_c2009
rename agg_c2009_t agg_c2009

reshape long res_c res_inc agg_c income, i(hh)
rename _j year

ipolate res_c year, generate(res_ci) epolate by(hh)
ipolate res_inc year, generate(res_inci) epolate by(hh)
ipolate income year, generate(income_i) epolate by(hh)

*** Drop if we only have the hh in 1 wave or less
gen ones = 1
replace ones = 0 if res_ci ==.
egen numyears = sum(ones), by(hh)
drop if numyears <= 1
drop res_c res_inc ones numyears


// 3. Question 1 - Regressions /////////////////////////////////////////////////

*** Create a simplified identifier for each individual 
sort hh year
egen id = group(hh) 

*** Regressions with household individualized coefficients
generate beta = .
generate phi = .
forvalues i = 1(1)2239 {
	reg d.res_ci d.res_inci d.agg_c if id==`i', nocons
	replace beta = _b[d.res_inci] if id==`i'
	replace phi = _b[d.agg_c] if id==`i'
}
*

// 4. Question 1 - Results /////////////////////////////////////////////////////

*** Histogram of betas
preserve
	collapse beta phi, by(hh)
* Trimming
	drop if beta<-2
	drop if beta>2
* Histogram
	histogram beta, title("Coefficient beta across households", color(black)) ///
	xtitle ("Betas") graphregion(color(white)) bcolor(maroon)
	graph export "results\q4_betas_rural.png", replace
	* Summary statistics
	sum beta, detail
restore

*** Histogram of phis
preserve
	collapse beta phi, by(hh)
	* Trimming	
	drop if phi > 0.00002
	drop if phi < -0.00002
	* Histogram
	histogram phi, title("Coefficient phi across households", color(black)) ///
	xtitle ("Phis") graphregion(color(white)) bcolor(maroon)
	graph export "results\q4_phis_rural.png", replace
	* Summary statistics
	sum phi, detail
restore

// 5. Question 3 - Regressions /////////////////////////////////////////////////

*** Regressions with coefficients not individualized
reg d.res_ci d.res_inci d.agg_c, nocons
display _b[d.res_inci]
display _b[d.agg_c]
*outreg2 using "results\q4_rural.xls"


// 6. Question 2 ///////////////////////////////////////////////////////////////

*** Average household
gen ones = 1
replace ones = 0 if income_i ==.
egen numyears = sum(ones), by(hh)
drop if numyears <= 1
drop ones numyears income
collapse (mean) income_i beta, by(hh)

*** Create a variable with the income group of each individual
sort income_i
gen nobs = _N
gen nhh = _n 
 
*** Compute mean and median of beta within of each income group
gen inc_group = 0
replace inc_group = 1 if nhh<=445
replace inc_group = 2 if nhh>445 & nhh<=890
replace inc_group = 3 if nhh>890 & nhh<=1335
replace inc_group = 4 if nhh>1335 & nhh<=1780
replace inc_group = 5 if nhh>1780 & nhh<=2226
 
*** Compute mean and median betas
forvalues i = 1(1)5 {
	sum beta if inc_group==`i', detail
 }
drop nhh
 
*** Define five beta groups
sort beta
gen nhh = _n 

*** Compute mean and median of income within within each beta group
gen beta_group = 0
replace beta_group = 1 if nhh<=445
replace beta_group = 2 if nhh>445 & nhh<=890
replace beta_group = 3 if nhh>890 & nhh<=1335
replace beta_group = 4 if nhh>1335 & nhh<=1780
replace beta_group = 5 if nhh>1780 & nhh<=2226
 
*** Compute mean and median betas
forvalues i = 1(1)5 {
sum income_i if beta_group==`i', detail
 }
*


