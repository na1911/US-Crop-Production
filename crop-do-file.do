capture log close
log using "Stata-Code/cropanalysis", text

// PROJECT: Climate Change and US Crop Production
// PROGRAM: crop-do-file.do
// AUTHOR: Carmen Nguyen
// MAY 2023

version 16.1
clear all
set more off


use "Stata-Code/cropdata.dta"
* Data sources: described in attached writing sample

label variable crop "Crop Production"
label variable pop "Total Population"
label variable white "White/Population Ratio"
label variable black "Black/Population Ratio"
label variable female "Female/Total Population Ratio"
label variable bachelor "People Getting Bachelor's Degree or Higher"
label variable politics "Red or Blue"
label variable politics "Red - 0, Blue - 1"
label variable ue_rate "Unemployment Rate"
label variable ue_rate "Unemployment Rate (%)"
label variable min_wage "Minimum Wage (USD)"
label variable med_hh_in "Median Household Income (USD)"
label variable poverty "Poverty Rate"
label variable pdsi "Palmer Drought Severity Index"
label variable hdd "Heating Degree Day"

***** DATA PROCESSING

*** Set up panel data
encode state, gen(stateid)
xtset stateid year

*** Create a categorical variable for region
gen regions = 1 * inlist(state, "CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA") + 2 * inlist(state, "IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO") + 2 * inlist(state, "NE", "ND", "SD") + 3 * inlist(state, "DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL") + 3 * inlist(state, "KY", "MS", "TN", "AR", "LA", "OK", "TX") + 4 * inlist(state, "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK") + 4 * inlist(state, "CA", "HI", "OR", "WA")

label def gnames ///
       1 "Northeast"  ///
       2 "Midwest"  ///
       3 "South" ///
	   4 "West" ///
	   
label val regions gnames

decode regions, gen(region)

*** Checking variable crop
xtline crop, overlay legen(off) xlabel(2007(1)2020) legend(off) ytitle(Crop Production) xtitle(Year) title(Crop Production) name(crop_by_state)
replace crop=. if state=="LA" & year==2015
hist crop, xtitle(Crop Production) normal name(hist_crop)
gen lncrop = ln(crop)
label variable lncrop "Natural Log Transformation of Crop Production"
hist lncrop, xtitle(Log-transformed Crop Production) normal name(hist_lncrop)

*** Checking correlation of PDSI and HDD using Pearson's coefficient
twoway scatter pdsi hdd, xtitle(HDD) ytitle(PDSI) name(hdd_pdsi_scatter)
hist pdsi, xtitle(PDSI) normal name(hist_pdsi)
hist hdd, xtitle(HDD) normal name(hist_hdd) 
* Pearson's correlation coefficient
pwcorr pdsi hdd, sig star(.05) obs

*** Checking variable pop
hist pop, xtitle(Total Population) normal name(hist_pop)
gen lnpop = ln(pop)
label variable lncrop "Natural Log Transformation of Total Population"
hist lnpop, xtitle(Log-transformed Total Population) normal name(hist_lnpop)

*** Descriptive statistics
summarize

*** Regional graphs
xtline hdd if region=="Northeast", overlay xlabel(2007(5)2020) ylabel(0(4000)10000) ytitle(HDD) legend(off) title(Northeast) name(hdd_northeast) nodraw
xtline hdd if region=="Midwest", overlay xlabel(2007(5)2020) ylabel(0(4000)10000) ytitle(HDD) legend(off) title(Midwest) name(hdd_midwest) nodraw
xtline hdd if region=="South", overlay xlabel(2007(5)2020) ylabel(0(4000)10000) ytitle(HDD) legend(off) title(South) name(hdd_south) nodraw
xtline hdd if region=="West", overlay xlabel(2007(5)2020) ylabel(0(4000)10000) ytitle(HDD) legend(off) title(West) name(hdd_west) nodraw
graph combine hdd_northeast hdd_midwest hdd_south hdd_west, name(hdd_by_region)

xtline pdsi if region=="Northeast", overlay xlabel(2007(5)2020) ylabel(-6(2)10) ytitle(PDSI) legend(off) title(Northeast) name(pdsi_northeast) nodraw
xtline pdsi if region=="Midwest", overlay xlabel(2007(5)2020) ylabel(-6(2)10) ytitle(PDSI) legend(off) title(Midwest) name(pdsi_midwest) nodraw
xtline pdsi if region=="South", overlay xlabel(2007(5)2020) ylabel(-6(2)10) ytitle(PDSI) legend(off) title(South) name(pdsi_south) nodraw
xtline pdsi if region=="West", overlay xlabel(2007(5)2020) ylabel(-6(2)10) ytitle(PDSI) legend(off) title(West) name(pdsi_west) nodraw
graph combine pdsi_northeast pdsi_midwest pdsi_south pdsi_west, name(pdsi_by_reion)

***** ANALYSIS

*** Simple OLS regressions

* Model 1
regress lncrop pdsi hdd
estimates table, star(.1 .05 .01) title(OLS 1)

* Model 2
regress lncrop pdsi hdd lnpop white black female bachelor 
estimates table, star(.1 .05 .01) title(OLS 2)

* Model 3
regress lncrop pdsi hdd lnpop white black female bachelor ue_rate min_wage med_hh_in poverty
estimates table, star(.1 .05 .01) title(OLS 3)

* Model 4
regress lncrop pdsi hdd lnpop white black female bachelor ue_rate min_wage med_hh_in poverty politics
estimates table, star(.1 .05 .01) title(OLS 4)

*** Fixed-effects regressions

ssc install reghdfe

* Model 1
reghdfe lncrop pdsi hdd, absorb(state year) vce(robust)
estimates table, star(.1 .05 .01) title(FE 1)

* Model 2
reghdfe lncrop pdsi hdd lnpop white black female bachelor, absorb(state year) vce(robust)
estimates table, star(.1 .05 .01) title(FE 2)

* Model 3
reghdfe lncrop pdsi hdd lnpop white black female bachelor ue_rate min_wage med_hh_in poverty, absorb(state year) vce(robust)
estimates table, star(.1 .05 .01) title(FE 3)

* Model 4
reghdfe lncrop pdsi hdd lnpop white black female bachelor ue_rate min_wage med_hh_in poverty politics, absorb(state year) vce(robust)
estimates table, star(.1 .05 .01) title(FE 4)

*** Regional fixed-effects regressions

* Northeast
reghdfe lncrop pdsi hdd lnpop white black female bachelor ue_rate min_wage med_hh_in poverty politics if region=="Northeast",absorb(state year) vce(robust)
estimates table, star(.1 .05 .01) title(Northeast)

* Midwest
reghdfe lncrop pdsi hdd lnpop white black female bachelor ue_rate min_wage med_hh_in poverty politics if region=="Midwest", absorb(state year) vce(robust)
estimates table, star(.1 .05 .01) title(Midwest)

* South
reghdfe lncrop pdsi hdd lnpop white black female bachelor ue_rate min_wage med_hh_in poverty politics if region=="South", absorb(state year) vce(robust)
estimates table, star(.1 .05 .01) title(South)

* West
reghdfe lncrop pdsi hdd lnpop white black female bachelor ue_rate min_wage med_hh_in poverty politics if region=="West", absorb(state year) vce(robust)
estimates table, star(.1 .05 .01) title(West)

*** FE with lagged variables
xtset stateid year
gen lag_pdsi = L.pdsi
gen lag_hdd = L.hdd
label variable lag_pdsi "1 year lagged PDSI"
label variable lag_hdd "1 year lagged HDD"

reghdfe lncrop lag_pdsi lnpop white black female bachelor ue_rate min_wage med_hh_in poverty politics, absorb(state year) vce(robust)
estimates table, star(.1 .05 .01) title(Lagged PDSI)

reghdfe lncrop lag_hdd lnpop white black female bachelor ue_rate min_wage med_hh_in poverty politics, absorb(state year) vce(robust)
estimates table, star(.1 .05 .01) title(Lagged HDD)

********************

* Save the manipulated dataset
save "Stata-Code/cropdata2.dta"
log close
