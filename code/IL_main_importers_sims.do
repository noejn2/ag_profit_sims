*** -- Replications of estimates in Table 4 
*** Dall'Erba, Chen and Nava (2021)
*** noejn2@illinois.edu

clear all
cd "[set-up working directory]ag_profit_sims\"
use "data\data4stage1", clear

/* Retrieving difference in total exports for each state */
*** estimation (full year)
ppml flowtotalvalue share_border traveltime_ln F_* ln_* home_* EXP_CLMT_TIME_FE* IMP_CLMT_TIME_FE* CLMT_PAIR_FE*, cluster(traveltime)
predict yhat

*** simulation: drought effects come from condutions to the middle of the century
ppml flowtotalvalue share_border traveltime_ln F_* ln_* home_* EXP_CLMT_TIME_FE* IMP_CLMT_TIME_FE* CLMT_PAIR_FE*, cluster(traveltime)
replace F_destsvdrtf = F_destsvdrtf + 3.6363636 if year == 2012 & dest == "LOUISIANA"
replace F_destsvdrtf = F_destsvdrtf + 2.1212121 if year == 2012 & dest == "MISSOURI"

replace F_origsvdrtf = F_origsvdrtf + 3.6363636 if year == 2012 & orig == "LOUISIANA"
replace F_origsvdrtf = F_origsvdrtf + 2.1212121 if year == 2012 & orig == "MISSOURI"

predict yhat_sim

keep orig dest year yhat yhat_sim

*** create predicted export for each state
keep if orig != dest
collapse (sum) totExport = yhat (sum) totExport_sim = yhat_sim, by(year orig)
rename orig state
replace state = lower(state)           /* convert upper case to lower case */
replace state = regexr(state,"_"," ")  /* replace "_" to " " in the string of state */

/*** merge with main data set for stage 2 ***/
merge 1:1 state year using "data/data4stage2"
drop _merge

*** rescale variables
replace income = income/1000

/* Table 4 - Replication */
*** panel data setup
xtset fips year

*** Column 4: Used for forecast with trade
xtreg crop_profit SVdrought_full totExport c.gdd##c.gdd c.gtp##c.gtp c.income##c.income c.density##c.density i.year#i.noaa_region, fe

gen trade_diff_sim = totExport_sim - totExport
* Competitor simulations
putexcel set "output/ag_profit_sims_IL.xlsx", modify sheet(importers)
putexcel A1 =("state")      /* colume A: state name */
putexcel B1 =("estimate")  /* colume B: estimates for the climate impact */
putexcel C1 =("SE")  /* colume C: std. dev for the climate impact */

local j = 2
local k = _N
forvalue i = 4(4)`k' {

		* create changes in trade because of drought days
		scalar trade_diff_sim_`i' = trade_diff_sim[`i']
		
		* create the impact of climate change
		lincom totExport * trade_diff_sim_`i'
		
		scalar estimate = (r(estimate))
		scalar se = (r(se))
		
		* record results in the spreadsheet
		putexcel A`j' = (state[`i'])   /* colume A: state names */
		putexcel B`j' = estimate       /* colume B: estimates for the climate impact */
		putexcel C`j' = se             /* colume C: std. dev for the climate impact */
		local j = `j' + 1
}
*end