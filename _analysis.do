* ------------------------------------------------------------------------------
* import data from Excel table
* ------------------------------------------------------------------------------

clear
cd "/Users/jcbarnes/Box Sync/manuscripts/_underReview/power_--_jq/data"
import excel "_table.xlsx", sheet("Sheet1") firstrow case(lower)
describe 
tab1 micro
gen macro = micro
drop micro


* count the total number of studies included in the meta-analyses
egen totalstudies = total(kofstudies) if kofstudies!=.
sum totalstudies







* ------------------------------------------------------------------------------
* convert all ESs to correlation (r); absolute value transform
* ------------------------------------------------------------------------------

capture drop es

* gen effect size (es) variable that is = r
gen es = r 

* r --> r
foreach i in zr phi  {
replace es = `i' if es==. & `i'!=.
}

* or & rr --> d  , then d --> r
replace loggedor = ln(oddsratioor) if loggedor==. & oddsratioor!=.
replace loggedor = ln(riskratio)   if loggedor==. & riskratio!=.
replace d = loggedor*(sqrt(3)/_pi) if d==. & loggedor!=.

* d (which now includes or & rr) --> r
foreach i in d hedgesg {
replace es = sqrt((`i'^2)/(4+(`i'^2))) if es==. & `i'!=.
}

* absolute value transformation
capture drop asAbs
gen esAbs = abs(es)
sum es esAbs, d








* ------------------------------------------------------------------------------
* effect sizes
* ------------------------------------------------------------------------------

* clear screen
cls

* histogram
sum esAbs, d
local m =`r(mean)'
local esMedian=`r(p50)'
local esp25=round(`r(p25)',0.001)
local esp75=round(`r(p75)',0.001)
twoway (hist esAbs, freq ///
	plotregion(color(white)) ///
	graphregion(color(white)) ///
	title("| {it:r} |", position(11) color(gs5)) ///
	ytitle("Frequency", color(gs5)) xtitle("") ///
	ylabel(, noticks labcolor(gs5)) ///
	xlabel(0(.2)1.00, grid gmax gmin noticks labcolor(gs5)) ///
	yscale(noline) ///
	xscale(lcolor(gs5) lw(medthick)) ///
	fcolor(gs11) lcolor(gs5)) ///
	(scatteri 0 `esMedian', msymbol(Dh) mcolor(black) ) ///
	(scatteri 0 `esp25', msymbol(Th) mcolor(black) ) ///
	(scatteri 0 `esp75', msymbol(Th) mcolor(black) ) ///
	(scatteri 0 `m', msymbol(Oh) mcolor(black) ), ///
	legend(off) ///
	name(histesAbs, replace)
gr export "histES.pdf", replace		








* ------------------------------------------------------------------------------
*  ns
* ------------------------------------------------------------------------------

* calculate average n if not already available
capture drop n
gen n = averagen if es!=.
replace n = totaln/k if n==. & es!=.
sum n
tab n
* more than 90% are n<=5,000

* histogram
sum n if n<=5000, d
local nm=`r(mean)'
local nMedian=`r(p50)'
local np25=round(`r(p25)',0.001)
local np75=round(`r(p75)',0.001)
twoway (hist n if n<=5000, freq ///
	plotregion(color(white)) ///
	graphregion(color(white)) ///
	title("Average {it:n}, where {it:n} {&le} 5,000", position(11) color(gs5)) ///
	ytitle("Frequency", color(gs5)) xtitle("") ///
	ylabel(, noticks labcolor(gs5)) ///
	xlabel(0(250)5000, grid gmax gmin noticks labcolor(gs5) angle(45)) ///
	yscale(noline) ///
	xscale(lcolor(gs5) lw(medthick)) ///
	fcolor(gs11) lcolor(gs5) ///
	bin(25)) ///
	(scatteri 0 `nMedian', msymbol(Dh) mcolor(black) ) ///
	(scatteri 0 `np25', msymbol(Th) mcolor(black) ) ///
	(scatteri 0 `np75', msymbol(Th) mcolor(black) ) ///
	(scatteri 0 `nm', msymbol(Oh) mcolor(black) ), ///
	legend(off) ///
	name(histN, replace)
graph export "histN.pdf", replace










* ------------------------------------------------------------------------------
* table 1
* ------------------------------------------------------------------------------

* ----------------------------------------
* all studies
* ----------------------------------------

* clear screen
cls

* effect size
sum esAbs, d
egen esAbsMode=mode(esAbs), maxmode
tab esAbsMode
centile esAbs, centile(2.5 97.5)

* n
sum n , d
egen nMode=mode(n), maxmode
tab nMode
centile n, centile(2.5 97.5)

* n<=5,000
sum n if n<=5000, d
capture drop nMode
egen nMode=mode(n) if n<=5000, maxmode
tab nMode
centile n if n<=5000, centile(2.5 97.5)

* ----------------------------------------









* ----------------------------------------
* intervention & non-intervention studies
* ----------------------------------------

* clear screen
cls

* intervention studies
preserve
keep if intervention==1
capture drop esAbsMode nMode

* effect size
sum esAbs, d
egen esAbsMode=mode(esAbs), maxmode
tab esAbsMode
centile esAbs, centile(2.5 97.5)

* n
sum n , d
egen nMode=mode(n), maxmode
tab nMode
centile n, centile(2.5 97.5)

* n<=5,000
sum n if n<=5000, d
capture drop nMode
egen nMode=mode(n) if n<=5000, maxmode
tab nMode
centile n if n<=5000, centile(2.5 97.5)
restore

* ----------------------------------------




* clear screen
cls

* non-intervention studies
preserve
keep if intervention==0
capture drop esAbsMode nMode

* effect size
sum esAbs, d
egen esAbsMode=mode(esAbs), maxmode
tab esAbsMode
centile esAbs, centile(2.5 97.5)

* n
sum n , d
egen nMode=mode(n), maxmode
tab nMode
centile n, centile(2.5 97.5)

* n<=5,000
sum n if n<=5000, d
capture drop nMode
egen nMode=mode(n) if n<=5000, maxmode
tab nMode
centile n if n<=5000, centile(2.5 97.5)
restore

* ----------------------------------------



* ES histogram
twoway ///
	(hist esAbs if intervention==0, freq bin(10) ///
	plotregion(color(white)) ///
	graphregion(color(white)) ///
	title("Panel A:" "| {it:r} |", position(11) color(gs5)) ///
	ytitle("Non-Intervention (Frequency)", color(gs5) axis(1)) xtitle("") ///
	ylabel(, noticks labcolor(gs5) axis(1)) ///
	yaxis(1) yscale(noline axis(1)) ///
	xlabel(0(.2)1.00, grid gmax gmin noticks labcolor(gs5)) ///
	xscale(lcolor(gs5) lw(medthick)) ///
	fcolor(gs8) lcolor(gs5) ) ///
	(hist esAbs if intervention==1, freq bin(10) ///
	yaxis(2) yscale(noline axis(2)) ///
	ytitle("Intervention (Frequency)", color(gs5) axis(2)) ///
	ylabel(, noticks labcolor(gs5) axis(2) grid) ///
	fcolor(gs13) lcolor(gs5) ) , ///
	legend(lab(1 "Non-Intervention") lab(2 "Intervention") pos(2) ring(0) rows(2) region(lcolor(white))) ///
	name(histESAbsInt, replace)
gr export "histES_int.pdf", replace



* n histogram
twoway ///
	(hist n if intervention==0 & n<=5000, freq bin(10) ///
	plotregion(color(white)) ///
	graphregion(color(white)) ///
	title("Panel B:" "Average {it:n}, where {it:n} {&le} 5,000", position(11) color(gs5)) ///
	ytitle("Non-Intervention (Frequency)", color(gs5) axis(1)) xtitle("") ///
	ylabel(, noticks labcolor(gs5) axis(1)) ///
	yaxis(1) yscale(noline axis(1)) ///
	xlabel(0(1000)5000, grid gmax gmin noticks labcolor(gs5)) ///
	xscale(lcolor(gs5) lw(medthick)) ///
	fcolor(gs8) lcolor(gs5) ) ///
	(hist n if intervention==1 & n<=5000, freq bin(10) ///
	yaxis(2) yscale(noline axis(2)) ///
	ytitle("Intervention (Frequency)", color(gs5) axis(2)) ///
	ylabel(, noticks labcolor(gs5) axis(2) grid) ///
	fcolor(gs13) lcolor(gs5) ) , ///
	legend(lab(1 "Non-Intervention") lab(2 "Intervention") pos(1) ring(0) rows(2) region(lcolor(white))) ///
	name(histNInt, replace)
gr export "histN_int.pdf", replace

* ----------------------------------------

















* ----------------------------------------
* individual-level & macro-level studies
* ----------------------------------------

* clear screen
cls

* individual-level studies 
preserve
keep if macro==0
capture drop esAbsMode nMode

* effect size
sum esAbs, d
egen esAbsMode=mode(esAbs), maxmode
tab esAbsMode
centile esAbs, centile(2.5 97.5)

* n
sum n , d
egen nMode=mode(n), maxmode
tab nMode
centile n, centile(2.5 97.5)

* n<=5,000
sum n if n<=5000, d
capture drop nMode
egen nMode=mode(n) if n<=5000, maxmode
tab nMode
centile n if n<=5000, centile(2.5 97.5)
restore

* ----------------------------------------





* clear screen
cls

* macro-level studies
preserve
keep if macro==1
capture drop esAbsMode nMode

* effect size
sum esAbs, d
egen esAbsMode=mode(esAbs), maxmode
tab esAbsMode
centile esAbs, centile(2.5 97.5)

* n
sum n , d
egen nMode=mode(n), maxmode
tab nMode
centile n, centile(2.5 97.5)

* n<=5,000
sum n if n<=5000, d
capture drop nMode
egen nMode=mode(n) if n<=5000, maxmode
tab nMode
centile n if n<=5000, centile(2.5 97.5)
restore

* ----------------------------------------


* ES histogram
twoway ///
	(hist esAbs if macro==0, freq bin(10) ///
	plotregion(color(white)) ///
	graphregion(color(white)) ///
	title("Panel A:" "| {it:r} |", position(11) color(gs5)) ///
	ytitle("Individual-Level (Frequency)", color(gs5) axis(1)) xtitle("") ///
	ylabel(, noticks labcolor(gs5) axis(1)) ///
	yaxis(1) yscale(noline axis(1)) ///
	xlabel(0(.2)1.00, grid gmax gmin noticks labcolor(gs5)) ///
	xscale(lcolor(gs5) lw(medthick)) ///
	fcolor(gs8) lcolor(gs5) ) ///
	(hist esAbs if macro==1, freq bin(10) ///
	yaxis(2) yscale(noline axis(2)) ///
	ytitle("Macro-Level (Frequency)", color(gs5) axis(2)) ///
	ylabel(, noticks labcolor(gs5) axis(2) grid) ///
	fcolor(gs13) lcolor(gs5) ) , ///
	legend(lab(1 "Individual-Level") lab(2 "Macro-Level") pos(2) ring(0) rows(2) region			(lcolor(white))) ///
	name(histESAbsMacro, replace)
gr export "histES_macro.pdf", replace



* n histogram
twoway ///
	(hist n if macro==0 & n<=5000, freq bin(10) ///
	plotregion(color(white)) ///
	graphregion(color(white)) ///
	title("Panel B:" "Average {it:n}, where {it:n} {&le} 5,000", position(11) color(gs5)) ///
	ytitle("Individual-Level (Frequency)", color(gs5) axis(1)) xtitle("") ///
	ylabel(, noticks labcolor(gs5) axis(1)) ///
	yaxis(1) yscale(noline axis(1)) ///
	xlabel(0(1000)5000, grid gmax gmin noticks labcolor(gs5)) ///
	xscale(lcolor(gs5) lw(medthick)) ///
	fcolor(gs8) lcolor(gs5) ) ///
	(hist n if macro==1 & n<=5000, freq bin(10) ///
	yaxis(2) yscale(noline axis(2)) ///
	ytitle("Macro-Level (Frequency)", color(gs5) axis(2)) ///
	ylabel(, noticks labcolor(gs5) axis(2) grid) ///
	fcolor(gs13) lcolor(gs5) ) , ///
	legend(lab(1 "Individual-Level") lab(2 "Macro-Level") pos(1) ring(0) rows(2) region			(lcolor(white))) ///
	name(histNmacro, replace)
gr export "histN_macro.pdf", replace

* ----------------------------------------





















* ------------------------------------------------------------------------------
* statistical power (1-B)
* ------------------------------------------------------------------------------

* clear screen
cls

* calculate statistical power for each ES & n combo
preserve
capture drop roundn
gen roundn = round(n)
capture drop study
gen study = _n
drop if esAbs==. | esAbs==0 | roundn==.
capture gen power = .
levelsof study, local(levels)
quietly foreach i of local levels {
sum esAbs if study==`i' 
local meanES=`r(mean)'
sum roundn if study==`i'
local meanN=`r(mean)'
power onecorr 0 `meanES', n(`meanN') alpha(0.05) table
replace power = `r(power)' if study==`i'
}


* ----------------------------------------
* all studies
* ----------------------------------------

* histogram
quietly sum power, d
local sm=`r(mean)'
local sMedian=`r(p50)'
local sp25=round(`r(p25)',0.001)
local sp75=round(`r(p75)',0.001)
twoway (hist power , freq ///
	plotregion(color(white)) ///
	graphregion(color(white)) ///
	title("Statistical Power (1-{&beta})", position(11) color(gs5)) ///
	ytitle("Frequency", color(gs5)) xtitle("") ///
	ylabel(, noticks labcolor(gs5)) ///
	xlabel(, grid gmax gmin noticks labcolor(gs5)) ///
	yscale(noline) ///
	xscale(lcolor(gs5) lw(medthick)) ///
	fcolor(gs11) lcolor(gs5)) ///
	(scatteri 0 `sMedian', msymbol(Dh) mcolor(black) ) ///
	(scatteri 0 `sp25', msymbol(Th) mcolor(black) ) ///
	(scatteri 0 `sp75', msymbol(Th) mcolor(black) ) ///
	(scatteri 0 `sm', msymbol(Oh) mcolor(black) ), ///
	legend(off) ///
	name(histPower, replace)
gr export "histPower.pdf", replace


* summary statistics for table 1
tab power
sum power, d
egen powerMode=mode(power), maxmode
tab powerMode
centile power, centile(2.5 97.5)

* ----------------------------------------










* ----------------------------------------
* interventions & non-interventions
* ----------------------------------------

* clear screen
cls

* interventions
sum power if intervention1==1, d
capture drop powerMode
egen powerMode=mode(power) if intervention1==1, maxmode
tab powerMode if intervention1==1
centile power if intervention1==1, centile(2.5 97.5)

* non-interventions
sum power if intervention1==0, d
capture drop powerMode
egen powerMode=mode(power) if intervention1==0, maxmode
tab powerMode if intervention1==0
centile power if intervention1==0, centile(2.5 97.5)

/* combined histogram
twoway ///
	(hist power if intervention==0, freq bin(10) ///
	plotregion(color(white)) ///
	graphregion(color(white)) ///
	title("Panel : Statistical Power", position(11) color(gs5)) ///
	ytitle("Non-Intervention (Frequency)", color(gs5) axis(1)) xtitle("") ///
	ylabel(, noticks labcolor(gs5) axis(1)) ///
	yaxis(1) yscale(noline axis(1)) ///
	xlabel(, grid gmax gmin noticks labcolor(gs5)) ///
	yscale(noline) ///
	xscale(lcolor(gs5) lw(medthick)) ///
	fcolor(gs8) lcolor(gs5) ) ///
	///
	(hist power if intervention==1, freq bin(10) ///
	yaxis(2) yscale(noline axis(2)) ///
	ytitle("Intervention (Frequency)", color(gs5) axis(2)) ///
	ylabel(, noticks labcolor(gs5) axis(2) grid) ///
	fcolor(gs13) lcolor(gs5) ) , ///
	legend(lab(1 "Non-Intervention") lab(2 "Intervention") pos(12) ring(0) stack region(lcolor(white))) ///
	name(histPowerInt, replace)
gr export "histPower_int&non-int.pdf", replace
*/

* interventions only
quietly sum power if intervention==1, d
local sm=`r(mean)'
local sMedian=`r(p50)'
local sp25=round(`r(p25)',0.001)
local sp75=round(`r(p75)',0.001)
twoway (hist power if intervention==1, freq bin(10) ///
	plotregion(color(white)) ///
	graphregion(color(white)) ///
	title("Panel C:" "Statistical Power, Intervention", position(11) color(gs5)) ///
	ytitle("Frequency", color(gs5)) xtitle("") ///
	ylabel(, noticks labcolor(gs5)) ///
	xlabel(, grid gmax gmin noticks labcolor(gs5)) ///
	yscale(noline) ///
	xscale(lcolor(gs5) lw(medthick)) ///
	fcolor(gs13) lcolor(gs5) ) ///
	(scatteri 0 `sMedian', msymbol(Dh) mcolor(black) ) ///
	(scatteri 0 `sp25', msymbol(Th) mcolor(black) ) ///
	(scatteri 0 `sp75', msymbol(Th) mcolor(black) ) ///
	(scatteri 0 `sm', msymbol(Oh) mcolor(black) ), ///
	legend(off) ///
	name(histPowerIntOnly, replace)
gr export "histPower_int-only.pdf", replace



* non-interventions only
quietly sum power if intervention==0, d
local sm=`r(mean)'
local sMedian=`r(p50)'
local sp25=round(`r(p25)',0.001)
local sp75=round(`r(p75)',0.001)
twoway (hist power if intervention==0, freq bin(10) ///
	plotregion(color(white)) ///
	graphregion(color(white)) ///
	title("Panel D:" "Statistical Power, Non-Intervention", position(11) color(gs5)) ///
	ytitle("Frequency", color(gs5)) xtitle("") ///
	ylabel(, noticks labcolor(gs5)) ///
	xlabel(, grid gmax gmin noticks labcolor(gs5)) ///
	yscale(noline) ///
	xscale(lcolor(gs5) lw(medthick)) ///
	fcolor(gs8) lcolor(gs5) ) ///
	(scatteri 0 `sMedian', msymbol(Dh) mcolor(black) ) ///
	(scatteri 0 `sp25', msymbol(Th) mcolor(black) ) ///
	(scatteri 0 `sp75', msymbol(Th) mcolor(black) ) ///
	(scatteri 0 `sm', msymbol(Oh) mcolor(black) ), ///
	legend(off) ///
	name(histPowerNonIntOnly, replace)
gr export "histPower_Non-int-only.pdf", replace

	
* combine into one plot
gr combine histESAbsInt histNInt histPowerIntOnly histPowerNonIntOnly, ///
	row(2) cols(2) graphregion(color(white)) 
gr export "combinedInt.pdf", replace

* ----------------------------------------













* ----------------------------------------
* individual-level & macro-level studies
* ----------------------------------------

* clear screen
cls

* individual-level
sum power if macro==0, d
capture drop powerMode
egen powerMode=mode(power) if macro==0, maxmode
tab powerMode if macro==0
centile power if macro==0, centile(2.5 97.5)

* macro-level
sum power if macro==1, d
capture drop powerMode
egen powerMode=mode(power) if macro==1, maxmode
tab powerMode if macro==1
centile power if macro==1, centile(2.5 97.5)


/* combined histogram
twoway ///
	(hist power if macro==0, freq bin(10) ///
	plotregion(color(white)) ///
	graphregion(color(white)) ///
	title("Panel C: Statistical Power", position(11) color(gs5)) ///
	ytitle("Individual-Level (Frequency)", color(gs5) axis(1)) xtitle("") ///
	ylabel(, noticks labcolor(gs5) axis(1)) ///
	yaxis(1) yscale(noline axis(1)) ///
	xlabel(, grid gmax gmin noticks labcolor(gs5)) ///
	yscale(noline) ///
	xscale(lcolor(gs5) lw(medthick)) ///
	fcolor(gs8) lcolor(gs5) ) ///
	///
	(hist power if macro==1, freq bin(10) ///
	yaxis(2) yscale(noline axis(2)) ///
	ytitle("Macro-Level (Frequency)", color(gs5) axis(2)) ///
	ylabel(, noticks labcolor(gs5) axis(2) grid) ///
	fcolor(gs13) lcolor(gs5) ) , ///
	legend(lab(1 "Individual-Level") lab(2 "Macro-Level") pos(12) ring(0) stack region(				lcolor(white))) ///
	name(histPowerIndMacro, replace)
gr export "histPower_indMacro.pdf", replace
*/

* individual-level only
quietly sum power if macro==0, d
local sm=`r(mean)'
local sMedian=`r(p50)'
local sp25=round(`r(p25)',0.001)
local sp75=round(`r(p75)',0.001)
twoway (hist power if macro==0, freq bin(10) ///
	plotregion(color(white)) ///
	graphregion(color(white)) ///
	title("Panel C:" "Statistical Power, Individual-Level", position(11) color(gs5)) ///
	ytitle("Frequency", color(gs5)) xtitle("") ///
	ylabel(, noticks labcolor(gs5)) ///
	xlabel(, grid gmax gmin noticks labcolor(gs5)) ///
	yscale(noline) ///
	xscale(lcolor(gs5) lw(medthick)) ///
	fcolor(gs8) lcolor(gs5) ) ///
	(scatteri 0 `sMedian', msymbol(Dh) mcolor(black) ) ///
	(scatteri 0 `sp25', msymbol(Th) mcolor(black) ) ///
	(scatteri 0 `sp75', msymbol(Th) mcolor(black) ) ///
	(scatteri 0 `sm', msymbol(Oh) mcolor(black) ), ///
	legend(off) ///
	name(histPowerIndOnly, replace)
gr export "histPower_indLevel-only.pdf", replace

	
	
	
* macro-level only
quietly sum power if macro==1, d
local sm=`r(mean)'
local sMedian=`r(p50)'
local sp25=round(`r(p25)',0.001)
local sp75=round(`r(p75)',0.001)
twoway(hist power if macro==1, freq bin(10) ///
	plotregion(color(white)) ///
	graphregion(color(white)) ///
	title("Panel D:" "Statistical Power, Macro-Level", position(11) color(gs5)) ///
	ytitle("Frequency", color(gs5)) xtitle("") ///
	ylabel(, noticks labcolor(gs5)) ///
	xlabel(, grid gmax gmin noticks labcolor(gs5)) ///
	yscale(noline) ///
	xscale(lcolor(gs5) lw(medthick)) ///
	fcolor(gs13) lcolor(gs5) ) ///
	(scatteri 0 `sMedian', msymbol(Dh) mcolor(black) ) ///
	(scatteri 0 `sp25', msymbol(Th) mcolor(black) ) ///
	(scatteri 0 `sp75', msymbol(Th) mcolor(black) ) ///
	(scatteri 0 `sm', msymbol(Oh) mcolor(black) ), ///
	legend(off) ///
	name(histPowerMacroOnly, replace)
gr export "histPower_macro-only.pdf", replace

	
* combine into one plot
gr combine histESAbsMacro histNmacro histPowerIndOnly histPowerMacroOnly, ///
	row(2) cols(2) graphregion(color(white)) 
gr export "combinedIndMacro.pdf", replace

* ----------------------------------------

restore

* ------------------------------------------------------------------------------



















* ------------------------------------------------------------------------------
* supplemental analysis: missing ESs
* ------------------------------------------------------------------------------

* clear screen
cls

* count the total number of ESs
egen totalES = total(esAbs) if esAbs!=.
sum totalES

preserve
drop if esAbs==.
sum esAbs
set obs 502
replace esAbs=.20 if esAbs==.
sum esAbs
restore






* ------------------------------------------------------------------------------
* supplemental analysis: one ES per study
* ------------------------------------------------------------------------------

* clear screen
cls

preserve
set seed 2217
drop if esAbs==.
sample 1 , count by(studyno)
sum esAbs
restore





* ------------------------------------------------------------------------------
* supplemental analysis: do ESs with n!=. differ from ES with n==.
* ------------------------------------------------------------------------------

* clear screen
cls

gen missN = 0 if n!=.
replace missN = 1 if n==.

ttest esAbs, by(missN)













* ------------------------------------------------------------------------------
* supplemental analysis: drop RRs
* ------------------------------------------------------------------------------

* clear screen
cls

gen rrtag = 1 if riskratio!=.
replace rrtag = 0 if riskratio==. & esAbs!=.

ttest esAbs, by(rrtag)

