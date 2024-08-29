cd C:\Users\PATH\TO\DATA
use LRTi, clear

log using LRTi.log

/* Descriptive */
gen lrti_binary = datelrti
recode lrti_binary 51134 = 0 0/51133 = 1
tab lrti_binary
tab gender
tab vaccine
tab partner
tab died
tab gender vaccine, row chi
tab gender lrti_binary, row chi
tab gender died, row chi

/* Defining Follow-up using yearly time scale */
stset dateofexit, fail(died) origin(dateofbirth) enter(dateofentry) id(patid) scale(365.25)

/* 
Lexis expansion to define exposure variables:
TIME since LRTi diagnosis as no infection == 0, <=3 months == 1, > 3 months == 2
Current AGE in three bins, 75-79, 80-84, 85+
*/

/* As the time scale is in years, 0.25 represents 3 months, and 10 represents the maximum follow up of 10 years */
stsplit LRTi, after(datelrti) at(0, 0.25, 10) 

recode LRTi -1 = 0 0 = 1 .25 = 2

/* Creating the label for the PRIMARY exposure */
label define LRTi_lab 0 "LRTi(-)" 1 "<= 3 months" 2 "> 3 months" 

label values LRTi LRTi_lab

/* Creating current age bins */
stsplit currage, at(75, 80, 85, 110) 

/* Age bin label creation */
label define age 75 "75-79" 80 "80-84" 85 "85+" 

label values currage age


foreach var in LRTi currage gender partner vaccine{
	recast int `var'
	display "`var'" _col(20) "`: type `var''"
}


/* 
Testing that the split was done properly with no corruption
_d = outcome indicator for each record
_t0 = Entry Time
_t = Exit Time
*/
list patid _d _t0 _t LRTi currage if patid == 49

tab _d

/* 
Data is correct, however "missing" values were filled in for the expanded dataset for "died" variable.
This may interfere with future analysis (?), and so they will be recoded to "0" aka not dead
 */

recode died .=0

/* More descriptive analysis with the new exposure variables */
foreach var in gender vaccine currage partner{
	tab LRTi `var', col chi
}
 
/* Crude estimate Rates for the exposures */
foreach var in gender vaccine LRTi currage partner{
	strate `var', per(1000)
}

/* Crude estimate Rate Ratios for the exposures */
stmh LRTi, c(1, 0)
stmh LRTi, c(2, 0)
stmh currage, c(80, 75)
stmh currage, c(85, 75)
/* setting married as baseline */
stmh partner, c(1, 0) 
stmh partner, c(2, 0)
/* Male as baseline */
stmh gender, c(0, 1)
/* Refused as baseline */
stmh vaccine, c(0, 1)

/* Variable association to primary exposure */

foreach var in currage gender partner vaccine {
 	tab `var' LRTi , col chi
}

/* 
Individually adjusted Rate Ratios by confounder per stratum of LRTi 
Also assesses effect modification 
*/
foreach var in currage gender partner vaccine{
	stmh LRTi, by(`var') c(1, 0)
	stmh LRTi, by(`var') c(2, 0)
}


/* Poisson Model Building */
streg i.LRTi, d(exp) nolog

foreach var in currage gender partner vaccine{
	streg i.LRTi i.`var', d(exp) nolog
}

/* LRTs */
/* Individually assessing model fit of each variable */
foreach var in currage gender partner vaccine{
	quietly streg i.LRTi i.`var', d(exp) nolog
	est store a
	quietly streg i.LRTi, d(exp) nolog
	est store b
	lrtest a b
}

/* Assessing effect modification for each variable */
foreach var in currage gender partner vaccine{
	quietly streg i.LRTi##i.`var', d(exp) nolog
	est store a
	quietly streg i.LRTi i.`var', d(exp) nolog
	est store b
	lrtest a b
}

/* Assessing the significance of the primary exposure against each other variable */
foreach var in currage gender partner vaccine{
	quietly streg i.`var' i.LRTi, d(exp) nolog
	est store a
	quietly streg i.`var', d(exp) nolog
	est store b
	lrtest a b
}

/* Effect Modification Significance */
streg i.LRTi##i.vaccine, d(exp) nolog
est store a
quietly streg i.LRTi, d(exp) nolog
est store b
lrtest a b

streg i.LRTi##i.gender, d(exp) nolog
est store a
quietly streg i.LRTi, d(exp) nolog
est store b
lrtest a b

/* Effect Modification Significance WITH confounding adjustments */
streg i.LRTi##i.vaccine i.currage, d(exp) nolog
est store a
quietly streg i.LRTi i.currage, d(exp) nolog
est store b
lrtest a b

streg i.LRTi##i.gender i.currage, d(exp) nolog
est store a
quietly streg i.LRTi i.currage, d(exp) nolog
est store b
lrtest a b

/* Testing for departure of linear trend after adjusting for condounding (Age). LRTi first, then age */
quietly streg i.LRTi i.currage, d(exp) nolog
est store a
quietly streg LRTi i.currage, d(exp) nolog
est store b
lrtest a b

quietly streg i.LRTi i.currage, d(exp) nolog
est store a
quietly streg i.LRTi currage, d(exp) nolog
est store b
lrtest a b

/* Effect Modification stratification */
streg LRTi##i.vaccine i.currage, d(exp) nolog
lincom 1.LRTi + 1.LRTi#1.vaccine, hr
lincom 2.LRTi + 2.LRTi#1.vaccine, hr
lincom 1.vaccine + 1.LRTi#1.vaccine, hr
lincom 1.vaccine + 2.LRTi#1.vaccine, hr

streg LRTi##i.gender i.currage, d(exp) nolog
lincom 1.LRTi + 1.LRTi#1.gender, hr
lincom 2.LRTi + 2.LRTi#1.gender, hr
lincom 1.gender + 1.LRTi#1.gender, hr
lincom 1.gender + 2.LRTi#1.gender, hr

/* Comparing Male and Female Vaccine Acceptance Rates */
tab gender vaccine, row chi
