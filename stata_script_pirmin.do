***************************************************************************
*********Auswwertungen
***************************************************************************
clear all
set mem 5000000
set maxvar 10000
set more off


**************************************************************************************
***Define Data folder
**************************************************************************************
global data "Z:\Projekte\HFCN\Fragebogen\Crypto\data\"
cd $data

global output "Z:\Projekte\HFCN\Fragebogen\Crypto\output\breitenfellner"

use "60011055b_externv_3.dta", clear

****lower case vars
rename *, lower

***set graphics scheme
set scheme white_tableau

***create socio-economic characteristics
****alter, stadt-land, bildung, einkommen, geschlecht, beruf, Auto oder nicht

***age
gen age=alterxp1
label variable age "Age"
gen sqage=age*age
label variable sqage "Squarred Age"

***agecat
recode age (1/29=1) (30/44=2) (45/59=3) (60/79=4) (80/150=5), gen(agecat)
label define l_agecat 1 "15-29 years"  2 "30-44 years" 3 "45-59 years" 4 "60-79 years" 5 "80+ years" 
label val agecat l_agecat


***female
gen female01=(geschlxp1==2)
label define l_female01 1 "female"  0 "male" 
label val female01 l_female01

***education
recode educxp1 (1/2=1) (3=2) (4/9=3) (10/12=4) (99=.), gen(edu4)
label define l_edu4 1 "Primary education"  2 "Low sec edu" 3 "High sec edu" 4 "Tertiary education" 
label val edu4 l_edu4
tabulate edu4, generate(edu4_d)

***municipality size
recode ogr (1/2=1) (3=2) (4/7=3) (8=4), gen(msize)
label define l_msize 1 "0-3000" 2 "3000-5000" 3 "5000-1Mio" 4 "1 Mio+" 
label val msize l_msize
tabulate msize, generate(msize_d)

***job status
recode taetxp1 (13=1) (14/15=2) (5=3) (6=4) (7/12=5) (3/4=5) (7/11=5) (16=5)  (99=.), gen(jobstatus)
label define l_jobstatus 1 "Fulltime"  2 "Parttime" 3 "Unemployed" 4 "Retired" 5 "Other" 
label val jobstatus l_jobstatus
tabulate jobstatus, generate(jobstatus_d)


**sum pincome
recode zpnett (25=1) (1/4=2) (5/7=3) (8/9=4) (10/11=5) (12/16=6) (17/24=7)  (26=.), gen(pinc)
label define l_pinc 1 "no income"  2 "0-900" 3 "900-1350" 4 "1350-1650" 5 "1650-1950" 6 "1950-3000" 7 "3000+" 
label val pinc l_pinc


global catsoc agecat female01 edu4 msize jobstatus pinc



*****************generate other vars

***bf1 =auto
gen cartowork01=(bf1==1)
replace cartowork01=. if bf1<0 | bf1==.

****Anreiz für Umstieg
recode bf5 (1=1) (2=2) (3=3) (4=4) (5=5) (.=.) (-97=.)  (-99=.), gen(umstieg)
label define l_umstieg 1 "staatl. Einmalzahlung"  2 "Steuervorteil Oeffis" 3 "Anbindung Oeffis" 4 "flex Arbeitszeit" 5 "Sonstiges" 
label val umstieg l_umstieg


*****Analyse für Breitenfellner


***Auto in die Arbeit

foreach var in $catsoc {
	
	graph bar cartowork01, over(`var') ///
	ytitle("Share using car to go to work")
	graph export "$output/CarToWork_`var'.png", replace width(1000)

}

***Anreiz für Umstieg

foreach var in $catsoc {
	
	catplot umstieg `var', fraction
	graph export "$output/Umstieg_`var'.png", replace width(1000)
}

	
***Anreiz für Umstieg

foreach var in $catsoc {
	
	catplot bf8_1 `var' if bf1>0, fraction ///
	title("pers. fin Situation Klimawandel 2 Jahre ")
	graph export "$output/FinKlima2_`var'.png", replace width(1000)
}

foreach var in $catsoc {
	
	catplot bf8_2 `var' if bf1>0, fraction ///
	title("pers. fin Situation Klimawandel 2 Jahre ")
	graph export "$output/FinKlima5_`var'.png", replace width(1000)
}


foreach var in $catsoc {
	
	catplot bf8_3 `var' if bf1>0, fraction ///
	title("pers. fin Situation Klimawandel 2 Jahre ")
	graph export "$output/FinKlima10_`var'.png", replace width(1000)
}

foreach var in $catsoc {
	
	catplot bf8_4 `var' if bf1>0, fraction ///
	title("pers. fin Situation Klimawandel 2 Jahre ")
	graph export "$output/FinKlima15_`var'.png", replace width(1000)
}



forvalues k=1/4 {

gen bf8_`k'_d=(bf8_`k'<3) if bf8_`k'>0	
	
	
} 


forvalues k=1/4 {

	binscatter bf8_`k'_d age, ///
	ytitle(Probability that answer is worse financial situation)	
	graph export "$output/FinKlima_age_`k'.png", replace width(1000)	
	
}



***bf19 und bf19a sparen
***cansave
gen cansave01=(bf19==1) if bf19>0

***valsave
gen valsave=bf19a if bf19a>0


twoway lpoly cansave01 age  if age<86 , lpattern(solid) bw(10) ///
title("Share who can save") ///
ytitle("Share") ///
xtitle("Age")
graph export "$output/Save_age.png", replace width(1000)	


twoway lpoly valsave age  if age<86 , lpattern(solid) bw(20)  ///
title("Monthly savings of those who can save") ///
ytitle("Euro") ///
xtitle("Age")
graph export "$output/Saveval_age.png", replace width(1000)	

binscatter valsave age  if age<86 
graph export "$output/Saveval_age2.png", replace width(1000)	



***sparverhalten und umwelt Green Finance


foreach var in $catsoc {
	
	catplot bf21a_1 `var' if bf21a_1>0, fraction ///
	title("Geldgeber sollen in Unternehmen investieren, die Gewinne machen, auch wenn sie damit die Umwelt belasten")
	graph export "$output/21Geldgeber_`var'.png", replace width(1000)
}

foreach var in $catsoc {
	
	catplot bf21a_2 `var' if bf21a_2>0, fraction ///
	title("Ökologisch nachhaltige Unternehmen sind langfristig ertragreicher")
	graph export "$output/21nachhaltig_`var'.png", replace width(1000)
}



foreach var in $catsoc {
	
	catplot bf21a_3 `var' if bf21a_3>0, fraction ///
	title("Die Einhaltung von Umweltstandards sind für Unternehmen vor allem Kostenfaktoren, die den Ertrag mindern")
	graph export "$output/21Standards_`var'.png", replace width(1000)
}


foreach var in $catsoc {
	
	catplot bf21a_4 `var' if bf21a_4>0, fraction ///
	title("Der Finanzsektor hat eine besondere Verantwortung für eine klimaneutrale Wirtschaft")
	graph export "$output/21Finanzsektor_`var'.png", replace width(1000)
}


foreach var in $catsoc {
	
	catplot bf21a_5 `var' if bf21a_5>0, fraction ///
	title("Der Klimawandel ist ein finanzielles Risiko für den Finanzsektor")
	graph export "$output/21Klimawandel_`var'.png", replace width(1000)
}


foreach var in $catsoc {
	
	catplot bf21a_6 `var' if bf21a_6>0, fraction ///
	title("Banken und andere Finanzdienstleister versuchen durch umweltfreundliches oder soziales Image nur mehr Gewinn zu machen (Greenwashing)")
	graph export "$output/21Greenwashing_`var'.png", replace width(1000)
}












