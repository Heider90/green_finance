#Goal if this script is to analyze the ONEB baromater 2022 question about green finance
#The individual characteristic data such as education level and income was already created in the script
#01_DataWrangling 


options(scipen = 999)
rm(list = ls())
Sys.setlocale("LC_ALL","English")  
library(tidyverse)
library(readxl)
library(data.table)
library(countrycode)
library(tidylog)
library(haven)
library(foreign)

#Load Survey Data
survey_test <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_KARIEM.rds")

#The following Questions are relevant for our analysis: 


#F71. - F7.4
#Anteil der ö. Bevölkerung die in 2/5/10/15 Jahren von Umweltkatastrophen betroffen ist?
#Answers: Values or -99 = keine Angabe, -97 don't know 


#F8.1-F8.4
#Der Klimawandel wird meine finanzielle Situation in den nächsten 2/5/10/15 Jahren verschlechtern/
#eher verschlechtern/weder noch/eher verbessern/weiß nicht/KA


#F21a_1:Geldgeber sollen in Unternehmen investieren, die Gewinne machen, 
#auch wenn sie damit die Umwelt belasten.
#F21a_2: Ökologisch nachhaltige Unternehmen sind langfristig ertragreicher.
#F21a_3: Die Einhaltung von Umweltstandards sind für Unternehmen vor allem Kostenfaktoren,
#die den Ertrag mindern.
#F21a_4: Der Finanzsektor hat eine besondere Verantwortung für eine klimaneutrale Wirtschaft. 
#F21a_5: Der Klimawandel ist ein finanzielles Risiko für den Finanzsektor.
#F21a_6: Banken und andere Finanzdienstleister versuchen durch umweltfreundliches
#oder soziales Image nur mehr Gewinn zu machen („Greenwashing“).



#F21b_1: Ich bevorzuge Finanzunternehmen die einen klaren ethischen umweltfreundlichen Standpunkt vertreten.

#F21b_2:Bevor ich mich für ein Finanzprodukt entscheide, würde ich mehrere Alternativen in Betracht ziehen, 
#um sicherzustellen, dass ethische, soziale und Umwelt-Kriterien erfüllt werden. 

#F21b_3:Ich habe mich bereits für ein oder mehrere Finanzprodukte entschieden, 
#die aktiv zum Klima- und Umweltschutz beitragen.

#F21b_4:Ich möchte wissen, ob mein veranlagtes Geld zum Klima- und Umweltschutz beiträgt.

#F21b_5:Mir ist es wichtig, dass mein veranlagtes Geld nicht in fossile Energieträger,
#wie Kohle, Öl und Gas, investiert wird.

#F21b_6:Mir ist es wichtig, dass meine Bank bis Mitte des Jahrhunderts klimaneutral wirtschaftet.

#F21b_7:Mir ist wichtig, dass meine Versicherung schrittweise aus Investments
#in Kohleförderung und Kohlekraft aussteigt.

#F21b_8: ch bin bereit, auf Teile meines Zinsertrags zu verzichten, 
#wenn mein Geld dafür in nachhaltige, umweltfreundliche und humanitäre Projekte investiert wird.
