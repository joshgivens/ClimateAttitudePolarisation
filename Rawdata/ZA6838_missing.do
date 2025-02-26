******************************************************
***Kodierung der Missing Values in das STATA-Format***
******************************************************

/* In allen Komponenten der GLES 2013 sind die Missing Values nach einem einheitlichen Schema codiert, das
   auf den Seiten der GLES (www.gesis.org/gles) heruntergeladen werden kann.
   Bei den Datensätzen im SPSS Format umfassen diese alle Werte von -99 bis -71 . Um auch in den STATA
   Datensätzen Missing Values zu verwenden, ist es nötig diese Missing Values in die STATA eigene Missing
   Value Logik zu übersetzen. Dies wird mit diesem Do-File ermöglicht.
   
   Bei Fragen oder Problemen wenden Sie sich gerne an gles@gesis.org 
   
   GESIS (November 2019) */
 *******************************************************
 
 ********************************
set more off

 
 
*** Verzeichnis festlegen
*cd "xxx"

*** Datensatz laden
*use "ZAXXX_vX-X-X.dta"

********************************


****SPSS Missings in STATA Format umbennenen (Numerisch)****

foreach var of varlist _all {
	capture confirm numeric var `var'
	if !_rc {
		mvdecode `var', mv(-71=.q \-72=.p \-81=.o \-82=.n \-83=.m ///
				 \-84=.l \-85=.k \-86=.j \-91=.i \-92=.h \-93=.g ///
				 \-94=.f \-95=.e \-96=.d \-97=.c \-98=.b ///
				 \-99=.a)
		local lab: value label `var'
		if "`lab'"!="" {
			label define `lab' ///
			.a"keine Angabe" ///
			.b"weiss nicht" ///
			.c"trifft nicht zu" ///
			.d"Split" ///
			.e"nicht teilgenommen" ///
			.f"nicht in Auswahlgesamtheit" ///
			.g"Interview abgebrochen" ///
			.h"Fehler in Daten" ///
			.i"Modus" ///
			.j"nicht wahlberechtigt" ///
			.k"nicht waehlen" ///
			.l"keine Erst-/Zweitstimme abgegeben" ///
			.m"ungueltig waehlen" ///
			.n"keine andere Partei waehlen" ///
			.o"noch nicht entschieden" ///
			.p"nicht einzuschaetzen" ///
			.q"nicht bekannt", modify
			}
		}
}



