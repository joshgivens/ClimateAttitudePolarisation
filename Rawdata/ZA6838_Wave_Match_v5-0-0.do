*****************************************************************************
*																			*
* Matching of GLES Panel Waves and Samples  								*
*																			*
* GESIS - Leibniz Institute for the Social Sciences							*		
* July 2021																*
*																			*
* Author: A. Stroppe / N. Jungmann								  						*
*****************************************************************************

/* 
   This Do-File merges and appends the corresponding data sets of the GLES Panel 
   and prepares the resulting data set for analysis.
   
   This Do-file is designed to create individualized data sets. In order to do 
   so, users need to specify which waves and samples of the GLES 
   Panel they would like to match. All data sets which you wish to match must be
   located in the same directory. Please define this path where noted.

   For detailed information on the waves and samples, please refer to the study 
   description, the wave reports, and the questionnaires.
   
   If you have any questions about this Do-File and its execution, or questions
   concerning the data collection and preperation of the GLES Panel,  
   please contact us via gles@gesis.org.
*/


***************************************************************************
*  Stata Settings  
***************************************************************************
version 13.0
clear
clear mata
clear matrix
set more off

***************************************************************************		
*  User Macros: PLEASE ADD YOUR OWN SPECIFICATIONS					
***************************************************************************

/* 
	Please specify all necessary information here. 
	Note that you must specify the working directory where all data sets you 
	want to match should be located and where the complete dataset will be saved 
	after the execution of this Do-file.
	
	You also need to specify the samples and waves you want to match. Since not 
	all waves are available for all samples (e.g., wave 1 to 9 are only available 
	for sample A1, A2 and A3), users are advised to consult the study description when choosing
	the option "all" in combination with different samples.
	
	Please note that Sample B datasets are only available for off-site access
	after signing a Data Use Agreement with GESIS. Please contact us via gles@gesis.org.
	
	Following combinations  will lead to the following results:
	
					
							
	Sample A + All Waves: 	Waves 1-15, Sample A1, A2 & A3 + 
							Profile Wave 2020, Sample A4
							
	Sample A4 + All Waves 	2021 Election Campaign Panel 
							(Profile Wave 2020 (a2) and from wave 15 onwards)					
							
	Sample B + All Waves: 	Recruitment Wave, Sample B + Wave 10-14, Sample B
	
	Sample AB + All Waves: 	Waves 1-15, Sample A1, A2 & A3 + 
							Profile Wave 2020, Sample A4 +
							Recruitment Wave, Sample B +
							Wave 10-14, Sample B
							
	
	Sample AB + Wave 10: 	Wave 10 of Sample A + Wave 10 of Sample B
	Sample AB + Wave 11: 	Wave 11 of Sample A + Wave 11 of Sample B
	Sample AB + Wave 12: 	Wave 12 of Sample A + Wave 12 of Sample B
	Sample AB + Wave 13: 	Wave 13 of Sample A + Wave 13 of Sample B
	Sample AB + Wave 14: 	Wave 14 of Sample A + Wave 14 of Sample B	
	(...)

	Please note	that you need to run the complete Do-file, including the 
	specified locals, to receive the data set. Hence, you will not be able to 
	run one part of the Do-file after the other. Be aware that this might take
	a while!
*/


/* Specify path where all data sets, which you wish to match, are saved. 
The merged data sets will also be saved in this directory.  */
local user_path `"J:\work\GLES\19_Mitarbeitende\Stroppe\testwiese"'  				

/* Specify sample(s) you want to include. */
local sample_select `"AB"' /* Select from: "A" , "A4", "B",  or "AB" */

/* Specify waves you want to include */
local wave_select `"all"' /* Select from: "all", "10", "11", ...  */

***************************************************************************
*  THE FOLLOWING SYNTAX DOES NOT NEED ANY ADJUSTMENT TO RUN SMOOTHLY *
***************************************************************************


***************************************************************************
*  Predefined Macros 
***************************************************************************
/* Defines macros to identify the correct data sets  */
/* Define ZA number */
local zanummer `"6838"'

/* Define version of data set */
local release `"5-0-0"'

/* Define field time:
Field times differ between the samples and added waves  */
local field_start_sA `"2016-10-06"'
local field_start_sA4 `"2020-09-21"'
local field_start_sB `"2017-07-31"'
																						 
/* Define newest wave */
local wave `"15"'

***************************************************************************
*  Check User Macros
***************************************************************************
/* Checks if the locals were defined correctly by user. If not, the process is
aborted and an error message displayed. */

/* Check sample */
	if `"`sample_select'"' == `"A"' {
	}
	else if `"`sample_select'"' == `"B"' {
	}
	else if `"`sample_select'"' == `"AB"' {
	}
	else if `"`sample_select'"' == `"A4"' {
	}
	else {
		dis as error `"The selection of sample(s) is not valid. Please specify the local sample_select correctly!"'
		exit 198
	}


/* Check wave */
	if `"`wave_select'"' == `"all"' {
	}
	else if inlist(`"`wave_select'"', `"10"' , `"11"', `"12"', `"13"', `"14"', `"15"') {
	}	
	else {
		dis as error `"The selection of wave(s) is not valid. Please specify the local wave_select correctly!"'
		exit 198
	}


/* Check sample and wave combination*/
	if `"`sample_select'"' == `"A"' & `"`wave_select'"' == `"all"' {
		dis as error`"Please note: For Sample A the selection `"all"' waves includes waves 1 to `wave' and Profile Wave 2020 (a2)."'
	}
	
	else if `"`sample_select'"' == `"B"' & `"`wave_select'"' == `"all"' {
		if `wave' == 15 {
			dis as error `"Please note: For Sample B the selection `"all"' waves includes the recruitment wave and waves 10 to 14."'
		}
		else if `wave' < 15 {
			dis as error `"Please note: For Sample B the selection `"all"' waves includes the recruitment wave and waves 10 to `wave'."'
		}
		else if `wave' == 16 {
			dis as error `"Please note: For Sample B the selection `"all"' waves includes the recruitment wave and waves 10 to 14 and `wave'."'
		}
		else if `wave' > 16 {
			dis as error `"Please note: For Sample B the selection `"all"' waves includes the recruitment wave and waves 10 to 14 and 16 to `wave'."'
		}
	}
	
	else if `"`sample_select'"' == `"AB"' & `"`wave_select'"' == `"all"' {
		if `wave' == 15 {
			dis as error `"Please note: For Sample AB the selection `"all"' waves includes waves 1 to `wave' + Profile Wave 2020 of Sample A and the recruitment wave and wave 10 to 14 of Sample B."'
		}
		else if `wave' < 15 {
			dis as error `"Please note: For Sample AB the selection `"all"' waves includes waves 1 to `wave' + Profile Wave 2020 of Sample A and the recruitment wave and wave 10 to `wave' of Sample B."'
		}
		else if `wave' == 16 {
			dis as error `"Please note: For Sample AB the selection `"all"' waves includes waves 1 to `wave' + Profile Wave 2020 of Sample A and the recruitment wave and wave 10 to 14 and `wave' of Sample B."'
		}
		else if `wave' > 16 {
			dis as error `"Please note: For Sample AB the selection `"all"' waves includes waves 1 to `wave' + Profile Wave 2020 of Sample A and the recruitment wave and wave 10 to 14 and 16 to `wave' of Sample B."'
		}
	}
	
	else if `"`sample_select'"' == `"A4"' & `"`wave_select'"' == `"all"' {
		if `wave' > 15 {
			dis as error `"Please note: For Sample A4 the selection `"all"' waves includes the Profile Wave 2020 (a2) and wave 15-`wave'."'
		}
		else if `wave' == 15 {
			dis as error `"Please note: For Sample A4 the selection `"all"' waves includes the Profile Wave 2020 (a2) and wave `wave'."'
		}
		else if `wave' < 15 {
			dis as error `"The file `"ZA`zanummer'_wa2_sA_v`release'.dta"' already contains the prepared dataset."'
			exit 198	
		}
	}

	foreach num of numlist 10/`wave' {
		if `"`wave_select'"' == `"15"' & `"`sample_select'"' == `"A"' {
			dis as error `"The file `"ZA`zanummer'_w`wave_select'_s`sample_select'_v`release'.dta"' already contains the prepared dataset of wave `wave_select' and sample `sample_select'."'
			exit 198
		}
		else if `"`wave_select'"' == `"15"' & `"`sample_select'"' == `"B"' {
			dis as error `"Since Sample B was not interviewed in Wave 15, the file `"ZA`zanummer'_w`wave_select'_sB_v`release'.dta"' does not exist."'
			exit 198
		}
		else if `"`wave_select'"' == `"15"' & `"`sample_select'"' == `"AB"' {
			dis as error `"Since Sample B was not interviewed in Wave 15, the file `"ZA`zanummer'_w`wave_select'_sB_v`release'.dta"' does not exist. The file `"ZA`zanummer'_w`wave_select'_sA_v`release'.dta"' already contains the prepared dataset of wave `wave_select', Sample A."'
			exit 198
		}
		else if `"`wave_select'"' == `"`num'"' & (`"`sample_select'"' == `"A"' | `"`sample_select'"' == `"B"') {
			dis as error `"The file `"ZA`zanummer'_w`wave_select'_s`sample_select'_v`release'.dta"' already contains the prepared dataset of Wave `wave_select', Sample `sample_select'."'
			exit 198
		}
		else {
		}
	}


***************************************************************************
*  Match Waves and Samples
***************************************************************************

/* Set path to specified user path */
cd `user_path'

/* Matching Sample A */

if  `"`wave_select'"' == `"all"' & (`"`sample_select'"' == `"A"' | `"`sample_select'"' == `"AB"' ) {

	/* Check if sample A data sets are in working directory */
	capture confirm file ZA`zanummer'_w1to9_sA_v`release'.dta
	if _rc == 601 {
		dis as error `"ZA`zanummer'_w1to9_sA_v`release'.dta is not in the working directory! Please change the directory."'
		exit 198
	}
	
	/* Load dataset wave 1 to 9 */
	use `"ZA`zanummer'_w1to9_sA_v`release'.dta"', clear	
	
	
	foreach num of numlist 10/14 {
			capture confirm file ZA`zanummer'_w`num'_sA_v`release'.dta
			if _rc == 0 {
			}
			else {
			dis as error `"ZA`zanummer'_w`num'_sA_v`release'.dta is not in the working directory! Please change the directory."'
				exit 198
			}

		/* merge wave 10 to current wave 
		The "update replace" option is used to update the participation and weighting variables.  */
		merge 1:1 lfdn using `"ZA`zanummer'_w`num'_sA_v`release'.dta"', update replace generate(_mergeW`num'sA)
		
		/* Recode and label participation variables:
		Variables on participation behavior in all GLES Panel waves do not follow the usual missing scheme
		but are updated in each wave corresponding to the participation behavior of respondents.
		For example, the numeric code corresponding to the participation behavior (p_participation) needs to be updated
		for those respondents who did not participate in the added wave. Additionally, the disposition code is change to "12 Invited"
		and the variable participation in this wave is recoded to "0" for this group. Lastly, the variable label of
		the number of complete interviews is updated (the information is already updated via the merge).*/
		
		/* Participation behavior concerning all waves */
		replace p_participation  = p_participation  + `"-0"' if kp`num'_participation == .

		/* Disposition code */
		replace kp`num'_dispcode = 12 if kp`num'_participation == .
		foreach var of varlist kp*_dispcode {
			replace `var' = 12 if `var' == .
			lab define `var' 12 "Eingeladen (12)", modify
			}

		/* Participation in added wave */
		replace kp`num'_participation = 0 if kp`num'_participation == .
		foreach var of varlist kp*_participation {
			replace `var' = 0 if `var' == .
			}

		/* Label variable "Number of Complete Participations" */
		label variable n_participation `"Anzahl vollstaendige Teilnahmen Welle 1 bis `num'"'
		
		foreach x of numlist 2/`num' { 
		local lab_teilnahme_`x' `"`x' "`x' Teilnahmen" "' 
		local lab_teilnahme  `lab_teilnahme' `lab_teilnahme_`x''
		}

		lab def N_TEILNAHMEN 0 "keine vollstaendige Teilnahme" 1 "1 Teilnahme"  `lab_teilnahme' , modify
		lab val n_participation N_TEILNAHMEN

		/* Recode metadata:
		The field time variables are adjusted and the variables replaced with the values of the 
		complete field time of the GLES Panel (including all waves and all sample). More detailed information 
		on the field time of each wave and sample are included in the individual datasets 
		and in the supplementary study description. */
		replace field_start = `"`field_start_sA'"'
		replace field_end = `"2018-11-21"' if `num' == 10 & kp`num'_participation != 0
		replace field_end = `"2019-06-12"' if `num' == 11 & kp`num'_participation != 0
		replace field_end = `"2019-11-20"' if `num' == 12 & kp`num'_participation != 0
		replace field_end = `"2020-05-05"' if `num' == 13 & kp`num'_participation != 0
		replace field_end = `"2020-11-17"' if `num' == 14 & kp`num'_participation != 0
		
		
		/* Label Weighting Variables:
		Variables have been updated with the values of the newest wave but keep the previous
		variable labels, which need to be adjusted.	*/
		label variable wei5_mz `"Sozial-/regionalstrukt. Gewicht(MZ2016) Teilnahme an Welle 5 bis `num'"'
		label variable wei5_on `"Sozial-/regional.Gewicht(Onliner)Teilnahme an Welle 5 bis `num'"'
		
		/* Missing coding:
		Since not all respondents partcipated in all waves and samples, missings are recoded accordingly based
		on the GLES missing scheme. */

		/* Save all variable names, except for the weighting variables, in a local for recoding.  */
		unab all_vars : _all
		unab no_recode : wei* 
		local recode_vars : list all_vars-no_recode

		/* Recode all variables to -95 "nicht teilgenommen" ("not participated") for respondents, who
		a) did not participate in wave 10
		b) did participate in the GLES Panel Sample A for the first time in wave 10 (f.e., panelists who
		participated in the Short-term Campaign Panel 2013 have been invited to each Panel wave since) */

		foreach var of varlist `recode_vars' {
			capture confirm numeric variable `var'
			if !_rc {
				qui recode `var' (. = -95)
				qui capture lab def `:value label `var'' -95 `"nicht teilgenommen"', modify
				if _rc {
					qui lab def `var' -95 `"nicht teilgenommen"', modify
					qui lab val `var' `var'
				}
			}
			else {
				qui replace `var' = `"-95 nicht teilgenommen"' if `var'== `""'
			}
		}

		/* Reorder:
		The paradata provided by Respondi were updated for the most recent wave and
		are ordered at the end of the dataset.
		*/
		order  p_enter_date - p_numstr3_2, before(_mergeW`num'*)
		}

	
* Appending Profile Wave 2020 (a2)

capture confirm file ZA`zanummer'_wa2_sA_v`release'.dta
	if _rc == 0 {
		}
	else {
		dis as error `"ZA`zanummer'_wa2_sA_v`release'.dta is not in the working directory! Please change the directory."'
		exit 198
	}	

/* Append data set */

append using `"ZA`zanummer'_wa2_sA_v`release'.dta"', gen(_appendWa2)

/* Recode sociodemographics:
Sociodemographics are not included in all waves, but if questions are included, it might be necessary
to update these variables ("kpX_") if the information was missing before. */

local nums	2280 2290 2291 2350 2390 3710 3720 3730 3740 3750 3760 3920 3930 2551 3940 3950 3960
	foreach num of local nums	{
	/* [PLEASE DON'T USE THIS PART OF THE LOOP, AS IT HAS NOT BEEN TESTED YET.]
		capture confirm variable kpa2_`num'
		if !_rc	{
				replace kpx_`num' = kpa2_`num' if sample == 6
		}
		else	{
			
		}
	*/
		capture confirm variable kpx_`num'flag
		if !_rc {
	// 		replace kpx_`num'flag == 5 if sample == 6 [PLEASE DON'T USE THIS PART OF THE LOOP, AS IT HAS NOT BEEN TESTED YET.]
			lab define `:value label kpx_`num'flag' 5 "Auskunft in Profilwelle 2020 gemacht", modify
		}
		else {
			
		}
	}

		order wei12_mz wei12_on, after(wei5_on)
		
		* from wave 16 onwards: foreach num of numlist 15/`wave' {
		foreach num of numlist 15 {
			capture confirm file ZA`zanummer'_w`num'_sA_v`release'.dta
			if _rc == 0 {
			}
			else {
			dis as error `"ZA`zanummer'_w`num'_sA_v`release'.dta is not in the working directory! Please change the directory."'
				exit 198
			}

		/* merge wave 15 to current wave 
		The "update replace" option is used to update the participation and weighting variables.  */
		merge 1:1 lfdn using `"ZA`zanummer'_w`num'_sA_v`release'.dta"', update replace generate(_mergeW`num'sA)
		
		/* Recode and label participation variables:
		Variables on participation behavior in all GLES Panel waves do not follow the usual missing scheme
		but are updated in each wave corresponding to the participation behavior of respondents.
		For example, the numeric code corresponding to the participation behavior (p_participation) needs to be updated
		for those respondents who did not participate in the added wave. Additionally, the disposition code is change to "12 Invited"
		and the variable participation in this wave is recoded to "0" for this group. Lastly, the variable label of
		the number of complete interviews is updated (the information is already updated via the merge).*/
		
		/* Participation behavior concerning all waves */
		replace p_participation  = p_participation  + `"-0"' if kp`num'_participation == .

		/* Disposition code */
		replace kp`num'_dispcode = 12 if kp`num'_participation == .
		foreach var of varlist kp*_dispcode {
			replace `var' = 12 if `var' == .
			lab define `:value label `var'' 12 "Eingeladen (12)", modify
			}

		/* Participation in added wave */
		replace kp`num'_participation = 0 if kp`num'_participation == .
		foreach var of varlist kp*_participation {
			replace `var' = 0 if `var' == .
			}

		/* Label variable "Number of Complete Participations" */
		label variable n_participation `"Anzahl vollstaendige Teilnahmen Welle 1 bis `num'"'
	
		foreach x of numlist `num' { 
		local lab_teilnahme_`x' `"`x' "`x' Teilnahmen" "' 
		local lab_teilnahme  `lab_teilnahme' `lab_teilnahme_`x''
		}
				/* AB WELLE 16 
			if `num' == 15 {
					foreach x of numlist `num' { 
					local lab_teilnahme_`x' `"`x' "`x' Teilnahmen" "' 
					local lab_teilnahme  `lab_teilnahme' `lab_teilnahme_`x''
					}
				}
				else {
					foreach x of numlist 15/`num' { 
					local lab_teilnahme_`x' `"`x' "`x' Teilnahmen" "' 
					local lab_teilnahme  `lab_teilnahme' `lab_teilnahme_`x''
					}					
				}
			*/

		lab def N_TEILNAHMEN 0 "keine vollstaendige Teilnahme" 1 "1 Teilnahme"  `lab_teilnahme' , modify
		lab val n_participation N_TEILNAHMEN


		/* Recode metadata:
		The field time variables are adjusted and the variables replaced with the values of the 
		complete field time of the GLES Panel (including all waves and all sample). More detailed information 
		on the field time of each wave and sample are included in the individual datasets 
		and in the supplementary study description. */
		replace field_start = `"`field_start_sA'"' if inrange(sample,2,4)
		replace field_start = `"`field_start_sA4'"' if sample == 6
		replace field_end = `"2021-03-12"' if `num' == 15 & kp`num'_participation != 0
		
		
		/* Label Weighting Variables:
		Variables have been updated with the values of the newest wave but keep the previous
		variable labels, which need to be adjusted.	*/
		label variable wei5_mz `"Sozial-/regionalstrukt. Gewicht(MZ2016) Teilnahme an Welle 5 bis `num'"'
		label variable wei5_on `"Sozial-/regional.Gewicht(Onliner)Teilnahme an Welle 5 bis `num'"'
		
		/* Missing coding:
		Since not all respondents partcipated in all waves and samples, missings are recoded accordingly based
		on the GLES missing scheme. */

		/* Save all variable names, except for the weighting variables, in a local for recoding.  */
		unab all_vars : _all
		unab no_recode : wei* 
		local recode_vars : list all_vars-no_recode

		/* Recode all variables to -95 "nicht teilgenommen" ("not participated") for respondents, who
		a) did not participate in wave 10
		b) did participate in the GLES Panel Sample A for the first time in wave 10 (f.e., panelists who
		participated in the Short-term Campaign Panel 2013 have been invited to each Panel wave since) */

		foreach var of varlist `recode_vars' {
			capture confirm numeric variable `var'
			if !_rc {
				qui recode `var' (. = -95)
				qui capture lab def `:value label `var'' -95 `"nicht teilgenommen"', modify
				if _rc {
					qui lab def `var' -95 `"nicht teilgenommen"', modify
					qui lab val `var' `var'
				}
			}
			else {
				qui replace `var' = `"-95 nicht teilgenommen"' if `var'== `""'
			}
		}

		/* Reorder:
		The paradata provided by Respondi were updated for the most recent wave and
		are ordered at the end of the dataset.
		*/
		order  p_enter_date - p_numstr3_2, before(_mergeW`num'*)
		}
		
}


	/* Save dataset if only sample A*/
	if `"`sample_select'"' == `"A"' {
		save `"ZA`zanummer'_`wave_select'waves_s`sample_select'_v`release'.dta"', replace 
	}
	
	
	
	
******************************************************************************************	






/* Append sample A with sample B */
	else if `"`sample_select'"' == `"AB"' {
		/* Check if sample B recruitment wave and wave 10 to current wave is in directory */
		
		foreach num of numlist 10/`wave' {
			if `num' == 15 {
				dis as error `"Please note: Since Sample B was not interviewed in Wave 15, the file `"ZA`zanummer'_w15_sB_v`release'.dta"' does not exist and will not be appended."'
			}
			else if `num' != 15 {
				capture confirm file ZA`zanummer'_w`num'_sB_v`release'.dta
				if _rc == 0 {
				}
				else {
					dis as error `"ZA`zanummer'_w`num'_sB_v`release'.dta is not in the working directory! Please change the directory."'
					exit 198
				}
			}
		}
		
		capture confirm file ZA`zanummer'_wrc_sB_v`release'.dta
		if _rc == 0 {
		}
		else {
			dis as error `"ZA`zanummer'_wrc_sB_v`release'.dta is not in the working directory! Please change the directory."'
			exit 198
		}
	
		/* Append data set with wave 10 sample B */
		append using `"ZA`zanummer'_w10_sB_v`release'.dta"', gen(_appendW10)
		
		/* Recode metadata:
		The field time variables are adjusted and the variables replaced with the values of the 
		complete field time of the GLES Panel (including all waves and all sample). More detailed information 
		for each wave and sample are included in the individual datasets and in the supplementary
		study description. */
		replace field_start = `"`field_start_SB'"' if sample == 5
		replace field_end = `"2019-01-31"' if sample == 5
		replace access_panel = -94 if sample == 5
	
		/* Recode sociodemographics:
		Sociodemographics are not included in all waves, but if questions are included, it might be necessary
		to update these variables ("kpX_") if the information was missing before. */
		replace kpx_2280 = kp10_2280 if sample == 5 & kp10_participation != 0
		replace kpx_2280flag = 4  if sample == 5 & kp10_participation != 0
		qui capture label define `:value label kpx_2280flag' 4 `"Angabe in Welle 10 gemacht"', modify
	
		
		/* Reorder:
		Some variables are only included in sample B and need to be reordered.
		*/
		order  kp10_500 kp10_510 kp10_520  kp10_codingbegin kp10_codingend kp10_inbox kp10_info, after(kp10_modus)
		order wei6_ow wei7_mz wei8_mz, after(wei5_on) 
		order kp10_2280, before(kp10_2601)
		order kp10_lastpage_sB, after(kp10_lastpage)
		
		/* Merge data set with additional wave(s) of sample B */
		foreach num of numlist 11/`wave' {
			if `num' == 15 {
			}
			else if `num' != 15 {
				merge 1:1 lfdn using `"ZA`zanummer'_w`num'_sB_v`release'.dta"', update replace generate(_mergeW`num'sB)
							
				/* Recode sociodemographics:
				Sociodemographics are not included in all waves, but if questions are included, it might be necessary
				to update these variables ("kpX_") if the information was missing before. */
				local flag= `num' - 6
				replace kpx_2280flag = `flag'  if sample == 5 & kp`num'_participation != 0 & kpx_2280flag == .
				qui capture label define `:value label kpx_2280flag' `flag' `"Angabe in Welle `num' gemacht"', modify
				
				/* Recode and label participation variables:
				Variables on participation behavior in all GLES Panel waves do not follow the usual missing scheme
				but are updated in each wave corresponding to the participation behavior of respondents.
				For example, the numeric code corresponding to the participation behavior (p_participation) needs to be updated
				for those respondents who did not participate in the added wave. Additionally, the disposition code is change to "12 Invited"
				and the variable participation in this wave is recoded to "0" for this group. Lastly, the variable label of
				the number of complete interviews is updated (the information is already updated via the merge).*/
				
				/* Participation behavior concerning all waves */
				replace p_participation = p_participation  + `"-0"' if kp`num'_participation == . & sample == 5

				/* Disposition code */
				replace kp`num'_dispcode = 12 if kp`num'_participation == . & kp`num'_group == 1
				local pre_wave = `wave'-1
				foreach x of numlist 10 / `pre_wave' {
					replace kp`x'_dispcode = 12 if kp`x'_dispcode == . & sample == 5
					lab define `:value label kp`x'_dispcode' 12 "Eingeladen (12)", modify
					}

				/* Participation in added wave */
				replace kp`num'_participation = 0 if kp`num'_participation == .
				foreach x of numlist 10 / `pre_wave' {
					replace kp`x'_participation = 0 if  kp`x'_participation == . & sample == 5
					}

				/* Label variable "Number of Complete Participations" */
				label variable n_participation `"Anzahl vollstaendige Teilnahmen Welle 1 bis aktuelle Welle"'
				
				
				
				
				if `num' == 11 {
					foreach x of numlist `num' { 
					local lab_teilnahme_`x' `"`x' "`x' Teilnahmen" "' 
					local lab_teilnahme  `lab_teilnahme' `lab_teilnahme_`x''
					}
				}
				else {
					foreach x of numlist 11/`num' { 
					local lab_teilnahme_`x' `"`x' "`x' Teilnahmen" "' 
					local lab_teilnahme  `lab_teilnahme' `lab_teilnahme_`x''
					}					
				}

				lab def N_TEILNAHMEN 0 "keine vollstaendige Teilnahme" 1 "1 Teilnahme"  `lab_teilnahme' , modify
				lab val n_participation N_TEILNAHMEN
									
				/* Recode metadata:
				The field time variables are adjusted and the variables replaced with the values of the 
				complete field time of the GLES Panel (including all waves and all sample). More detailed information 
				for each wave and sample are included in the individual datasets and in the supplementary
				study description. */
				replace field_start = `"`field_start_SB'"' if sample == 5
				replace field_end = `"2019-07-08"' if `num' == 11 & kp`num'_participation != 0 & sample == 5
				replace field_end = `"2019-12-17"' if `num' == 12 & kp`num'_participation != 0 & sample == 5
				replace field_end = `"2020-06-02"' if `num' == 13 & kp`num'_participation != 0 & sample == 5
				replace field_end = `"2020-12-17"' if `num' == 14 & kp`num'_participation != 0 & sample == 5
				replace access_panel = -94 if sample == 5
				
				/* Reorder:
				Some variables are only included in sample B and need to be reordered.
				*/
				if inlist(`"`wave_select'"', `"10"' , `"11"', `"12"', `"13"', `"14"') {
				order  kp`num'_500 kp`num'_510 kp`num'_520  kp`num'_codingbegin kp`num'_codingend kp`num'_inbox , after(kp`num'_modus)
				}
				order wei9_ow wei10_mz wei11_mz, after(wei8_mz) 
				order kp`num'_2280, before(kp`num'_2601)
				order kp`num'_lastpage_sB, after(kp`num'_lastpage)
			}
		}
		
		/* Merge data set with recruitment wave of sample B
		Only the variable elecdist17_ is changed through the "update replace" command. The field_end
		variable is adjusted during the next stept, all other variables which are included in both 
		data sets, are already coded the same. */
		merge 1:1 lfdn using `"ZA`zanummer'_wrc_sB_v`release'.dta"', update replace gen(_mergeWrc)

		/* Missing Coding:
		Since not all respondents partcipated in all waves and samples, missings are recoded accordingly based
		on the GLES missing scheme. */
		unab all_vars : _all
		unab no_recode : wei* 
		local recode_vars : list all_vars-no_recode

			foreach var of varlist `recode_vars' {
			capture confirm numeric variable `var'
			if !_rc {
				qui recode `var' (. = -94) if sample != 5
				qui recode `var' (. = -95) if sample == 5
				qui capture label define `:value label `var'' ///
					-94 `"nicht in Auswahlgesamtheit"' ///
					-95 `"nicht teilgenommen"' , modify
				if _rc {
					qui label define `var' 	-94 `"nicht in Auswahlgesamtheit"' ///
											-95 `"nicht teilgenommen"' , modify
					qui label value `var' `var'
				}
			}
			else {
				replace `var' = `"-94 nicht in Auswahlgesamtheit"' if `var' == `""' & sample != 5
				replace `var' = `"-95 nicht teilgenommen"' if `var' == `""' & sample == 5

			}
		}
		
					
		/* save dataset */
		save `"ZA`zanummer'_`wave_select'waves_s`sample_select'_v`release'.dta"', replace 
		}

		
		
		
******************************************************************************************	





/* Matching sample B recruitment wave and wave 10 to current wave */

else if `"`wave_select'"' == `"all"' & `"`sample_select'"' == `"B"'   {

	/* Check if sample B recruitment wave and wave 10 to current wave is in directory */
	foreach num of numlist 10/`wave' {
		if `num' == 15 {
			dis as error `"Please note: Since Sample B was not interviewed in Wave 15, the file `"ZA`zanummer'_w15_sB_v`release'.dta"' does not exist and will not be appended."'
		}
		else if `num' != 15 {
			capture confirm file ZA`zanummer'_w`num'_sB_v`release'.dta
			if _rc == 0 {
			}
			else {
				dis as error `"ZA`zanummer'_w`num'_sB_v`release'.dta is not in the working directory! Please change the directory."'
				exit 198
			}
		}
	}
		
	capture confirm file ZA`zanummer'_wrc_sB_v`release'.dta
	if _rc == 0 {
	}
	else {
	dis as error `"ZA`zanummer'_wrc_sB_v`release'.dta is not in the working directory! Please change the directory."'
		exit 198
	}
		
		
	/* use data set with wave 10 sample B */
	use `"ZA`zanummer'_w10_sB_v`release'.dta"', clear
	
	/* Merge data set with additional wave(s) of sample B */
	foreach num of numlist 11/`wave' {

		if `num' == 15 {
			dis as error `"Please note: Since Sample B was not interviewed in Wave 15, the file `"ZA`zanummer'_w15_sB_v`release'.dta"' does not exist and will not be appended."'
		}
		else if `num' != 15 {
		
			merge 1:1 lfdn using `"ZA`zanummer'_w`num'_sB_v`release'.dta"', update replace generate(_mergeW`num'sB)
	
			/* Participation behavior concerning all waves */
			replace p_participation = p_participation  + `"-0"' if kp`num'_participation == . 

			/* Disposition code */
			replace kp`num'_dispcode = 12 if kp`num'_participation == . & kp`num'_group == 1
			local pre_wave = `num'-1
			foreach x of numlist 10 / `pre_wave' {
				replace kp`x'_dispcode = 12 if kp`x'_dispcode == . 
				lab define `:value label kp`x'_dispcode' 12 "Eingeladen (12)", modify
				}

			/* Participation in added wave */
			replace kp`num'_participation = 0 if kp`num'_participation == .
			foreach x of numlist 10 / `pre_wave' {
				replace kp`x'_participation = 0 if kp`x'_participation == .
				}

			/* Label variable "Number of Complete Participations" */
			label variable n_participation `"Anzahl vollstaendige Teilnahmen Welle 1 bis `num'"'
			
			/* Recode metadata:
			The field time variables are adjusted and the variables replaced with the values of the 
			complete field time of the GLES Panel (including all waves and all sample). More detailed information 
			for each wave and sample are included in the individual datasets and in the supplementary
			study description. */
			replace field_start = `"`field_start_SB'"' if sample == 5
			replace field_end = `"2019-07-08"' if `num' == 11 & kp`num'_participation != 0
			replace field_end = `"2019-12-17"' if `num' == 12 & kp`num'_participation != 0 
			replace field_end = `"2020-06-02"' if `num' == 13 & kp`num'_participation != 0
			replace field_end = `"2020-12-17"' if `num' == 14 & kp`num'_participation != 0 
			
			/* Reorder:
			Some variables need to be reordered.
			*/
			order wei9_ow wei10_mz wei11_mz, after(wei8_mz)
		}
	}
			
	rename field_end field_end_tmp
	
	/* merge data set with recruitment wave of sample B */
	merge 1:1 lfdn using `"ZA`zanummer'_wrc_sB_v`release'.dta"', gen(_mergeWrc)
	
	/* Recode Metadata:
	The field time variables are adjusted and the variables replaced with the values of the 
	complete field time of the GLES Panel (including all waves and all sample). */
	replace field_start = `"`field_start_SB'"' 
	replace field_end = field_end_tmp
	drop field_end_tmp
	
	/* Missing coding:
	Since not all respondents partcipated in all waves and samples, missings are recoded accordingly based
	on the GLES missing scheme. */

	/* Save all variable names, except for the weighting variables, in a local for recoding.  */
	unab all_vars : _all
	unab no_recode : wei* 
	local recode_vars : list all_vars-no_recode

	/* Recode all variables to -95 "nicht teilgenommen" ("not participated") for respondents, who
	a) did not participate in wave 10
	b) did participate in the GLES Panel Sample A for the first time in wave 10 (f.e., panelists who
	participated in the Short-term Campaign Panel 2013 have been invited to each Panel wave since) */

	foreach var of varlist `recode_vars' {
		capture confirm numeric variable `var'
		if !_rc {
			qui recode `var' (. = -95)
			qui capture lab def `:value label `var'' -95 `"nicht teilgenommen"', modify
			if _rc {
				qui lab def `var' -95 `"nicht teilgenommen"', modify
				qui lab val `var' `var'
			}
		}
		else {
			qui replace `var' = `"-95 nicht teilgenommen"' if `var'== `""'
		}
	}	
	
	/* Reorder:
	Some variables need to be reordered. */
	order  rc1_*, after(lfdn)
	
	/* Save dataset  */ 
	save `"ZA`zanummer'_`wave_select'waves_s`sample_select'_v`release'.dta"', replace 

	}
	
	
	
	
********************************************************************************

/* Matching sample A and B for a single wave */

else if inlist(`"`wave_select'"', `"10"' , `"11"', `"12"', `"13"', `"14"') & `"`sample_select'"' == `"AB"' {
	/* Check if  wave of sample A and B are in directory */
	capture confirm file ZA`zanummer'_w`wave_select'_sB_v`release'.dta
	if _rc == 0 {
	}
	else {
		dis as error `"ZA`zanummer'_w`wave_select'_sB_v`release'.dta is not in the working directory! Please change the directory."'
		exit 198
	}
	capture confirm file ZA`zanummer'_w`wave_select'_sA_v`release'.dta
	if _rc == 0 {
	}
	else {
		dis as error `"ZA`zanummer'_w`wave_select'_sA_v`release'.dta is not in the working directory! Please change the directory."'
		exit 198
	}

	/* Load sample A data set */
	use `"ZA`zanummer'_w`wave_select'_sA_v`release'.dta"', clear 

	/* Append data set with wave 10 sample B */
	append using `"ZA`zanummer'_w`wave_select'_sB_v`release'.dta"', gen(_appendW`wave_select')
	
	/* Recode metadata:
	The field time variables are adjusted and the variables replaced with the values of the 
	complete field time of the GLES Panel (including all waves and all sample). More detailed information 
	for each wave and sample are included in the individual datasets and in the supplementary
	study description. */
	replace field_start = `"`field_start_SB'"' if sample == 5
	replace field_end = `"2019-01-31"' if `wave_select' == 10 & sample == 5
	replace field_end = `"2019-07-08"' if `wave_select' == 11 & sample == 5
	replace field_end = `"2019-12-17"' if `wave_select' == 12 & sample == 5
	replace field_end = `"2020-06-02"' if `wave_select' == 13 & sample == 5
	replace field_end = `"2020-12-17"' if `wave_select' == 14 & sample == 5
	replace access_panel = -94 if sample == 5

	/* Recode sociodemographics:
	Sociodemographics are not included in all waves, but if questions are included, it might be necessary
	to update the time-invariant variables ("kpX_") if the information was missing before. */
	replace kpx_2280 = kp`wave_select'_2280 if sample == 5 & kp`wave_select'_participation != 0
	local flag = `wave_select' - 6
	replace kpx_2280flag = `flag' if kp`wave_select'_participation != 0 & kpx_2280flag == .
	qui capture label define `:value label kpx_2280flag' `flag' `"Angabe in Welle `wave_select' gemacht"', modify
	
	/* Adjust value labels if necessary */
	qui capture label define `:value label kp`wave_select'_1631' 10 `"kein Internetzugang vorhanden"', modify


	/* Missing Coding:
		Since not all respondents partcipated in all waves and samples, missings are recoded accordingly based
		on the GLES missing scheme. */
		unab all_vars : _all
		unab no_recode : wei* 
		local recode_vars : list all_vars-no_recode

		foreach var of varlist `recode_vars' {
		capture confirm numeric variable `var'
		if !_rc {
			qui recode `var' (. = -94)
			qui capture lab def `:value label `var'' -94 `"nicht in Auswahlgesamtheit"', modify
			if _rc {
				qui lab def `var' -94 `"nicht in Auswahlgesamtheit"', modify
				qui lab val `var' `var'
			}
		}
		else {
			qui replace `var' = `"-94 nicht in Auswahlgesamtheit"' if `var'== `""'
		}
	}
		
	/* Reorder:
	Some variables are only included in sample B and need to be reordered.
	*/
	if `"`wave_select'"' == `"10"' {
		order  kp`wave_select'_500 kp`wave_select'_510 kp`wave_select'_520  kp`wave_select'_codingbegin kp`wave_select'_codingend kp`wave_select'_inbox kp`wave_select'_info, after(kp`wave_select'_modus)
		order wei6_ow wei7_mz wei8_mz, after(wei5_on) 
	}
	else if inlist(`"`wave_select'"', `"11"', `"12"', `"13"', `"14"') {
		order  kp`wave_select'_500 kp`wave_select'_510 kp`wave_select'_520  kp`wave_select'_codingbegin kp`wave_select'_codingend kp`wave_select'_inbox, after(kp`wave_select'_modus)
		order wei9_ow wei10_mz wei11_mz, after(wei5_on) 
	}
	else {
		order wei9_ow wei10_mz wei11_mz, after(wei5_on)
	}

	order kp`wave_select'_2280, before(kp`wave_select'_2601)
	order kp`wave_select'_lastpage_sB, after(kp`wave_select'_lastpage)
	
	/* Save dataset  */
		save `"ZA`zanummer'_wave`wave_select'_s`sample_select'_v`release'.dta"', replace
}


 
