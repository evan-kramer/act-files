clear all
set more off, perm
set type double, perm
capture log close
macro drop _all
program drop _all
estimates drop _all

/*
Producing ACT Files 
Evan Kramer
6/13/2017
*/

* Define macros
global grad = "K:/ORP_accountability/data/2016_graduation_rate/student_level_20161201.csv" 
global sas = "K:/ORP_accountability/data/2016_ACT/ACT_Student_ID_Crosswalk_From_SAS_10.10.16.csv" 
global act15 = "K:/ORP_accountability/data/2015_ACT/TN_790Select_2015.dta"
global act16 = "K:/ORP_accountability/data/2016_ACT/TN_790Select_2016.dta" 
global out = "K:/ORP_accountability/data/2016_ACT"
local ret = 0
local act = 0
local stu = 1
local dis = 0
local sch = 0
local sta = 0
local reg = 0

* Retake
if `ret' == 1 {
	* Read in retake file
	# delimit ;
	infix
		year 1-2
		str last 3-27
		str first 28-43
		str middle 44-44
		str address 45-84
		str gender 88-88
		grade 89-90
		act_id 91-99
		dob 101-106
		str city 117-141
		zip 146-154
		state_id 192-204
		act_hs_code 205-210
		test_date 233-236
		str english 773-774
		str math 775-776
		str reading 777-778
		str science 779-780
		str composite 781-782
		
		using "K:Research_Transfers/ACT Files/2016 Retake/DY2016_A_S_ACT-DATA_TN_Retake.txt", clear;
	# delimit cr
	
	* Clean
	* Look at ACT IDs
	* Check that all students are getting matched
	
}

* Cohort file
if `act' == 1 {
	* Read in highest score file
	# delimit ;
	infix 
		str first 3-27
		str last 28-43
		str mi 44-44
		str dob 155-162
		str id 192-204
		str test_date 227-232
		str id2 691-700
		english 731-732
		math 733-734
		reading 735-736
		science 737-738
		composite 739-740
		
		using "K:\ORP_accountability\data\2016_ACT/DY2016_tn_S43_ACT-DATA_TENNESSEE_DEPT_OF_ED_with_highest_scores_80952 (1).txt", clear;
	# delimit cr

	* Clean
	gen dob_yr = substr(dob, 1, 4), after(dob)
	gen dob_mo = substr(dob, 5, 2), after(dob_yr)
	gen dob_da = substr(dob, 7, 2), after(dob_mo)
	replace dob = dob_yr + "-" + dob_mo + "-" + dob_da
	gen _dob = date(dob, "YMD", 2050), after(dob)
	format _dob %td
	drop dob* 
	rename _dob dob
	destring id id2, force replace
	replace id2 = id if id != id2

	* Try to match records
	keep if id == .

	exit
}

* Student-level
if `stu' == 1 {
	* Read in and clean grad file - universe of students
	import delimited using "$grad", clear
	keep if included_in_cohort == "Y" & completion_type == 1
	keep student_key *t_name gender system school race_ethnicity econ_dis ell sped
	
	* Use SAS student-level to check first
	tempfile grad
	save `grad', replace
	import delimited using "$sas", clear

	** Resolve duplicates and merge
	/*
	local english = 18
	local math = 22
	local reading = 22
	local science = 23
	foreach s in english math reading science {
		gen cr_`s' = `s' >= ``s'' & `s' != . & ``s'' != .
	}
	egen total_cr = rowtotal(cr_*)
	drop cr_*
	*/
		
	gsort state_student_id_new -composite /*-total_cr*/ -english -math -reading -science /* figure out which observation has the highest number of college-ready scores and keep that observation; */
	collapse (first) english math reading science composite, by(state_student_id_new)
	mmerge state_student_id_new using `grad', type(n:1) umatch(student_key)
	drop if _merge <= 1
	drop _merge

	* Look in state day files from 2015 and 2016 -- NEED TO ADD IN THE PART ABOUT THE MAX NUMBER OF CR BENCHMARKS
	tempfile act
	save `act'
	use "$act15", clear
	append using "$act16" 
	drop if test_location == "M"
	gsort state_stud_id -act_composite -act_eng -act_math -act_read -act_sci
	collapse (firstnm) *t_name act_*, by(state_stud_id)

	* Merge with other ACT files
	rename (act_eng act_read act_sci) (act_english act_reading act_science)
	mmerge state_stud_id using `act', type(n:1) umatch(state_student_id_new) /* no other records from state day files that were not in the cohort file */
	foreach v in english math reading science composite {
		replace `v' = act_`v' if `v' == . & act_`v' != .
	}
	drop if _merge <= 1
	drop _merge act_*
	
	local english = 18
	local math = 22
	local reading = 22
	local science = 23
	foreach s in english math reading science {
		gen n_cr_`s' = `s' >= ``s'' & `s' != . & ``s'' != .
	}
	gen n_cr_all = n_cr_english == 1 & n_cr_math == 1 & n_cr_reading == 1 & n_cr_science == 1
	order system school, first
	
	* Output file
	compress
	save "$out/act_student_level_EK", replace
	
	
	/* 
	*Check against JP's file
	keep state_stud_id english math reading science composite
	foreach v in english math reading science composite {
		rename `v' `v'_ek
	}
	rename state_stud_id studentkey
	mmerge studentkey using "K:/ORP_accountability/data/2016_ACT/2017_ACT_student_level", type(1:1)
	rename act_composite composite
	foreach v in english math reading science composite {
		order `v', after(`v'_ek)
		display "`v'"
		count if `v' != `v'_ek
	}
	*/
}

* District-level
if `dis' == 1 {
	use "$out/act_student_level_EK", clear
	
	/*
	use "$out/2017_ACT_student_level", clear
	rename (act_composite met_All4_CRB) (composite n_cr_all)
	foreach v in english math reading science {
		rename met_CRB_`v' n_cr_`v'
	}
	keep if system == 950
	collapse (mean) english math reading science composite
	*/
	
	gen enrolled = 1
	gen tested = composite != .
	gen valid_tests = tested == 1
	foreach v in english math reading science composite {
		replace `v' = . if valid_tests != 1
	}
	gen n_21_or_higher = composite >= 21 & composite != .
	gen n_below_19 = composite < 19
	gen n_female_21_or_higher = n_21_or_higher == 1 & gender == "F"
	gen n_male_21_or_higher = n_21_or_higher == 1 & gender == "M"
	gen n_gender_missing_21_or_higher = n_21_or_higher == 1 & gender == ""
	gen n_female_below_19 = n_below_19 == 1 & gender == "F"
	gen n_male_below_19 = n_below_19 == 1 & gender == "M"
	gen n_gender_missing_below_19 = n_below_19 == 1 & gender == ""
	gen n_female = gender == "F" & valid_tests == 1
	gen n_male = gender == "M" & valid_tests == 1
	gen n_gender_missing = gender == "" & valid_tests == 1
	gen subgroup = "All Students"
	
	/*
	tempfile pre
	save `pre', replace
	collapse (mean) english math reading science composite (sum) enrolled tested valid_tests n_* 
	gen system = 0
	tempfile state
	save `state', replace
	
	use `pre', clear
	collapse (mean) english math reading science composite (sum) enrolled tested valid_tests n_*, by(system)
	append using `state'
	gsort system
	
	foreach v in science reading math english {
		replace `v' = round(`v', 0.1)
		gen pct_cr_`v' = round(100 * n_cr_`v' / valid_tests, 0.1), after(valid_tests)
	}
	replace composite = round(composite, 0.1)
	gen pct_cr_all = round(100 * n_cr_all / valid_tests, 0.1), after(pct_cr_science)
	
	foreach p in below_19 21_or_higher {
		gen pct_`p' = round(100 * n_`p' / valid_tests, 0.1), after(n_`p')
		foreach v in female male gender_missing {
			gen pct_`v'_`p' = round(100 * n_`v'_`p' / n_`v', 0.1), after(n_`v'_`p')
		}
	}
	
	*
	tempfile pre 
	save `pre', replace
	collapse (mean) english math reading science composite ///
		(sum) enrolled tested valid_tests n_* (firstnm) subgroup
	gen system = 0 
	tempfile state 
	save `state', replace
	use `pre', clear
	*
	*/
		
	collapse (mean) english math reading science composite ///
		(sum) enrolled tested valid_tests n_* (firstnm) subgroup, by(system)
		
	foreach v in english math reading science {
		replace `v' = round(`v', 0.1)
		gen pct_cr_`v' = round(100 * n_cr_`v' / valid_tests, 0.1), after(n_cr_`v')
		rename `v' avg_`v'
	}
	replace composite = round(composite, 0.1)
	rename composite avg_composite
	gen pct_cr_all = round(100 * n_cr_all / valid_tests, 0.1), after(n_cr_all)
	
	foreach p in below_19 21_or_higher {
		gen pct_`p' = round(100 * n_`p' / valid_tests, 0.1), after(n_`p')
		foreach v in female male gender_missing {
			gen pct_`v'_`p' = round(100 * n_`v'_`p' / n_`v', 0.1), after(n_`v'_`p')
		}
	}
	gen participation_rate = round(100 * tested / enrolled, 1), after(tested)
	drop n_male n_female n_gender_missing
	order subgroup, after(system)
	gsort system
	
	* Output file
	compress
	save "$out/act_district_level_EK", replace
	
	/*
	*Check against JP's file
	keep system avg_english avg_math avg_reading avg_science avg_composite participation_rate ///
		valid_tests n_cr_english n_cr_math n_cr_reading n_cr_science n_cr_all n_21_or_higher ///
		n_below_19 n_female_21_or_higher n_male_21_or_higher n_female_below_19 n_male_below_19 n_female n_male
	foreach v in avg_english avg_math avg_reading avg_science avg_composite participation_rate valid_tests n_cr_english n_cr_math n_cr_reading n_cr_science n_cr_all n_21_or_higher n_below_19 n_female_21_or_higher n_male_21_or_higher n_female_below_19 n_male_below_19 n_female n_male {
		rename `v' `v'_ek
	}	

	mmerge system using "$out/ACT_district2017", type(1:n)
	
	keep if subgroup == "All Students" 
	foreach v in english math reading science {
		rename `v'_avg avg_`v' 
		rename met_CRB_`v' n_cr_`v'
	}
	
	rename (act_composite_avg n_21_orhigher n_below19 met_All4_CRB male_n_21_orhigher ///
		female_n_21_orhigher male_n_below19 female_n_below19 female_count male_count) ///
		(avg_composite n_21_or_higher n_below_19 n_cr_all n_male_21_or_higher n_female_21_or_higher n_male_below_19 n_female_below_19 n_female n_male)
	
	foreach v in avg_english avg_math avg_reading avg_science avg_composite participation_rate valid_tests n_cr_english n_cr_math n_cr_reading n_cr_science n_cr_all n_21_or_higher n_below_19 n_female_21_or_higher n_male_21_or_higher n_female_below_19 n_male_below_19 n_female n_male {
		order `v', after(`v'_ek)
		display "`v'"
		count if `v' != `v'_ek
	}
	*/
}

* School-level
if `sch' == 1 {
	use "$out/act_student_level_EK", clear
	gen enrolled = 1
	gen tested = composite != .
	gen valid_tests = tested == 1
	foreach v in english math reading science composite {
		replace `v' = . if valid_tests != 1
	}
	gen n_21_or_higher = composite >= 21 & composite != .
	gen n_below_19 = composite < 19
	gen n_female_21_or_higher = n_21_or_higher == 1 & gender == "F"
	gen n_male_21_or_higher = n_21_or_higher == 1 & gender == "M"
	gen n_gender_missing_21_or_higher = n_21_or_higher == 1 & gender == ""
	gen n_female_below_19 = n_below_19 == 1 & gender == "F"
	gen n_male_below_19 = n_below_19 == 1 & gender == "M"
	gen n_gender_missing_below_19 = n_below_19 == 1 & gender == ""
	gen n_female = gender == "F" & valid_tests == 1
	gen n_male = gender == "M" & valid_tests == 1
	gen n_gender_missing = gender == "" & valid_tests == 1
	gen subgroup = "All Students"
	
	collapse (mean) english math reading science composite ///
		(sum) enrolled tested valid_tests n_* (firstnm) subgroup, by(system school)
		
	foreach v in english math reading science {
		replace `v' = round(`v', 0.1)
		gen pct_cr_`v' = round(100 * n_cr_`v' / valid_tests, 0.1), after(n_cr_`v')
		rename `v' avg_`v'
	}
	replace composite = round(composite, 0.1)
	rename composite avg_composite
	gen pct_cr_all = round(100 * n_cr_all / valid_tests, 0.1), after(n_cr_all)
	
	foreach p in below_19 21_or_higher {
		gen pct_`p' = round(100 * n_`p' / valid_tests, 0.1), after(n_`p')
		foreach v in female male gender_missing {
			gen pct_`v'_`p' = round(100 * n_`v'_`p' / n_`v', 0.1), after(n_`v'_`p')
		}
	}
	gen participation_rate = round(100 * tested / enrolled, 1), after(tested)
	drop n_male n_female n_gender_missing
	order subgroup, after(school)
	gsort system school
	
	* Output file
	compress
	save "$out/act_school_level_EK", replace 
	
	/* 
		*Check against JP's file
	tempfile ek
	save `ek', replace
	
	use "$out/ACT_school2017", clear
	keep if subgroup == "All Students" 
	foreach v in english math reading science {
		rename `v'_avg avg_`v' 
		rename met_CRB_`v' n_cr_`v'
	}
	rename (act_composite_avg n_21_orhigher n_below19 met_All4_CRB male_pct21orhigher ///
		female_pct21orhigher male_pctbelow19 female_pctbelow19) ///
		(avg_composite n_21_or_higher n_below_19 n_cr_all pct_male_21_or_higher pct_female_21_or_higher pct_male_below_19 pct_female_below_19)
	
	tempfile jp
	save `jp', replace
	
	use `ek', clear
	foreach v in avg_english avg_math avg_reading avg_science avg_composite n_cr_english pct_cr_english n_cr_math pct_cr_math n_cr_reading pct_cr_reading n_cr_science pct_cr_science n_cr_all pct_cr_all enrolled tested participation_rate valid_tests n_21_or_higher pct_21_or_higher n_below_19 pct_below_19 pct_female_21_or_higher pct_male_21_or_higher pct_female_below_19 pct_male_below_19 n_female n_male {
		rename `v' `v'_ek
	}
		
	mmerge system school using `jp', type(1:1)
	foreach v in avg_english avg_math avg_reading avg_science avg_composite participation_rate valid_tests n_cr_english n_cr_math n_cr_reading n_cr_science n_cr_all n_21_or_higher n_below_19 pct_female_21_or_higher pct_male_21_or_higher pct_female_below_19 pct_male_below_19 {
		order `v', after(`v'_ek)
		display "`v'"
		count if `v' != `v'_ek
	}
	*/
}

* State-level
if `sta' == 1 {
	use "$out/act_student_level_EK", clear
	gen enrolled = 1
	gen tested = composite != .
	gen valid_tests = tested == 1
	foreach v in english math reading science composite {
		replace `v' = . if valid_tests != 1
	}
	gen n_21_or_higher = composite >= 21 & composite != .
	gen n_below_19 = composite < 19
	gen n_female_21_or_higher = n_21_or_higher == 1 & gender == "F"
	gen n_male_21_or_higher = n_21_or_higher == 1 & gender == "M"
	gen n_gender_missing_21_or_higher = n_21_or_higher == 1 & gender == ""
	gen n_female_below_19 = n_below_19 == 1 & gender == "F"
	gen n_male_below_19 = n_below_19 == 1 & gender == "M"
	gen n_gender_missing_below_19 = n_below_19 == 1 & gender == ""
	gen n_female = gender == "F" & valid_tests == 1
	gen n_male = gender == "M" & valid_tests == 1
	gen n_gender_missing = gender == "" & valid_tests == 1
	gen subgroup = "All Students"
	
	collapse (mean) english math reading science composite ///
		(sum) enrolled tested valid_tests n_* (firstnm) subgroup
		
	foreach v in english math reading science {
		replace `v' = round(`v', 0.1)
		gen pct_cr_`v' = round(100 * n_cr_`v' / valid_tests, 0.1), after(n_cr_`v')
		rename `v' avg_`v'
	}
	replace composite = round(composite, 0.1)
	rename composite avg_composite
	gen pct_cr_all = round(100 * n_cr_all / valid_tests, 0.1), after(n_cr_all)
	
	foreach p in below_19 21_or_higher {
		gen pct_`p' = round(100 * n_`p' / valid_tests, 0.1), after(n_`p')
		foreach v in female male gender_missing {
			gen pct_`v'_`p' = round(100 * n_`v'_`p' / n_`v', 0.1), after(n_`v'_`p')
		}
	}
	gen participation_rate = round(100 * tested / enrolled, 1), after(tested)
	drop n_male n_female n_gender_missing
	order subgroup, first

	* Output file
	compress
	save "$out/act_state_level_EK", replace
}

* By CORE region
if `reg' == 1 {
	use "$out/act_student_level_EK", clear
	gen enrolled = 1
	gen tested = composite != .
	gen valid_tests = tested == 1
	foreach v in english math reading science composite {
		replace `v' = . if valid_tests != 1
	}
	gen n_21_or_higher = composite >= 21 & composite != .
	gen n_below_19 = composite < 19
	gen n_female_21_or_higher = n_21_or_higher == 1 & gender == "F"
	gen n_male_21_or_higher = n_21_or_higher == 1 & gender == "M"
	gen n_gender_missing_21_or_higher = n_21_or_higher == 1 & gender == ""
	gen n_female_below_19 = n_below_19 == 1 & gender == "F"
	gen n_male_below_19 = n_below_19 == 1 & gender == "M"
	gen n_gender_missing_below_19 = n_below_19 == 1 & gender == ""
	gen n_female = gender == "F" & valid_tests == 1
	gen n_male = gender == "M" & valid_tests == 1
	gen n_gender_missing = gender == "" & valid_tests == 1
	gen subgroup = "All Students"
	
	* Merge on CORE region
	mmerge system using "C:/Users/CA19130/Documents/Data/Crosswalks/core_region_crosswalk", type(n:1)
	keep if _merge == 3 & region != ""
	drop analyst email director _merge 
	order system_name, after(system)
	
	collapse (mean) english math reading science composite ///
		(sum) enrolled tested valid_tests n_* (firstnm) subgroup, by(region)
		
	foreach v in english math reading science {
		replace `v' = round(`v', 0.1)
		gen pct_cr_`v' = round(100 * n_cr_`v' / valid_tests, 0.1), after(n_cr_`v')
		rename `v' avg_`v'
	}
	replace composite = round(composite, 0.1)
	rename composite avg_composite
	gen pct_cr_all = round(100 * n_cr_all / valid_tests, 0.1), after(n_cr_all)
	
	foreach p in below_19 21_or_higher {
		gen pct_`p' = round(100 * n_`p' / valid_tests, 0.1), after(n_`p')
		foreach v in female male gender_missing {
			gen pct_`v'_`p' = round(100 * n_`v'_`p' / n_`v', 0.1), after(n_`v'_`p')
		}
	}
	gen participation_rate = round(100 * tested / enrolled, 1), after(tested)
	drop n_male n_female n_gender_missing
	order subgroup, after(region)
	
	append using "$out/act_state_level_EK"
	gsort region, mfirst
	replace region = "State" if region == ""
		
	* Output file
	compress
	save "$out/act_regional_level_EK", replace
	*export excel using "C:/Users/CA19130/Desktop/act_regional_level", replace firstrow(var)
}

