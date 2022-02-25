//RISCIS Merging of Files

// Defining WORKING DIRECTORY This should be the one with the raw SAS files
cd "C:\Users\Ali\OneDrive\Projects\RISCIS\analysis"

// DDANy

import sas using "daany.sas7bdat"

bysort SIC: keep if _n==1


keep SIC __SUBJECTKEY __STUDYOID PSITEID AGE SEX HEIGHT BMI ETHNIC RACE RACEOTH trtgrp

save "tempmerg_1.dta" , replace 

// DACCTG

clear
import sas using "dacctg.sas7bdat"

keep  SIC  visit da_ASIA da_MO_LW_TOT da_MO_TOT da_MO_UP_TOT da_selfcare da_sf36_all da_asia_any da_sf36_any da_grassp_any da_injsev_any da_eq5d_any da_scim_any

rename * *_


reshape wide da_ASIA da_MO_LW_TOT da_MO_TOT da_MO_UP_TOT da_selfcare da_sf36_all da_asia_any da_sf36_any da_grassp_any da_injsev_any da_eq5d_any da_scim_any, i(SIC) j(visit)

rename SIC_ SIC

merge 1:1 SIC using "tempmerg_1.dta"

drop _merge

save "tempmerg_1.dta" , replace 

// DAE

clear
import sas using "dasia.sas7bdat"

keep   SIC  AIS  DATEASIA  ASSNL NLI MO_UR_TOT MO_UL_TOT MO_LR_TOT MO_LL_TOT MO_R_TOT MO_L_TOT MO_UP_TOT MO_LW_TOT MO_TOT SE_LTR_TOT SE_LTL_TOT SE_PPR_TOT SE_PPL_TOT SE_LT_TOT SE_PP_TOT SE_LEV_R SE_LEV_L MO_LEV_R MO_LEV_L ASIA ASVAC ASDAS VISIT

gen NLI_N = .
replace NLI_N = 1 if NLI== "C1"
replace NLI_N = 2 if NLI== "C2"
replace NLI_N = 3 if NLI== "C3"
replace NLI_N = 4 if NLI== "C4"
replace NLI_N = 5 if NLI== "C5"
replace NLI_N = 6 if NLI== "C6"
replace NLI_N = 7 if NLI== "C7"
replace NLI_N = 9 if NLI== "C8"
replace NLI_N = 10 if NLI== "T1"
replace NLI_N = 11 if NLI== "T2"
replace NLI_N = 12 if NLI== "T3"
replace NLI_N = 13 if NLI== "T4"
replace NLI_N = 14 if NLI== "T5"
replace NLI_N = 15 if NLI== "T6"
replace NLI_N = 16 if NLI== "T7"
replace NLI_N = 17 if NLI== "T8"
replace NLI_N = 18 if NLI== "T9"
replace NLI_N = 19 if NLI== "T10"
replace NLI_N = 20 if NLI== "T12"
replace NLI_N = 21 if NLI== "L1"
replace NLI_N = 22 if NLI== "L2"
replace NLI_N = 23 if NLI== "L3"
replace NLI_N = 24 if NLI== "L4"
replace NLI_N = 25 if NLI== "L5"
replace NLI_N = 26 if NLI== "S1"
replace NLI_N = 27 if NLI== "S2"
replace NLI_N = 28 if NLI== "S3"
replace NLI_N = 29 if NLI== "S4-5"


rename * *_

rename  SIC_ SIC
rename VISIT_ visit

reshape wide  DATEASIA_ ASVAC_ ASDAS_ ASSNL_ ASIA_ MO_UR_TOT_ MO_UL_TOT_ MO_LR_TOT_ MO_LL_TOT_ MO_R_TOT_ MO_L_TOT_ MO_UP_TOT_ MO_LW_TOT_ MO_TOT_ SE_LTR_TOT_ SE_LTL_TOT_ SE_PPR_TOT_ SE_PPL_TOT_ SE_LT_TOT_ SE_PP_TOT_ SE_LEV_R_ SE_LEV_L_ MO_LEV_R_ MO_LEV_L_ NLI_ AIS_ NLI_N_, i(SIC) j(visit) 



merge 1:1 SIC using "tempmerg_1.dta"
drop _merge
save "tempmerg_1.dta" , replace 


// DCCI


clear
import sas using "DCCI.sas7bdat"


keep SIC CCIMI CCICH CCIPV CCICD CCIDE CCICP CCICT CCIUD CCIML CCISL CCIDBND CCIDBWD CCIHM CCIRD CCITM CCILK CCILY CCIMST CCIADS AGE_SCORE COM_SCORE CCI CCI_PROB

merge 1:1 SIC using "tempmerg_1.dta"
drop _merge
save "tempmerg_1.dta" , replace 

// DDEMO


clear
import sas using "DDEMO.sas7bdat"

keep SIC ELIGFL ENRFL COMPFL WDREAS WDREASOT FDOSDT LDOSDT SURGDT WDDT LASTDT LASTVIS INJURY_DTM DA_ANY_EFFICACY_180 SITENAME

merge 1:1 SIC using "tempmerg_1.dta"
drop _merge
save "tempmerg_1.dta" , replace 

//DEQ5D

clear
import sas using "DEQ5D.sas7bdat"

keep SIC EQ5D1 EQ5D2 EQ5D3 EQ5D4 EQ5D5 EQ5D6 EQ_INDEX VISIT

rename * *_
rename  SIC_ SIC
rename VISIT_ visit

reshape wide  EQ5D1_ EQ5D2_ EQ5D3_ EQ5D4_ EQ5D5_ EQ5D6_ EQ_INDEX_, i(SIC) j(visit) 

merge 1:1 SIC using "tempmerg_1.dta"
drop _merge
save "tempmerg_1.dta" , replace 

//DGRASSP
clear
import sas using "DGRASSP.sas7bdat"


keep SIC visit GRDHPRE GRDHPOS GRS_R GRS_L GRS GRSWM_R GRSWM_L GRSWM GRTOTAL_R GRTOTAL_L GRTOTAL
rename * *_

rename  SIC_ SIC
rename visit_ visit

reshape wide  GRDHPRE_ GRDHPOS_ GRS_R_ GRS_L_ GRS_ GRSWM_R_ GRSWM_L_ GRSWM_ GRTOTAL_R_ GRTOTAL_L_ GRTOTAL_, i(SIC) j(visit) 
merge 1:1 SIC using "tempmerg_1.dta"
drop _merge
save "tempmerg_1.dta" , replace 

//DPAINNRS
clear
import sas using "DPAINNRS.sas7bdat"

keep SIC VISIT PAINBD

rename * *_
rename  SIC_ SIC
rename VISIT_ visit


reshape wide  PAINBD_, i(SIC) j(visit)

merge 1:1 SIC using "tempmerg_1.dta"
drop _merge
save "tempmerg_1.dta" , replace 

//DPTACCTG_WIDE

clear
import sas using "DPTACCTG_WIDE.sas7bdat"

keep actualany1 actualany2 actualany3 actualany4 actualany5 actualany6 actualany7 actualwin1 actualwin2 actualwin3 actualwin4 actualwin5 actualwin6 actualwin7 expected1 expected2 expected3 expected4 expected5 expected6 expected7 death1 death2 death3 death4 death5 death6 death7 theoretical1 theoretical2 theoretical3 theoretical4 theoretical5 theoretical6 theoretical7 ineligible1 ineligible2 ineligible3 ineligible4 ineligible5 ineligible6 ineligible7 ptwd1 ptwd2 ptwd3 ptwd4 ptwd5 ptwd6 ptwd7 invwd1 invwd2 invwd3 invwd4 invwd5 invwd6 invwd7 WDREASOT WDREAS WDDT SIC lastvis lastvisdt invwdvis

merge 1:1 SIC using "tempmerg_1.dta"
drop _merge
save "tempmerg_1.dta" , replace 


//DSCIM

clear
import sas using "DSCIM.sas7bdat"

keep SIC selfcare mobility rsmgmt TOTAL_SCIM MOBOUTDR MOBINDR visit

rename * *_
rename  SIC_ SIC
rename visit_ visit

reshape wide  MOBINDR_ MOBOUTDR_ TOTAL_SCIM_ mobility_ rsmgmt_ selfcare_ , i(SIC) j(visit)

merge 1:1 SIC using "tempmerg_1.dta"
drop _merge
save "tempmerg_1.dta" , replace 

//DSF36
clear
import sas using "DSF36.sas7bdat"

keep SIC _1 VISIT BP_T EM_T EN_T GH_T PF_T RE_T RP_T SF_T AGG_PHYS_T AGG_MENT_T FSFM1 FSFM2

rename * *_
rename  SIC_ SIC
rename VISIT_ visit

reshape wide  _1 BP_T_ EM_T_ EN_T_ GH_T_ PF_T_ RE_T_ RP_T_ SF_T_ AGG_PHYS_T_ AGG_MENT_T_ FSFM1_ FSFM2_, i(SIC) j(visit)

merge 1:1 SIC using "tempmerg_1.dta"
drop _merge
save "tempmerg_1.dta" , replace 


save  "RISCISDATAv1.0.dta" , replace




//open file

use "RISCISDATAv1.0.dta"








// Generating difference valriables

gen TOTMDiff6m = MO_TOT_6 -MO_TOT_1

gen UEMDiff6m = MO_UP_TOT_6- MO_UP_TOT_1

gen LEMDiff6m = MO_LW_TOT_6- MO_LW_TOT_1

gen NLIDiff6m = NLI_N_6- NLI_N_1

replace ASIA_1 = . if ASIA_1 == 9
generate baseASIA = ""
replace baseASIA = "A" if ASIA_1 == 1
replace baseASIA = "B" if ASIA_1 == 2
replace baseASIA = "C" if ASIA_1 == 3
replace baseASIA = "D" if ASIA_1 == 4


//LABELLING

label define Treatment 1 "Riluzole" 2 "Placebo"
label values trtgrp Treatment
label variable UEMDiff6m "UEM Change at 6 Months"
label variable TOTMDiff6m "Total Motor Change at 6 Months"
label variable LEMDiff6m "LEM Change at 6 Months"
label variable NLIDiff6m "NLI Change at 6 Months"