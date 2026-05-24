# =============================================================================
# COSurgCo — Script 04a: Clean & Recode Pilot Dataset
# Input:  data/raw/COSECSASurgicalAudit-PilotData_DATA_LABELS_2026-04-18_1152.csv
# Output: data/processed/pilot_adults.rds / pilot_paeds.rds
#
# Key harmonisation notes:
#  - Pilot sites: 7, 8, 9, 11 (implementation: 13,14,18,19,20,21,22)
#  - 30-day outcome (col 95) = death_30d; death_inhospital not recorded → NA
#  - Payment mode, facilities_prior, NGT not in pilot → NA
#  - Return to theatre col = 30-day return only → return_30d_yn
#  - Adult surgical types are broader — non-standard groups → "Other"
#  - Duplicate column names handled by positional deduplication
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(lubridate)
  library(stringr); library(purrr); library(tidyr)
})

root_dir  <- file.path("/Users","matemba","Library","CloudStorage",
                        "OneDrive-Personal","cosecsa_surgical_audits")
proc_dir  <- file.path(root_dir, "data", "processed")
dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)

pilot_csv <- file.path(root_dir, "data", "raw",
  "COSECSASurgicalAudit-PilotData_DATA_LABELS_2026-04-18_1152.csv")

clean_names_dedup <- function(df) {
  nms <- names(df) |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("_+", "_") |>
    str_replace_all("^_|_$", "")
  seen <- list()
  for (i in seq_along(nms)) {
    nm <- nms[i]
    if (nm %in% names(seen)) nms[i] <- paste0(nm, "_", i)
    seen[[nm]] <- TRUE
  }
  names(df) <- nms; df
}

chk <- function(x) if_else(str_to_lower(x) == "checked", 1L, 0L, NA_integer_)
parse_age_years <- function(x) {
  x <- str_squish(tolower(as.character(x)))
  m <- str_match(x, "([0-9.]+)\\s*(month|months)")
  if (!is.na(m[1,1])) return(as.numeric(m[1,2]) / 12)
  as.numeric(str_extract(x, "[0-9]+\\.?[0-9]*"))
}

raw <- read_csv(pilot_csv, show_col_types = FALSE, name_repair = "unique") |>
  clean_names_dedup()

d <- raw |>
  rename(
    study_id          = record_id,
    dob               = date_of_birth_of_the_patient,
    date_admission    = date_on_admission,
    age_raw           = age_of_the_patient_at_time_of_surgery_months_years,
    sex               = gender_of_the_patient,
    weight_done       = patient_s_weight,
    weight_kg         = patient_s_weight_kg,
    height_done       = patient_s_height,
    height_m          = patient_s_height_in_meters,
    hb_gdl            = hb_level_g_dl_immediate_before_surgery,
    date_surgery      = date_of_surgery,
    age_group_redcap  = age_group_of_the_patient,
    surg_type_adult   = type_of_surgery_adults,
    surg_name_adult   = type_of_surgery_performed_adult,
    surg_type_paeds   = types_of_surgery_paediatrics,
    surg_name_paeds   = type_of_surgery_performed_paediatrics,
    cm_cad        = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_coronary_artery_disease,
    cm_chf        = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_congestive_heart_failure,
    cm_dm         = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_diabetes_mellitus,
    cm_cancer     = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_metastatic_cancer,
    cm_htn        = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_hypertension,
    cm_stroke     = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_stroke,
    cm_copd       = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_chronic_obstructive_pulmonary_disease,
    cm_hiv        = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_hiv,
    cm_renal      = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_chronic_renal_disease,
    cm_asthma     = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_asthma,
    cm_blood      = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_blood_related_conditions_e_g_sickle_cell_anaemia_haemophilia,
    cm_cirrhosis  = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_cirrhosis,
    cm_malnutrition= did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_malnutrition,
    cm_none       = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_none_of_the_above,
    cm_other      = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_other_specify,
    urgency          = urgency_of_operation,
    asa              = asa_physical_status_score,
    prophy_abx       = was_prophylactic_antibiotics_given_pre_operative,
    prophy_abx_timing= please_approximate_the_timing_of_prophylactic_antibiotics_administration,
    vte_prophy       = was_pharmacological_venous_thromboembolism_prophylaxis_prescribed,
    vte_timing       = if_yes_vte_prophylaxis_was_provided,
    preop_fasting    = preoperative_fasting,
    senior_surgeon   = what_was_the_level_of_the_most_senior_surgeon_who_participated_in_procedure,
    senior_anaes     = what_was_the_level_of_the_most_senior_anaesthesia_provider_for_this_procedure_in_the_or,
    field_prep       = was_the_surgical_field_preparation_done_according_to_local_standards,
    anaes_type       = anaesthesia_type_used_select_all_that_apply,
    blood_loss_ml    = estimated_blood_loss_in_mls,
    surg_duration_min= length_of_surgery_in_minutes,
    minimal_invasive = were_minimal_invasive_procedures_e_g_laparoscopy_used_for_this_operation,
    wound_class      = what_was_the_class_of_the_wound,
    intraop_oximeter = was_there_a_continuous_intraoperative_monitoring_using_pulse_oximeter,
    intraop_ecg      = was_a_continuous_intraoperative_monitoring_using_ecg,
    intraop_capnography = was_there_continuous_use_of_carbon_dioxide_monitoring_using_capnography,
    who_checklist    = was_the_who_surgical_safety_checklist_completed,
    intraop_abx      = was_intraoperative_antibiotics_provided,
    intraop_transfusion = was_the_patient_given_intra_operative_blood_transfusion,
    eras_protocol    = was_enhanced_recovery_after_surgery_eras_protocol_for_this_patient_care,
    transfusion_units= if_blood_transfusion_was_given_how_many_units,
    disposition      = what_was_the_patient_s_disposition,
    return_30d_raw   = did_the_patient_require_re_operation_or_return_to_theatre_within_30_days_of_index_operation,
    outcome_30d_raw  = what_was_the_outcome_of_the_patient_at_30_days,
    date_discharge   = date_of_discharge_dd_mm_yyyy,
    poms_pulmonary   = on_discharge_from_hospital_complete_the_following_checklist_on_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_multiple_answers_possible_for_adapted_postoperative_morbidity_survey_choice_pulmonary_has_the_patient_developed_a_new_requirement_for_oxygen_or_respiratory_support_during_postoperative_admission,
    poms_infectious  = on_discharge_from_hospital_complete_the_following_checklist_on_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_multiple_answers_possible_for_adapted_postoperative_morbidity_survey_choice_did_the_patient_receive_antibiotics_and_or_had_a_temperature_of_38_degrees_c_during_postoperative_admission,
    poms_renal       = on_discharge_from_hospital_complete_the_following_checklist_on_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_multiple_answers_possible_for_adapted_postoperative_morbidity_survey_choice_renal_presence_of_oliguria_500_ml_24_h_or_0_5_ml_kg_hr_increased_serum_creatinine_30_from_preoperative_level,
    poms_gi          = on_discharge_from_hospital_complete_the_following_checklist_on_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_multiple_answers_possible_for_adapted_postoperative_morbidity_survey_choice_gastro_intestinal_unable_to_tolerate_an_enteral_diet_for_any_reason_including_nausea_vomiting_and_abdominal_distension,
    poms_cardiovascular = on_discharge_from_hospital_complete_the_following_checklist_on_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_multiple_answers_possible_for_adapted_postoperative_morbidity_survey_choice_cardiovascular_diagnostic_tests_or_therapy_for_any_of_the_following_new_myocardial_infarction_or_ischaemia_hypotension_requiring_fluid_therapy_200_ml_hr_or_pharmacological_therapy_atrial_or_ventricular_arrhythmias_cardiogenic_pulmonary_oedema_thrombotic_event_requiring_anticoagulation,
    poms_neurological = on_discharge_from_hospital_complete_the_following_checklist_on_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_multiple_answers_possible_for_adapted_postoperative_morbidity_survey_choice_new_focal_neurological_deficit_confusion_delirium_or_coma,
    poms_haematological = on_discharge_from_hospital_complete_the_following_checklist_on_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_multiple_answers_possible_for_adapted_postoperative_morbidity_survey_choice_hematological_requirement_for_any_of_the_following_packed_erythrocytes_platelets_fresh_frozen_plasma_or_cryoprecipitate,
    poms_wound       = on_discharge_from_hospital_complete_the_following_checklist_on_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_multiple_answers_possible_for_adapted_postoperative_morbidity_survey_choice_wound_wound_dehiscence_requiring_surgical_exploration_or_drainage_of_pus_from_the_operation_wound_with_or_without_isolation_of_organisms,
    poms_pain        = on_discharge_from_hospital_complete_the_following_checklist_on_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_multiple_answers_possible_for_adapted_postoperative_morbidity_survey_choice_pain_new_postoperative_pain_significant_enough_to_require_parenteral_opioids_or_regional_analgesia,
    poms_none        = on_discharge_from_hospital_complete_the_following_checklist_on_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_multiple_answers_possible_for_adapted_postoperative_morbidity_survey_choice_no_complication_developed,
    clavien_raw      = post_operative_complications_claiven_dindo_classification,
    complication_grade = complication_severity_grading,
    ssi              = did_the_patient_develop_post_operative_surgical_site_infection,
    ssi_type         = if_yes_which_type,
    cause_death      = cause_of_death
  )

# Variables NOT in pilot
d <- d |> mutate(
  payment_mode=NA_character_, payment_cat=NA_character_,
  facilities_prior=NA_real_, ngt_required=NA_character_,
  ngt_required_f=NA_character_, ngt_days=NA_real_,
  return_theatre=NA_character_, return_theatre_yn=NA_integer_,
  alive_inhospital=NA_character_, died_inhospital=NA_character_,
  death_inhospital=NA_integer_, date_death=as.Date(NA),
  alive_30d=NA_character_, cause_death_30d=NA_character_,
  ssi_type_cat=NA_character_, data_source="Pilot"
)

d <- d |> mutate(
  site = str_extract(as.character(study_id), "^[0-9]+") |>
    factor(levels=c("7","8","9","11"))
)

d <- d |> mutate(across(c(dob,date_admission,date_surgery,date_discharge),
                         \(x) suppressWarnings(ymd(x))))
d <- d |> mutate(age_years = map_dbl(age_raw, parse_age_years))
d <- d |> mutate(
  weight_kg = suppressWarnings(as.numeric(weight_kg)),
  height_m  = suppressWarnings(as.numeric(height_m)),
  hb_gdl    = suppressWarnings(as.numeric(hb_gdl)),
  hb_gdl    = if_else(hb_gdl > 25, NA_real_, hb_gdl),
  blood_loss_ml     = suppressWarnings(as.numeric(blood_loss_ml)),
  surg_duration_min = suppressWarnings(as.numeric(surg_duration_min)),
  transfusion_units = suppressWarnings(as.numeric(transfusion_units)),
  los_days = as.numeric(date_discharge - date_admission),
  los_days = if_else(!is.na(los_days) & (los_days < 0 | los_days > 180), NA_real_, los_days)
)

d <- d |> mutate(across(c(cm_cad,cm_chf,cm_dm,cm_cancer,cm_htn,cm_stroke,cm_copd,
                           cm_hiv,cm_renal,cm_asthma,cm_blood,cm_cirrhosis,cm_malnutrition,
                           cm_none,cm_other,poms_pulmonary,poms_infectious,poms_renal,poms_gi,
                           poms_cardiovascular,poms_neurological,poms_haematological,
                           poms_wound,poms_pain,poms_none), chk))

d <- d |> mutate(
  any_comorbidity = if_else(
    cm_cad==1|cm_chf==1|cm_dm==1|cm_cancer==1|cm_htn==1|cm_stroke==1|cm_copd==1|
      cm_hiv==1|cm_renal==1|cm_asthma==1|cm_blood==1|cm_cirrhosis==1|cm_malnutrition==1,
    1L, 0L, NA_integer_),
  death_30d = case_when(
    str_to_lower(outcome_30d_raw) == "patient died"         ~ 1L,
    str_to_lower(outcome_30d_raw) == "alive and discharged" ~ 0L,
    TRUE ~ NA_integer_),
  return_30d_yn = case_when(
    str_to_upper(return_30d_raw) == "YES" ~ 1L,
    str_to_upper(return_30d_raw) == "NO"  ~ 0L,
    TRUE ~ NA_integer_),
  bmi = weight_kg / (height_m^2),
  bmi_cat = case_when(
    bmi < 18.5 ~ "Underweight", bmi >= 18.5 & bmi < 25 ~ "Normal weight",
    bmi >= 25 & bmi < 30 ~ "Overweight", bmi >= 30 ~ "Obese", TRUE ~ NA_character_
  ) |> factor(levels=c("Underweight","Normal weight","Overweight","Obese")),
  urgency_cat = case_when(
    str_detect(str_to_lower(urgency),"elective")  ~ "Elective",
    str_detect(str_to_lower(urgency),"urgent")    ~ "Urgent",
    str_detect(str_to_lower(urgency),"emergency") ~ "Emergency",
    TRUE ~ NA_character_
  ) |> factor(levels=c("Elective","Urgent","Emergency")),
  asa_cat = case_when(
    str_detect(asa,"ASA I ")   ~ "ASA I",  str_detect(asa,"ASA II ")  ~ "ASA II",
    str_detect(asa,"ASA III ") ~ "ASA III",str_detect(asa,"ASA IV ")  ~ "ASA IV",
    str_detect(asa,"ASA V ")   ~ "ASA V",  TRUE ~ NA_character_
  ) |> factor(levels=c("ASA I","ASA II","ASA III","ASA IV","ASA V"), ordered=TRUE),
  procedure_group = case_when(
    age_group_redcap == "Adult patient" ~ case_when(
      str_detect(str_to_lower(surg_type_adult),"gastrointestinal|hepatobiliary") ~ "GI & hepatobiliary",
      str_detect(str_to_lower(surg_type_adult),"cardiothoracic|vascular")        ~ "Cardiothoracic & vascular",
      str_detect(str_to_lower(surg_type_adult),"neurosurgery")                   ~ "Neurosurgery",
      str_detect(str_to_lower(surg_type_adult),"oncolog|cancer")                 ~ "Oncologic/cancer",
      str_detect(str_to_lower(surg_type_adult),"orthopaed|trauma")              ~ "Orthopaedic & trauma",
      str_detect(str_to_lower(surg_type_adult),"urology|kidney")                 ~ "Urology & kidney",
      !is.na(surg_type_adult) ~ "Other", TRUE ~ NA_character_),
    age_group_redcap == "Paediatric patient" ~ case_when(
      str_detect(str_to_lower(surg_type_paeds),"congenital") ~ "Congenital anomaly",
      str_detect(str_to_lower(surg_type_paeds),"oncolog")    ~ "Oncological (paeds)",
      str_detect(str_to_lower(surg_type_paeds),"gastro")     ~ "GI & hepatobiliary (paeds)",
      str_detect(str_to_lower(surg_type_paeds),"neuro")      ~ "Neurosurgery",
      str_detect(str_to_lower(surg_type_paeds),"trauma|ortho")~ "Orthopaedic & trauma",
      str_detect(str_to_lower(surg_type_paeds),"urolog")     ~ "Urology & kidney",
      !is.na(surg_type_paeds) ~ "Other", TRUE ~ NA_character_),
    TRUE ~ NA_character_) |> factor(),
  anaes_group = case_when(
    str_detect(str_to_lower(anaes_type),"local")   ~ "Local anaesthesia",
    str_detect(str_to_lower(anaes_type),"regional")~ "Regional anaesthesia",
    str_detect(str_to_lower(anaes_type),"general") ~ "General anaesthesia",
    str_detect(str_to_lower(anaes_type),"monitored|sedation|mac") ~ "MAC / Sedation",
    str_detect(str_to_lower(anaes_type),"walant")  ~ "WALANT",
    !is.na(anaes_type) ~ "Other/unknown", TRUE ~ NA_character_
  ) |> factor(levels=c("General anaesthesia","Regional anaesthesia","Local anaesthesia",
                        "MAC / Sedation","WALANT","Other/unknown")),
  wound_cat = case_when(
    str_detect(str_to_upper(wound_class),"CLASS I\\b")   ~ "Class I: Clean",
    str_detect(str_to_upper(wound_class),"CLASS II\\b")  ~ "Class II: Clean-contaminated",
    str_detect(str_to_upper(wound_class),"CLASS III\\b") ~ "Class III: Contaminated",
    str_detect(str_to_upper(wound_class),"CLASS IV\\b")  ~ "Class IV: Dirty/infected",
    TRUE ~ NA_character_
  ) |> factor(levels=c("Class I: Clean","Class II: Clean-contaminated",
                        "Class III: Contaminated","Class IV: Dirty/infected"), ordered=TRUE),
  wound_binary = case_when(
    wound_cat %in% c("Class I: Clean","Class II: Clean-contaminated") ~ "Clean/clean-contaminated",
    wound_cat %in% c("Class III: Contaminated","Class IV: Dirty/infected") ~ "Contaminated/dirty-infected",
    TRUE ~ NA_character_) |> factor(levels=c("Clean/clean-contaminated","Contaminated/dirty-infected")),
  across(c(who_checklist,intraop_oximeter,intraop_ecg,intraop_capnography,
           eras_protocol,field_prep,prophy_abx,intraop_abx,
           intraop_transfusion,vte_prophy),
         \(x) case_when(
           str_to_upper(x)=="YES" ~ "Yes", str_to_upper(x)=="NO" ~ "No",
           str_to_upper(x) %in% c("NOT APPLICABLE","NOT INDICATED") ~ "Not applicable",
           TRUE ~ NA_character_) |> factor(levels=c("Yes","No","Not applicable")),
         .names="{.col}_f"),
  minimal_invasive_f = case_when(
    str_to_upper(minimal_invasive)=="YES" ~ "Yes",
    str_detect(str_to_lower(minimal_invasive),"converted") ~ "Yes (converted to open)",
    str_to_upper(minimal_invasive)=="NO" ~ "No", TRUE ~ NA_character_
  ) |> factor(levels=c("Yes","Yes (converted to open)","No")),
  surgeon_level = case_when(
    str_detect(str_to_lower(senior_surgeon),"superspecialist|super") ~ "Superspecialist",
    str_detect(str_to_lower(senior_surgeon),"specialist")            ~ "Specialist surgeon",
    str_detect(str_to_lower(senior_surgeon),"fcs|mcs")              ~ "COSECSA fellow",
    str_detect(str_to_lower(senior_surgeon),"mmed|resident|registrar")~ "Resident/registrar",
    TRUE ~ NA_character_
  ) |> factor(levels=c("Superspecialist","Specialist surgeon","COSECSA fellow","Resident/registrar")),
  anaes_level = case_when(
    str_detect(str_to_lower(senior_anaes),"specialist anaesthetist") ~ "Specialist anaesthetist",
    str_detect(str_to_lower(senior_anaes),"non-physician")           ~ "Non-physician anaesthetist",
    str_detect(str_to_lower(senior_anaes),"non-specialist|anaesthesiologists") ~ "Non-specialist physician",
    str_detect(str_to_lower(senior_anaes),"trainee|fellow|resident") ~ "Anaesthesia trainee",
    str_detect(str_to_lower(senior_anaes),"surgeon")                ~ "Surgeon",
    TRUE ~ NA_character_
  ) |> factor(levels=c("Specialist anaesthetist","Non-specialist physician",
                        "Non-physician anaesthetist","Anaesthesia trainee","Surgeon")),
  disposition_cat = case_when(
    str_detect(str_to_lower(disposition),"general ward")   ~ "General ward",
    str_detect(str_to_lower(disposition),"discharge|home") ~ "Discharge home",
    str_detect(str_to_lower(disposition),"hdu")            ~ "HDU",
    str_detect(str_to_lower(disposition),"icu")            ~ "ICU",
    str_detect(str_to_lower(disposition),"death on")       ~ "Death on table",
    TRUE ~ NA_character_
  ) |> factor(levels=c("General ward","HDU","ICU","Discharge home","Death on table")),
  clavien_grade = case_when(
    str_detect(str_to_upper(clavien_raw),"GRADE I\\b")   ~ "I",
    str_detect(str_to_upper(clavien_raw),"GRADE II\\b")  ~ "II",
    str_detect(str_to_upper(clavien_raw),"GRADE IIIA")   ~ "IIIa",
    str_detect(str_to_upper(clavien_raw),"GRADE IIIB")   ~ "IIIb",
    str_detect(str_to_upper(clavien_raw),"GRADE III\\b") ~ "III",
    str_detect(str_to_upper(clavien_raw),"GRADE IVA|GRADE IVB|GRADE IV\\b") ~ "IV",
    str_detect(str_to_upper(clavien_raw),"GRADE V")      ~ "V",
    TRUE ~ NA_character_) |> factor(levels=c("I","II","III","IIIa","IIIb","IV","V"), ordered=TRUE),
  clavien_group = case_when(
    clavien_grade %in% c("I","II")                 ~ "Minor (I-II)",
    clavien_grade %in% c("III","IIIa","IIIb","IV") ~ "Major (III-IV)",
    clavien_grade == "V"                            ~ "Death (V)",
    TRUE ~ NA_character_) |> factor(levels=c("Minor (I-II)","Major (III-IV)","Death (V)")),
  any_complication = if_else(!is.na(clavien_grade), 1L, 0L),
  complication_severity = case_when(
    str_detect(str_to_upper(complication_grade),"MINOR") ~ "Minor",
    str_detect(str_to_upper(complication_grade),"MAJOR") ~ "Major",
    TRUE ~ NA_character_) |> factor(levels=c("Minor","Major")),
  ssi_yn = case_when(
    str_to_upper(ssi)=="YES" ~ 1L, str_to_upper(ssi)=="NO" ~ 0L, TRUE ~ NA_integer_),
  ssi_type_cat = case_when(
    str_detect(str_to_lower(ssi_type),"superficial") ~ "Superficial SSI",
    str_detect(str_to_lower(ssi_type),"deep")        ~ "Deep SSI",
    str_detect(str_to_lower(ssi_type),"organ|space") ~ "Organ/space SSI",
    !is.na(ssi_type) ~ "Other SSI", TRUE ~ NA_character_) |> factor(),
  fasting_cat = case_when(
    str_detect(str_to_lower(preop_fasting),"adequate")  ~ "Adequate",
    str_detect(str_to_lower(preop_fasting),"prolonged") ~ "Prolonged",
    str_detect(str_to_lower(preop_fasting),"no fasting") ~ "No fasting",
    TRUE ~ NA_character_) |> factor(levels=c("Adequate","Prolonged","No fasting")),
  cause_death_cat = case_when(
    str_detect(str_to_lower(as.character(cause_death)),"multiorgan|multi.organ") ~ "Multiorgan failure",
    str_detect(str_to_lower(as.character(cause_death)),"sepsis")  ~ "Sepsis",
    str_detect(str_to_lower(as.character(cause_death)),"cardiac") ~ "Cardiac failure",
    str_detect(str_to_lower(as.character(cause_death)),"other")   ~ "Other",
    TRUE ~ NA_character_) |> factor(),
  los_cat = case_when(
    los_days <= 3 ~ "<=3 days", los_days <= 7 ~ "4-7 days",
    los_days <= 14 ~ "8-14 days", los_days > 14 ~ ">14 days",
    TRUE ~ NA_character_) |> factor(levels=c("<=3 days","4-7 days","8-14 days",">14 days")),
  poms_any = if_else(
    poms_pulmonary==1|poms_infectious==1|poms_renal==1|poms_gi==1|
      poms_cardiovascular==1|poms_neurological==1|poms_haematological==1|
      poms_wound==1|poms_pain==1, 1L, 0L),
  age_group = case_when(
    age_group_redcap=="Adult patient" ~ case_when(
      age_years>=18 & age_years<=44 ~ "Young adulthood (18-44)",
      age_years>=45 & age_years<=64 ~ "Middle adulthood (45-64)",
      age_years>=65 ~ "Older adulthood (65+)", TRUE ~ NA_character_),
    age_group_redcap=="Paediatric patient" ~ case_when(
      age_years < (29/365) ~ "Neonate (0-28 days)",
      age_years < 2        ~ "Infant (29 days-<2 yrs)",
      age_years < 18       ~ "Child/adolescent (2-17 yrs)",
      TRUE ~ NA_character_),
    TRUE ~ NA_character_) |> factor()
)

pilot_adults <- d |> filter(age_group_redcap == "Adult patient")
pilot_paeds  <- d |> filter(age_group_redcap == "Paediatric patient")

cat("Pilot adults:", nrow(pilot_adults), "| Pilot paeds:", nrow(pilot_paeds), "\n")
cat("\nUrgency:\n"); print(table(pilot_adults$urgency_cat, useNA="ifany"))
cat("\nClavien:\n"); print(table(pilot_adults$clavien_grade, useNA="ifany"))
cat("\n30-day deaths:\n"); print(table(pilot_adults$death_30d, useNA="ifany"))
cat("\nProcedure groups:\n"); print(table(pilot_adults$procedure_group, useNA="ifany"))

saveRDS(pilot_adults, file.path(proc_dir, "pilot_adults.rds"))
saveRDS(pilot_paeds,  file.path(proc_dir, "pilot_paeds.rds"))
cat("\n✓ 04a complete\n")
