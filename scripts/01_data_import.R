# =============================================================================
# COSurgCo Surgical Audit — Phase 1: Data Import & Cleaning
# Script: 01_data_import.R
#
# OUTPUT: data/processed/d_adults_raw.rds
#         data/processed/d_paeds_raw.rds
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(lubridate)
  library(stringr); library(purrr); library(tidyr)
})

# ── PATHS ────────────────────────
root_dir <- file.path(
  "/Users", "matemba", "Library", "CloudStorage", "OneDrive-Personal", "cosecsa_surgical_audits"
)
raw_csv  <- file.path(root_dir, "data", "raw",
  "SurgicalAuditInECSAR-ImplementationPhaseA_DATA_LABELS_2026-04-18_1157.csv")
proc_dir <- file.path(root_dir, "data", "processed")
dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)

# ── HELPER: clean column names ────────────────────────────────────────────────
# (replaces janitor::clean_names() — no extra package needed)
clean_names_simple <- function(df) {
  names(df) <- names(df) |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("_+", "_") |>
    str_replace_all("^_|_$", "")
  df
}

# ── READ ──────────────────────────────────────────────────────────────────────
raw <- read_csv(raw_csv, show_col_types = FALSE, name_repair = "unique") |>
  clean_names_simple()
cat("Raw data:", nrow(raw), "rows x", ncol(raw), "columns\n")

# ── RENAME ────────────────────────────────────────────────────────────────────
# NOTE: Two columns share the same cleaned name 'if_other_please_specify'
#       (col 18 = payment other; col 107 = 30-day cause of death other specify).
#       We select them by column position to avoid the duplicate-name error.
d <- raw |>
  rename(
    study_id             = study_id_1,
    date_entry           = date,
    mrn                  = medical_record_number,
    residency            = patient_s_residency,
    dob                  = date_of_birth_of_the_patient,
    date_admission       = date_on_admission,
    age_raw              = age_at_time_of_surgery_months_years,
    sex                  = gender_of_the_patient,
    weight_done          = patient_s_weight,
    weight_kg            = patient_s_weight_kg,
    height_done          = patient_s_height,
    height_m             = patient_s_height_in_meters,
    hb_gdl               = hb_level_g_dl_immediate_before_surgery,
    facilities_prior     = number_of_facilities_visited_prior_the_current_where_surgery_is_taking_place,
    date_surgery         = date_of_surgery,
    payment_mode         = mode_of_surgical_payment,
    payment_other        = 18,   # "If other, please specify" (payment)
    age_group_redcap     = age_group_of_the_patient,
    surg_type_adult      = type_of_surgery_adults,
    surg_name_adult      = type_of_surgery_performed_adult,
    surg_unlisted_adult  = if_the_type_of_procedure_is_not_listed_please_add_here,
    surg_type_paeds      = types_of_surgery_paediatrics,
    surg_name_paeds      = type_of_surgery_performed_paediatrics,
    surg_unlisted_paeds  = if_the_type_of_pediatric_surgery_is_not_listed_please_add_here,
    cm_cad         = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_coronary_artery_disease,
    cm_chf         = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_congestive_heart_failure,
    cm_dm          = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_diabetes_mellitus,
    cm_cancer      = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_metastatic_cancer,
    cm_htn         = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_hypertension,
    cm_stroke      = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_stroke,
    cm_copd        = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_chronic_obstructive_pulmonary_disease,
    cm_hiv         = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_hiv,
    cm_renal       = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_chronic_renal_disease,
    cm_asthma      = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_asthma,
    cm_blood       = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_blood_related_conditions_e_g_sickle_cell_anaemia_haemophilia,
    cm_cirrhosis   = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_cirrhosis,
    cm_malnutrition= did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_malnutrition,
    cm_none        = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_none_of_the_above,
    cm_other       = did_the_patient_have_any_of_these_preoperative_chronic_comorbid_conditions_check_all_that_apply_choice_other_specify,
    urgency              = urgency_of_operation,
    asa                  = asa_physical_status_score,
    prophy_abx           = was_prophylactic_antibiotics_given_pre_operative,
    prophy_abx_timing    = please_approximate_the_timing_of_prophylactic_antibiotics_administration,
    vte_prophy           = was_pharmacological_venous_thromboembolism_prophylaxis_prescribed,
    vte_timing           = if_yes_vte_prophylaxis_was_provided,
    preop_fasting        = preoperative_fasting,
    senior_surgeon       = what_was_the_level_of_the_most_senior_surgeon_who_participated_in_procedure,
    senior_anaes         = what_was_the_level_of_the_most_senior_anaesthesia_provider_for_this_procedure_in_the_or,
    field_prep           = was_the_surgical_field_preparation_done_according_to_local_standards,
    anaes_type           = anaesthesia_type_used_select_all_that_apply,
    blood_loss_ml        = estimated_blood_loss_in_mls,
    surg_duration_min    = length_of_surgery_in_minutes,
    minimal_invasive     = were_minimal_invasive_procedures_e_g_laparoscopy_used_for_this_operation,
    wound_class          = what_was_the_class_of_the_wound,
    intraop_oximeter     = was_there_a_continuous_intraoperative_monitoring_using_pulse_oximeter,
    intraop_ecg          = was_a_continuous_intraoperative_monitoring_using_ecg,
    intraop_capnography  = was_there_continuous_use_of_carbon_dioxide_monitoring_using_capnography,
    who_checklist        = was_the_who_surgical_safety_checklist_completed,
    intraop_abx          = was_intraoperative_antibiotics_provided,
    intraop_transfusion  = was_the_patient_given_intra_operative_blood_transfusion,
    eras_protocol        = was_enhanced_recovery_after_surgery_eras_protocol_for_this_patient_care,
    transfusion_units    = if_blood_transfusion_was_given_how_many_units,
    indication_surg      = indication_for_the_surgical_procedure,
    ind_trauma           = trauma_injuries_and_orthopedic,
    ind_abdominal        = acute_abdominal_conditions,
    ind_cancer           = cancers_and_tumors,
    ind_urology          = urological_conditions,
    ind_thoracic         = thoracic_conditions,
    ind_neuro            = neurosurgery,
    ind_neonatal         = neonatal_and_infant_conditions,
    ind_specify          = specify,
    disposition          = what_was_the_patient_s_disposition_after_surgery,
    ngt_required         = did_the_patient_require_nasogastric_tube_decompression,
    ngt_days             = if_yes_for_how_long_in_days,
    clavien_raw          = post_operative_complications_claiven_dindo_classification,
    complication_grade   = complication_severity_grading,
    ssi                  = did_the_patient_develop_post_operative_surgical_site_infection,
    ssi_type             = if_yes_which_type_of_ssi,
    return_theatre       = did_the_patient_require_re_operation_or_return_to_theatre_from_the_index_operation,
    return_theatre_date  = when_was_the_patient_re_operated,
    alive_inhospital     = in_hospital_survival_choice_alive_and_discharged,
    died_inhospital      = in_hospital_survival_choice_patient_died,
    date_death           = date_of_death_dd_mm_yyyy,
    date_discharge       = date_of_discharge_dd_mm_yyyy,
    cause_death          = cause_of_death_90,
    cause_death_other    = specify_other_cause_of_death,
    poms_pulmonary       = postoperative_morbidity_survey_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_choice_pulmonary_has_the_patient_developed_a_new_requirement_for_oxygen_or_respiratory_support_during_postoperative_admission,
    poms_infectious      = postoperative_morbidity_survey_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_choice_did_the_patient_receive_antibiotics_and_or_had_a_temperature_of_38_degrees_c_during_postoperative_admission,
    poms_renal           = postoperative_morbidity_survey_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_choice_renal_presence_of_oliguria_500_ml_24_h_or_0_5_ml_kg_hr_increased_serum_creatinine_30_from_preoperative_level,
    poms_gi              = postoperative_morbidity_survey_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_choice_gastro_intestinal_unable_to_tolerate_an_enteral_diet_for_any_reason_including_nausea_vomiting_and_abdominal_distension,
    poms_cardiovascular  = postoperative_morbidity_survey_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_choice_cardiovascular_diagnostic_tests_or_therapy_for_any_of_the_following_new_myocardial_infarction_or_ischaemia_hypotension_requiring_fluid_therapy_200_ml_hr_or_pharmacological_therapy_atrial_or_ventricular_arrhythmias_cardiogenic_pulmonary_oedema_thrombotic_event_requiring_anticoagulation,
    poms_neurological    = postoperative_morbidity_survey_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_choice_new_focal_neurological_deficit_confusion_delirium_or_coma,
    poms_haematological  = postoperative_morbidity_survey_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_choice_hematological_requirement_for_any_of_the_following_packed_erythrocytes_platelets_fresh_frozen_plasma_or_cryoprecipitate,
    poms_wound           = postoperative_morbidity_survey_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_choice_wound_wound_dehiscence_requiring_surgical_exploration_or_drainage_of_pus_from_the_operation_wound_with_or_without_isolation_of_organisms,
    poms_pain            = postoperative_morbidity_survey_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_choice_pain_new_postoperative_pain_significant_enough_to_require_parenteral_opioids_or_regional_analgesia,
    poms_none            = postoperative_morbidity_survey_morbidity_developed_during_any_24_h_period_of_postoperative_hospital_admission_choice_no_complication_developed,
    alive_30d            = was_the_patient_alive_at_30_days,
    cause_death_30d      = cause_of_death_106,
    cause_death_30d_spec = 107,  # "If Other, please specify:" (30-day instrument)
    return_30d           = did_the_patient_require_re_operation_or_return_to_theatre_within_30_days_of_index_operation,
    return_30d_date      = date_of_re_operation,
    selfcare_30d         = self_care_activities_bathing_showering_dressing_using_the_toilet_managing_hygiene,
    mobility_30d         = mobility_activities_getting_in_and_out_of_bed_walking_inside_outside_home_using_stairs,
    household_30d        = household_management_activities_preparing_meals_cleaning_laundry_shopping,
    social_30d           = social_and_leisure_activities_participating_in_social_activities_leisure_activities
  )

# ── SITE ──────────────────────────────────────────────────────────────────────
d <- d |>
  mutate(site = str_extract(study_id, "^[0-9]+") |>
           factor(levels = c("13","14","18","19","20","21","22")))

# ── DATES ─────────────────────────────────────────────────────────────────────
d <- d |>
  mutate(across(c(dob, date_admission, date_surgery, date_discharge, date_death),
                \(x) suppressWarnings(ymd(x))))

# ── AGE (free-text → decimal years) ──────────────────────────────────────────
parse_age_years <- function(x) {
  x <- str_squish(tolower(as.character(x)))
  m <- str_match(x, "([0-9.]+)\\s*(month|months)")
  if (!is.na(m[1,1])) return(as.numeric(m[1,2]) / 12)
  as.numeric(str_extract(x, "[0-9]+\\.?[0-9]*"))
}
d <- d |> mutate(age_years = map_dbl(age_raw, parse_age_years))

# ── NUMERIC FIELDS ────────────────────────────────────────────────────────────
d <- d |>
  mutate(
    weight_kg         = suppressWarnings(as.numeric(weight_kg)),
    height_m          = suppressWarnings(as.numeric(height_m)),
    hb_gdl            = suppressWarnings(as.numeric(hb_gdl)),
    hb_gdl            = if_else(hb_gdl > 25, NA_real_, hb_gdl),  # flag outlier
    blood_loss_ml     = suppressWarnings(as.numeric(blood_loss_ml)),
    surg_duration_min = suppressWarnings(as.numeric(surg_duration_min)),
    ngt_days          = suppressWarnings(as.numeric(ngt_days)),
    transfusion_units = suppressWarnings(as.numeric(transfusion_units)),
    facilities_prior  = case_when(
      str_to_lower(facilities_prior) %in% c("none","non","no","0") ~ 0,
      str_to_lower(facilities_prior) == "one"   ~ 1,
      str_to_lower(facilities_prior) == "two"   ~ 2,
      str_to_lower(facilities_prior) == "three" ~ 3,
      TRUE ~ suppressWarnings(as.numeric(facilities_prior))
    )
  )

# ── LENGTH OF STAY ────────────────────────────────────────────────────────────
d <- d |>
  mutate(
    effective_discharge = coalesce(date_discharge, date_death),
    los_days = as.numeric(effective_discharge - date_admission)
  )

# ── CHECKED/UNCHECKED → 0/1 ───────────────────────────────────────────────────
chk <- function(x) if_else(str_to_lower(x) == "checked", 1L, 0L, NA_integer_)
d <- d |>
  mutate(across(c(cm_cad, cm_chf, cm_dm, cm_cancer, cm_htn, cm_stroke,
                  cm_copd, cm_hiv, cm_renal, cm_asthma, cm_blood,
                  cm_cirrhosis, cm_malnutrition, cm_none, cm_other,
                  poms_pulmonary, poms_infectious, poms_renal, poms_gi,
                  poms_cardiovascular, poms_neurological, poms_haematological,
                  poms_wound, poms_pain, poms_none), chk))

d <- d |>
  mutate(any_comorbidity = if_else(
    cm_cad==1|cm_chf==1|cm_dm==1|cm_cancer==1|cm_htn==1|cm_stroke==1|
      cm_copd==1|cm_hiv==1|cm_renal==1|cm_asthma==1|cm_blood==1|
      cm_cirrhosis==1|cm_malnutrition==1, 1L, 0L, NA_integer_))

# ── IN-HOSPITAL MORTALITY ─────────────────────────────────────────────────────
d <- d |>
  mutate(death_inhospital = case_when(
    str_to_lower(died_inhospital)  == "checked" ~ 1L,
    str_to_lower(alive_inhospital) == "checked" ~ 0L,
    TRUE ~ NA_integer_))

# ── SPLIT ─────────────────────────────────────────────────────────────────────
d_adults <- d |> filter(age_group_redcap == "Adult patient")
d_paeds  <- d |> filter(age_group_redcap == "Paediatric patient")

cat("Adults:", nrow(d_adults), "| Paediatrics:", nrow(d_paeds), "\n")

# ── SAVE ──────────────────────────────────────────────────────────────────────
saveRDS(d_adults, file.path(proc_dir, "d_adults_raw.rds"))
saveRDS(d_paeds,  file.path(proc_dir, "d_paeds_raw.rds"))
write_csv(d_adults, file.path(proc_dir, "d_adults_raw.csv"))
write_csv(d_paeds,  file.path(proc_dir, "d_paeds_raw.csv"))
cat("✓ Phase 1 complete. Saved to", proc_dir, "\n")
