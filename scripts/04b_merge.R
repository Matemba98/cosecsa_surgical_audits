# =============================================================================
# COSurgCo — Script 04b: Merge Pilot + Implementation Datasets
# Run AFTER 04a_clean_pilot.R and 02_recode.R
# Input:  data/processed/pilot_adults.rds, pilot_paeds.rds
#         data/processed/d_adults.rds, d_paeds.rds
# Output: data/processed/combined_adults.rds (n=1,241)
#         data/processed/combined_paeds.rds  (n=272)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr)
})

root_dir <- file.path("/Users","matemba","Library","CloudStorage",
                       "OneDrive-Personal","cosecsa_surgical_audits")
proc_dir <- file.path(root_dir, "data", "processed")

pilot_adults <- readRDS(file.path(proc_dir, "pilot_adults.rds"))
pilot_paeds  <- readRDS(file.path(proc_dir, "pilot_paeds.rds"))
impl_adults  <- readRDS(file.path(proc_dir, "d_adults.rds"))
impl_paeds   <- readRDS(file.path(proc_dir, "d_paeds.rds"))
impl_adults$data_source <- "Implementation"
impl_paeds$data_source  <- "Implementation"

shared_cols <- c(
  "study_id","site","data_source",
  "age_years","age_group","age_group_redcap","sex",
  "weight_kg","height_m","bmi","bmi_cat","hb_gdl","facilities_prior",
  "los_days","los_cat","payment_mode","payment_cat",
  "any_comorbidity","cm_cad","cm_chf","cm_dm","cm_cancer","cm_htn","cm_stroke",
  "cm_copd","cm_hiv","cm_renal","cm_asthma","cm_blood","cm_cirrhosis",
  "cm_malnutrition","cm_none","cm_other",
  "procedure_group","surg_type_adult","surg_type_paeds",
  "urgency_cat","asa_cat","anaes_group","anaes_level","surgeon_level",
  "wound_cat","wound_binary","blood_loss_ml","surg_duration_min",
  "fasting_cat","disposition_cat",
  "prophy_abx_f","intraop_abx_f","vte_prophy_f","who_checklist_f",
  "intraop_oximeter_f","intraop_ecg_f","intraop_capnography_f","eras_protocol_f",
  "minimal_invasive_f","intraop_transfusion_f","field_prep_f","ngt_required_f",
  "clavien_grade","clavien_group","any_complication","complication_severity",
  "ssi_yn","ssi_type_cat","return_theatre_yn","return_30d_yn",
  "death_inhospital","death_30d","cause_death_cat",
  "poms_any","poms_pulmonary","poms_infectious","poms_renal","poms_gi",
  "poms_cardiovascular","poms_neurological","poms_haematological",
  "poms_wound","poms_pain","poms_none"
)

safe_select <- function(d, cols) {
  missing <- setdiff(cols, names(d))
  for (col in missing) d[[col]] <- NA
  d[, cols]
}

int_cols <- c("any_complication","death_inhospital","death_30d","ssi_yn",
              "return_theatre_yn","return_30d_yn","any_comorbidity",
              "cm_cad","cm_chf","cm_dm","cm_cancer","cm_htn","cm_stroke","cm_copd",
              "cm_hiv","cm_renal","cm_asthma","cm_blood","cm_cirrhosis",
              "cm_malnutrition","cm_none","cm_other",
              "poms_any","poms_pulmonary","poms_infectious","poms_renal","poms_gi",
              "poms_cardiovascular","poms_neurological","poms_haematological",
              "poms_wound","poms_pain","poms_none")

num_cols <- c("age_years","weight_kg","height_m","bmi","hb_gdl","facilities_prior",
              "los_days","blood_loss_ml","surg_duration_min")

harmonise <- function(d) {
  # Convert all factors to character first
  d <- d |> mutate(across(where(is.factor), as.character))
  # Integer columns
  for (col in intersect(int_cols, names(d))) {
    d[[col]] <- suppressWarnings(as.integer(d[[col]]))
  }
  # Numeric columns
  for (col in intersect(num_cols, names(d))) {
    d[[col]] <- suppressWarnings(as.numeric(d[[col]]))
  }
  # Refactor
  d |> mutate(
    urgency_cat   = factor(urgency_cat, levels=c("Elective","Urgent","Emergency")),
    asa_cat       = factor(asa_cat, levels=c("ASA I","ASA II","ASA III","ASA IV","ASA V"), ordered=TRUE),
    bmi_cat       = factor(bmi_cat, levels=c("Underweight","Normal weight","Overweight","Obese")),
    los_cat       = factor(los_cat, levels=c("<=3 days","4-7 days","8-14 days",">14 days")),
    clavien_grade = factor(clavien_grade, levels=c("I","II","III","IIIa","IIIb","IV","V"), ordered=TRUE),
    clavien_group = factor(clavien_group, levels=c("Minor (I-II)","Major (III-IV)","Death (V)")),
    procedure_group = factor(procedure_group),
    anaes_group   = factor(anaes_group, levels=c("General anaesthesia","Regional anaesthesia",
                            "Local anaesthesia","MAC / Sedation","WALANT","Other/unknown")),
    wound_cat     = factor(wound_cat, levels=c("Class I: Clean","Class II: Clean-contaminated",
                            "Class III: Contaminated","Class IV: Dirty/infected"), ordered=TRUE),
    wound_binary  = factor(wound_binary, levels=c("Clean/clean-contaminated","Contaminated/dirty-infected")),
    across(ends_with("_f"), ~factor(.x, levels=c("Yes","No","Not applicable"))),
    surgeon_level = factor(surgeon_level, levels=c("Superspecialist","Specialist surgeon",
                            "COSECSA fellow","Resident/registrar")),
    anaes_level   = factor(anaes_level, levels=c("Specialist anaesthetist","Non-specialist physician",
                            "Non-physician anaesthetist","Anaesthesia trainee","Surgeon")),
    disposition_cat = factor(disposition_cat, levels=c("General ward","HDU","ICU",
                              "Discharge home","Death on table")),
    fasting_cat   = factor(fasting_cat, levels=c("Adequate","Prolonged","No fasting")),
    complication_severity = factor(complication_severity, levels=c("Minor","Major")),
    site = factor(as.character(site))
  )
}

combined_adults <- bind_rows(
  safe_select(pilot_adults, shared_cols) |> harmonise(),
  safe_select(impl_adults,  shared_cols) |> harmonise()
)
combined_paeds <- bind_rows(
  safe_select(pilot_paeds, shared_cols) |> harmonise(),
  safe_select(impl_paeds,  shared_cols) |> harmonise()
)

cat("=== COMBINED ADULTS:", nrow(combined_adults), "===\n")
print(table(combined_adults$data_source))
cat("\nUrgency:\n"); print(table(combined_adults$urgency_cat, useNA="ifany"))
cat("\nProcedure groups:\n"); print(table(combined_adults$procedure_group, useNA="ifany"))
cat("\n30-day deaths:\n"); print(table(combined_adults$death_30d, useNA="ifany"))
cat("\nIn-hospital deaths:\n"); print(table(combined_adults$death_inhospital, useNA="ifany"))

cat("\n=== COMBINED PAEDS:", nrow(combined_paeds), "===\n")
print(table(combined_paeds$data_source))
cat("\nAge group:\n"); print(table(combined_paeds$age_group, useNA="ifany"))
cat("\nProcedure group:\n"); print(table(combined_paeds$procedure_group, useNA="ifany"))

saveRDS(combined_adults, file.path(proc_dir, "combined_adults.rds"))
saveRDS(combined_paeds,  file.path(proc_dir, "combined_paeds.rds"))
cat("\n✓ 04b complete\n")
