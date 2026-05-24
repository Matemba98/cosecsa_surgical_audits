# =============================================================================
# COSurgCo Surgical Audit — Phase 2: Variable Recoding
# Script: 02_recode.R
# INPUT:  data/processed/d_adults_raw.rds / d_paeds_raw.rds
# OUTPUT: data/processed/d_adults.rds / d_paeds.rds
# =============================================================================

# ── PACKAGES ────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(lubridate)
  library(stringr); library(purrr); library(tidyr)
})

# ── PATHS ────────────────────────
root_dir <- file.path(
  "/Users", "matemba", "Library", "CloudStorage", "OneDrive-Personal", "cosecsa_surgical_audits"
)
proc_dir <- file.path(root_dir, "data", "processed")

d_adults <- readRDS(file.path(proc_dir, "d_adults_raw.rds"))
d_paeds  <- readRDS(file.path(proc_dir, "d_paeds_raw.rds"))

cat("Adults:", nrow(d_adults), "| Paeds:", nrow(d_paeds), "\n")

# ── RECODE FUNCTION ──────────────────────────────────────────────────────────
recode_shared <- function(d, cohort) {
  d |>
    mutate(
      # BMI
      bmi = weight_kg / (height_m^2),
      bmi_cat = case_when(
        bmi <  18.5              ~ "Underweight",
        bmi >= 18.5 & bmi < 25  ~ "Normal weight",
        bmi >= 25.0 & bmi < 30  ~ "Overweight",
        bmi >= 30                ~ "Obese",
        TRUE                     ~ NA_character_
      ) |> factor(levels = c("Underweight","Normal weight","Overweight","Obese")),

      # Procedure group
      procedure_group = (if (cohort == "adult") surg_type_adult else surg_type_paeds) |>
        str_squish() |>
        recode(
          "Gastrointestinal and hepatobiliary (upper and lower gastrointestinal)" = "GI & hepatobiliary",
          "Cardiothoracic and vascular (cardiac, vascular, thoracic lung, and thoracic gut)" = "Cardiothoracic & vascular",
          "Neurosurgery (procedures involving the brain and cervical spine)" = "Neurosurgery",
          "Oncologic/cancer surgery" = "Oncologic/cancer",
          "Orthopedic and Trauma" = "Orthopaedic & trauma",
          "Urology and kidney" = "Urology & kidney",
          "Congenital anomaly surgery" = "Congenital anomaly",
          "Oncological" = "Oncological (paeds)"
        ) |> factor(),

      # Anaesthesia
      anaes_group = case_when(
        str_detect(str_to_lower(anaes_type), "local")            ~ "Local anaesthesia",
        str_detect(str_to_lower(anaes_type), "regional")         ~ "Regional anaesthesia",
        str_detect(str_to_lower(anaes_type), "general")          ~ "General anaesthesia",
        str_detect(str_to_lower(anaes_type), "monitored|sedation|mac") ~ "MAC / Sedation",
        str_detect(str_to_lower(anaes_type), "walant")           ~ "WALANT",
        !is.na(anaes_type)                                       ~ "Other/unknown",
        TRUE ~ NA_character_
      ) |> factor(levels = c("General anaesthesia","Regional anaesthesia",
                             "Local anaesthesia","MAC / Sedation","WALANT","Other/unknown")),

      # Clavien-Dindo
      clavien_grade = case_when(
        str_detect(str_to_upper(clavien_raw), "GRADE I\\b")   ~ "I",
        str_detect(str_to_upper(clavien_raw), "GRADE II\\b")  ~ "II",
        str_detect(str_to_upper(clavien_raw), "GRADE IIIA")   ~ "IIIa",
        str_detect(str_to_upper(clavien_raw), "GRADE IIIB")   ~ "IIIb",
        str_detect(str_to_upper(clavien_raw), "GRADE III\\b") ~ "III",
        str_detect(str_to_upper(clavien_raw), "GRADE IVA|GRADE IVB|GRADE IV\\b") ~ "IV",
        str_detect(str_to_upper(clavien_raw), "GRADE V")      ~ "V",
        TRUE ~ NA_character_
      ) |> factor(levels = c("I","II","III","IIIa","IIIb","IV","V"), ordered = TRUE),

      clavien_group = case_when(
        clavien_grade %in% c("I","II")                ~ "Minor (I-II)",
        clavien_grade %in% c("III","IIIa","IIIb","IV") ~ "Major (III-IV)",
        clavien_grade == "V"                           ~ "Death (V)",
        TRUE ~ NA_character_
      ) |> factor(levels = c("Minor (I-II)","Major (III-IV)","Death (V)")),

      any_complication = if_else(!is.na(clavien_grade), 1L, 0L),

      # Complication severity
      complication_severity = case_when(
        str_detect(str_to_upper(complication_grade), "MINOR") ~ "Minor",
        str_detect(str_to_upper(complication_grade), "MAJOR") ~ "Major",
        TRUE ~ NA_character_
      ) |> factor(levels = c("Minor","Major")),

      # SSI
      ssi_yn = case_when(
        str_to_upper(ssi) == "YES" ~ 1L,
        str_to_upper(ssi) == "NO"  ~ 0L,
        TRUE ~ NA_integer_),

      ssi_type_cat = case_when(
        str_detect(str_to_lower(ssi_type), "superficial") ~ "Superficial SSI",
        str_detect(str_to_lower(ssi_type), "deep")        ~ "Deep SSI",
        !is.na(ssi_type)                                  ~ "Other SSI",
        TRUE ~ NA_character_) |> factor(),

      # Return to theatre
      return_theatre_yn = case_when(
        str_to_upper(return_theatre) == "YES" ~ 1L,
        str_to_upper(return_theatre) == "NO"  ~ 0L,
        TRUE ~ NA_integer_),

      return_30d_yn = case_when(
        str_to_upper(return_30d) == "YES" ~ 1L,
        str_to_upper(return_30d) == "NO"  ~ 0L,
        TRUE ~ NA_integer_),

      # 30-day mortality
      death_30d = case_when(
        str_to_lower(alive_30d) == "alive"        ~ 0L,
        str_to_lower(alive_30d) == "patient died" ~ 1L,
        TRUE ~ NA_integer_),

      # Payment
      payment_cat = case_when(
        str_detect(str_to_lower(payment_mode), "insurance") ~ "Health insurance",
        str_detect(str_to_lower(payment_mode), "self")      ~ "Self-payment",
        str_detect(str_to_lower(payment_mode), "other")     ~ "Other",
        TRUE ~ NA_character_
      ) |> factor(levels = c("Health insurance","Self-payment","Other")),

      # Urgency
      urgency_cat = case_when(
        str_detect(str_to_lower(urgency), "elective")  ~ "Elective",
        str_detect(str_to_lower(urgency), "urgent")    ~ "Urgent",
        str_detect(str_to_lower(urgency), "emergency") ~ "Emergency",
        TRUE ~ NA_character_
      ) |> factor(levels = c("Elective","Urgent","Emergency")),

      # ASA
      asa_cat = case_when(
        str_detect(asa, "ASA I ")   ~ "ASA I",
        str_detect(asa, "ASA II ")  ~ "ASA II",
        str_detect(asa, "ASA III ") ~ "ASA III",
        str_detect(asa, "ASA IV ")  ~ "ASA IV",
        str_detect(asa, "ASA V ")   ~ "ASA V",
        TRUE ~ NA_character_
      ) |> factor(levels = c("ASA I","ASA II","ASA III","ASA IV","ASA V"), ordered = TRUE),

      # Wound class
      wound_cat = case_when(
        str_detect(str_to_upper(wound_class), "CLASS I\\b")   ~ "Class I: Clean",
        str_detect(str_to_upper(wound_class), "CLASS II\\b")  ~ "Class II: Clean-contaminated",
        str_detect(str_to_upper(wound_class), "CLASS III\\b") ~ "Class III: Contaminated",
        str_detect(str_to_upper(wound_class), "CLASS IV\\b")  ~ "Class IV: Dirty/infected",
        TRUE ~ NA_character_
      ) |> factor(levels = c("Class I: Clean","Class II: Clean-contaminated",
                             "Class III: Contaminated","Class IV: Dirty/infected"),
                  ordered = TRUE),

      wound_binary = case_when(
        wound_cat %in% c("Class I: Clean","Class II: Clean-contaminated") ~
          "Clean/clean-contaminated",
        wound_cat %in% c("Class III: Contaminated","Class IV: Dirty/infected") ~
          "Contaminated/dirty-infected",
        TRUE ~ NA_character_
      ) |> factor(levels = c("Clean/clean-contaminated","Contaminated/dirty-infected")),

      # Intraop checklist — yes/no factors
      across(
        c(who_checklist, intraop_oximeter, intraop_ecg, intraop_capnography,
          eras_protocol, field_prep, prophy_abx, intraop_abx,
          intraop_transfusion, vte_prophy, ngt_required),
        \(x) case_when(
          str_to_upper(x) == "YES"            ~ "Yes",
          str_to_upper(x) == "NO"             ~ "No",
          str_to_upper(x) %in% c("NOT APPLICABLE","NOT INDICATED") ~ "Not applicable",
          TRUE ~ NA_character_
        ) |> factor(levels = c("Yes","No","Not applicable")),
        .names = "{.col}_f"
      ),

      # Minimal invasive
      minimal_invasive_f = case_when(
        str_to_upper(minimal_invasive) == "YES"                   ~ "Yes",
        str_detect(str_to_lower(minimal_invasive), "converted")   ~ "Yes (converted to open)",
        str_to_upper(minimal_invasive) == "NO"                    ~ "No",
        TRUE ~ NA_character_
      ) |> factor(levels = c("Yes","Yes (converted to open)","No")),

      # Senior surgeon level
      surgeon_level = case_when(
        str_detect(str_to_lower(senior_surgeon), "superspecialist|super") ~ "Superspecialist",
        str_detect(str_to_lower(senior_surgeon), "specialist")            ~ "Specialist surgeon",
        str_detect(str_to_lower(senior_surgeon), "fcs|mcs")              ~ "COSECSA fellow",
        str_detect(str_to_lower(senior_surgeon), "mmed|resident|registrar") ~ "Resident/registrar",
        TRUE ~ NA_character_
      ) |> factor(levels = c("Superspecialist","Specialist surgeon",
                             "COSECSA fellow","Resident/registrar")),

      # Senior anaesthesia provider
      anaes_level = case_when(
        str_detect(str_to_lower(senior_anaes), "specialist anaesthetist") ~ "Specialist anaesthetist",
        str_detect(str_to_lower(senior_anaes), "non-physician")           ~ "Non-physician anaesthetist",
        str_detect(str_to_lower(senior_anaes), "non-specialist|anaesthesiologists") ~
          "Non-specialist physician",
        str_detect(str_to_lower(senior_anaes), "trainee|fellow|resident") ~ "Anaesthesia trainee",
        str_detect(str_to_lower(senior_anaes), "surgeon")                ~ "Surgeon",
        TRUE ~ NA_character_
      ) |> factor(levels = c("Specialist anaesthetist","Non-specialist physician",
                             "Non-physician anaesthetist","Anaesthesia trainee","Surgeon")),

      # Disposition
      disposition_cat = case_when(
        str_detect(str_to_lower(disposition), "general ward") ~ "General ward",
        str_detect(str_to_lower(disposition), "discharge|home") ~ "Discharge home",
        str_detect(str_to_lower(disposition), "hdu") ~ "HDU",
        str_detect(str_to_lower(disposition), "icu") ~ "ICU",
        TRUE ~ NA_character_
      ) |> factor(levels = c("General ward","HDU","ICU","Discharge home")),

      # LOS category
      los_cat = case_when(
        los_days <= 3  ~ "<=3 days",
        los_days <= 7  ~ "4-7 days",
        los_days <= 14 ~ "8-14 days",
        los_days >  14 ~ ">14 days",
        TRUE ~ NA_character_
      ) |> factor(levels = c("<=3 days","4-7 days","8-14 days",">14 days")),

      # Preop fasting
      fasting_cat = case_when(
        str_detect(str_to_lower(preop_fasting), "adequate")  ~ "Adequate",
        str_detect(str_to_lower(preop_fasting), "prolonged") ~ "Prolonged",
        str_detect(str_to_lower(preop_fasting), "no fasting") ~ "No fasting",
        TRUE ~ NA_character_
      ) |> factor(levels = c("Adequate","Prolonged","No fasting")),

      # Cause of death
      cause_death_cat = coalesce(cause_death, cause_death_30d) |>
        (\(x) case_when(
          str_detect(str_to_lower(x), "multiorgan|multi.organ") ~ "Multiorgan failure",
          str_detect(str_to_lower(x), "sepsis")                 ~ "Sepsis",
          str_detect(str_to_lower(x), "cardiac")                ~ "Cardiac failure",
          str_detect(str_to_lower(x), "other")                  ~ "Other",
          TRUE ~ NA_character_
        ))() |> factor(),

      # POMS any
      poms_any = if_else(
        poms_pulmonary==1|poms_infectious==1|poms_renal==1|poms_gi==1|
          poms_cardiovascular==1|poms_neurological==1|poms_haematological==1|
          poms_wound==1|poms_pain==1, 1L, 0L)
    )
}

# Age groups
adults <- recode_shared(d_adults, "adult") |>
  mutate(age_group = case_when(
    age_years >= 18 & age_years <= 44 ~ "Young adulthood (18-44)",
    age_years >= 45 & age_years <= 64 ~ "Middle adulthood (45-64)",
    age_years >= 65                   ~ "Older adulthood (65+)",
    TRUE ~ NA_character_
  ) |> factor(levels = c("Young adulthood (18-44)","Middle adulthood (45-64)","Older adulthood (65+)")))

paeds <- recode_shared(d_paeds, "paeds") |>
  mutate(age_group = case_when(
    age_years < (29/365)  ~ "Neonate (0-28 days)",
    age_years < 2         ~ "Infant (29 days-<2 yrs)",
    age_years < 18        ~ "Child/adolescent (2-17 yrs)",
    TRUE ~ NA_character_
  ) |> factor(levels = c("Neonate (0-28 days)","Infant (29 days-<2 yrs)","Child/adolescent (2-17 yrs)")))

# ── REPORT RESULTS ────────────────────────────────────────────────────────────
cat("\n========== ADULTS (n=", nrow(adults), ") ==========\n", sep="")
cat("Age (years): median=", median(adults$age_years, na.rm=TRUE),
    " IQR=", quantile(adults$age_years,.25,na.rm=T),"-",
    quantile(adults$age_years,.75,na.rm=T),"\n")
cat("\nAge groups:\n"); print(table(adults$age_group, useNA="ifany"))
cat("\nSex:\n"); print(table(adults$sex, useNA="ifany"))
cat("\nBMI categories (n with data=",sum(!is.na(adults$bmi)),"):\n")
print(table(adults$bmi_cat, useNA="ifany"))
cat("\nProcedure group:\n"); print(table(adults$procedure_group, useNA="ifany"))
cat("\nUrgency:\n"); print(table(adults$urgency_cat, useNA="ifany"))
cat("\nASA:\n"); print(table(adults$asa_cat, useNA="ifany"))
cat("\nAnaesthesia:\n"); print(table(adults$anaes_group, useNA="ifany"))
cat("\nWound class:\n"); print(table(adults$wound_cat, useNA="ifany"))
cat("\nClavien grade:\n"); print(table(adults$clavien_grade, useNA="ifany"))
cat("\nClavien group:\n"); print(table(adults$clavien_group, useNA="ifany"))
cat("\nAny complication:\n"); print(table(adults$any_complication, useNA="ifany"))
cat("\nSSI:\n"); print(table(adults$ssi_yn, useNA="ifany"))
cat("\nReturn to theatre (in-hosp):\n"); print(table(adults$return_theatre_yn, useNA="ifany"))
cat("\nIn-hospital death:\n"); print(table(adults$death_inhospital, useNA="ifany"))
cat("\n30-day follow-up death:\n"); print(table(adults$death_30d, useNA="ifany"))
cat("\nLOS (days): median=", median(adults$los_days, na.rm=TRUE),
    " IQR=", quantile(adults$los_days,.25,na.rm=T),"-",
    quantile(adults$los_days,.75,na.rm=T),"\n")
cat("\nLOS category:\n"); print(table(adults$los_cat, useNA="ifany"))
cat("\nPayment:\n"); print(table(adults$payment_cat, useNA="ifany"))
cat("\nWHO checklist:\n"); print(table(adults$who_checklist_f, useNA="ifany"))
cat("\nPulse oximeter:\n"); print(table(adults$intraop_oximeter_f, useNA="ifany"))
cat("\nECG monitoring:\n"); print(table(adults$intraop_ecg_f, useNA="ifany"))
cat("\nCapnography:\n"); print(table(adults$intraop_capnography_f, useNA="ifany"))
cat("\nERAS protocol:\n"); print(table(adults$eras_protocol_f, useNA="ifany"))
cat("\nVTE prophylaxis:\n"); print(table(adults$vte_prophy_f, useNA="ifany"))
cat("\nSurgeon level:\n"); print(table(adults$surgeon_level, useNA="ifany"))
cat("\nAnaes provider level:\n"); print(table(adults$anaes_level, useNA="ifany"))
cat("\nDisposition:\n"); print(table(adults$disposition_cat, useNA="ifany"))
cat("\nPOMS any morbidity:\n"); print(table(adults$poms_any, useNA="ifany"))
cat("\nCause of death:\n"); print(table(adults$cause_death_cat, useNA="ifany"))

cat("\n========== PAEDIATRICS (n=", nrow(paeds), ") ==========\n", sep="")
cat("Age (years): median=", median(paeds$age_years, na.rm=TRUE),
    " IQR=", quantile(paeds$age_years,.25,na.rm=T),"-",
    quantile(paeds$age_years,.75,na.rm=T),"\n")
cat("\nAge groups:\n"); print(table(paeds$age_group, useNA="ifany"))
cat("\nSex:\n"); print(table(paeds$sex, useNA="ifany"))
cat("\nProcedure group:\n"); print(table(paeds$procedure_group, useNA="ifany"))
cat("\nUrgency:\n"); print(table(paeds$urgency_cat, useNA="ifany"))
cat("\nASA:\n"); print(table(paeds$asa_cat, useNA="ifany"))
cat("\nAnaesthesia:\n"); print(table(paeds$anaes_group, useNA="ifany"))
cat("\nClavien grade:\n"); print(table(paeds$clavien_grade, useNA="ifany"))
cat("\nClavien group:\n"); print(table(paeds$clavien_group, useNA="ifany"))
cat("\nAny complication:\n"); print(table(paeds$any_complication, useNA="ifany"))
cat("\nSSI:\n"); print(table(paeds$ssi_yn, useNA="ifany"))
cat("\nIn-hospital death:\n"); print(table(paeds$death_inhospital, useNA="ifany"))
cat("\nLOS (days): median=", median(paeds$los_days, na.rm=TRUE),
    " IQR=", quantile(paeds$los_days,.25,na.rm=T),"-",
    quantile(paeds$los_days,.75,na.rm=T),"\n")
cat("\nLOS category:\n"); print(table(paeds$los_cat, useNA="ifany"))

# ── SAVE ─────────────────────────────────────────────────────────────────────
saveRDS(adults, file.path(proc_dir, "d_adults.rds"))
saveRDS(paeds,  file.path(proc_dir, "d_paeds.rds"))
write_csv(adults, file.path(proc_dir, "d_adults.csv"))
write_csv(paeds,  file.path(proc_dir, "d_paeds.csv"))
cat("\n✓ Saved d_adults.rds/.csv and d_paeds.rds/.csv to", proc_dir, "\n")
