# =============================================================================
# COSurgCo Surgical Audit — Phase 3: Summary Tables
# Script: 03_summary_tables.R
# Produces two Word documents (adults / paediatrics), each with three stacked
# sections matching the ASOS Lancet table structure:
#   Section 1 — Patient case-mix
#   Section 2 — Surgical & perioperative profile
#   Section 3 — Postoperative & 30-day outcomes
#
# OUTPUT:
#   outputs/Table1_Adults.docx
#   outputs/Table2_Paediatrics.docx
# =============================================================================

# ── PACKAGES ──────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr)
  library(purrr); library(flextable); library(officer)
})

# ── PATHS ─────────────────────────────────────────────────────────────────────
root_dir  <- file.path("/Users", "matemba", "Library", "CloudStorage",
                       "OneDrive-Personal", "cosecsa_surgical_audits")
proc_dir  <- file.path(root_dir, "data", "processed")
out_dir   <- file.path(root_dir, "outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

adults <- readRDS(file.path(proc_dir, "d_adults.rds"))
paeds  <- readRDS(file.path(proc_dir, "d_paeds.rds"))

# ── FIX LOS OUTLIERS (identified in QC) ──────────────────────────────────────
adults <- adults |>
  mutate(los_days = if_else(!is.na(los_days) & (los_days < 0 | los_days > 180),
                            NA_real_, los_days),
         los_cat  = case_when(
           los_days <= 3  ~ "<=3 days",
           los_days <= 7  ~ "4-7 days",
           los_days <= 14 ~ "8-14 days",
           los_days >  14 ~ ">14 days",
           TRUE ~ NA_character_
         ) |> factor(levels = c("<=3 days","4-7 days","8-14 days",">14 days")))

paeds <- paeds |>
  mutate(los_days = if_else(!is.na(los_days) & (los_days < 0 | los_days > 180),
                            NA_real_, los_days),
         los_cat  = case_when(
           los_days <= 3  ~ "<=3 days",
           los_days <= 7  ~ "4-7 days",
           los_days <= 14 ~ "8-14 days",
           los_days >  14 ~ ">14 days",
           TRUE ~ NA_character_
         ) |> factor(levels = c("<=3 days","4-7 days","8-14 days",">14 days")))

# =============================================================================
# TABLE ENGINE
# =============================================================================

# ── Column splitter: returns 5 sub-datasets ───────────────────────────────────
make_cols <- function(d) {
  list(
    All        = d,
    Comp       = d |> filter(any_complication == 1),
    No_comp    = d |> filter(any_complication == 0),
    Died       = d |> filter(death_inhospital == 1),
    Survived   = d |> filter(death_inhospital == 0)
  )
}

# ── Continuous summary: median (IQR) ─────────────────────────────────────────
fmt_cont <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) return("—")
  sprintf("%.1f (%.1f–%.1f)",
          median(x), quantile(x, 0.25), quantile(x, 0.75))
}

# ── Categorical summary: n (%) ────────────────────────────────────────────────
fmt_cat <- function(x, level, total_n) {
  n <- sum(!is.na(x) & as.character(x) == level, na.rm = TRUE)
  if (total_n == 0) return("—")
  sprintf("%d (%.1f)", n, 100 * n / total_n)
}

# ── Count non-missing for denominator ────────────────────────────────────────
denom <- function(x) sum(!is.na(x) & as.character(x) != "")

# ── Build one summary row ─────────────────────────────────────────────────────
# type = "cont"  → one row: variable label | median (IQR) across 5 cols
# type = "cat"   → header row (blank values) + one row per level
# type = "yn"    → single "Yes" row for binary 0/1 variables

build_rows <- function(cols, var, label, type = "cat", levels = NULL,
                       level_labels = NULL, indent = TRUE) {

  n_all <- nrow(cols$All)

  if (type == "cont") {
    vals <- map_chr(cols, ~ fmt_cont(.x[[var]]))
    return(tibble(
      label      = if (indent) paste0("  ", label) else label,
      All        = vals[["All"]],
      Comp       = vals[["Comp"]],
      No_comp    = vals[["No_comp"]],
      Died       = vals[["Died"]],
      Survived   = vals[["Survived"]],
      is_header  = FALSE
    ))
  }

  if (type == "yn") {
    vals <- map_chr(cols, function(d) {
      x <- d[[var]]
      if (is.factor(x) || is.character(x)) {
        xc  <- as.character(x)
        n   <- sum(xc == "Yes", na.rm = TRUE)
        tot <- sum(xc %in% c("Yes", "No"), na.rm = TRUE)
      } else {
        n   <- sum(x == 1, na.rm = TRUE)
        tot <- denom(x)
      }
      if (tot == 0) "—" else sprintf("%d (%.1f)", n, 100 * n / tot)
    })
    return(tibble(
      label     = if (indent) paste0("  ", label) else label,
      All       = vals[["All"]],
      Comp      = vals[["Comp"]],
      No_comp   = vals[["No_comp"]],
      Died      = vals[["Died"]],
      Survived  = vals[["Survived"]],
      is_header = FALSE
    ))
  }

  # type == "cat"
  x_all <- cols$All[[var]]
  if (is.null(levels)) levels <- sort(unique(na.omit(as.character(x_all))))
  if (is.null(level_labels)) level_labels <- levels

  header <- tibble(
    label     = label,
    All       = "",
    Comp      = "",
    No_comp   = "",
    Died      = "",
    Survived  = "",
    is_header = TRUE
  )

  level_rows <- map2_dfr(levels, level_labels, function(lv, lb) {
    vals <- map_chr(cols, function(d) {
      x   <- d[[var]]
      tot <- denom(x)
      fmt_cat(x, lv, tot)
    })
    tibble(
      label     = paste0("  ", lb),
      All       = vals[["All"]],
      Comp      = vals[["Comp"]],
      No_comp   = vals[["No_comp"]],
      Died      = vals[["Died"]],
      Survived  = vals[["Survived"]],
      is_header = FALSE
    )
  })

  bind_rows(header, level_rows)
}

# ── Section header row ────────────────────────────────────────────────────────
section_row <- function(title) {
  tibble(label = title, All = "", Comp = "", No_comp = "",
         Died = "", Survived = "", is_header = NA)
}

# ── N row at the top ──────────────────────────────────────────────────────────
n_row <- function(cols) {
  ns <- map_chr(cols, ~ paste0("n = ", nrow(.x)))
  tibble(label = "", All = ns[["All"]], Comp = ns[["Comp"]],
         No_comp = ns[["No_comp"]], Died = ns[["Died"]],
         Survived = ns[["Survived"]], is_header = FALSE)
}

# =============================================================================
# BUILD THE TABLE DATA FRAME
# =============================================================================
build_table <- function(d, cohort = c("adult","paeds")) {
  cohort <- match.arg(cohort)
  cols   <- make_cols(d)

  rows <- bind_rows(

    n_row(cols),

    # ── SECTION 1: Patient case-mix ──────────────────────────────────────────
    section_row("PATIENT CASE-MIX"),

    build_rows(cols, "age_years", "Age (years), median (IQR)", type = "cont"),

    if (cohort == "adult") {
      build_rows(cols, "age_group", "Age group",
                 levels = c("Young adulthood (18-44)",
                            "Middle adulthood (45-64)",
                            "Older adulthood (65+)"))
    } else {
      build_rows(cols, "age_group", "Age group",
                 levels = c("Neonate (0-28 days)",
                            "Infant (29 days-<2 yrs)",
                            "Child/adolescent (2-17 yrs)"))
    },

    build_rows(cols, "sex", "Sex",
               levels = c("Male","Female"),
               level_labels = c("Male","Female")),

    build_rows(cols, "bmi", "BMI (kg/m\u00b2), median (IQR)", type = "cont"),

    build_rows(cols, "bmi_cat", "BMI category",
               levels = c("Underweight","Normal weight","Overweight","Obese")),

    build_rows(cols, "hb_gdl", "Haemoglobin (g/dL), median (IQR)", type = "cont"),

    build_rows(cols, "facilities_prior",
               "Facilities visited prior, median (IQR)", type = "cont"),

    build_rows(cols, "payment_cat", "Mode of surgical payment",
               levels = c("Health insurance","Self-payment","Other")),

    build_rows(cols, "any_comorbidity",
               "Any pre-existing comorbidity", type = "yn"),

    build_rows(cols, "cm_htn",          "  Hypertension",          type = "yn"),
    build_rows(cols, "cm_dm",           "  Diabetes mellitus",     type = "yn"),
    build_rows(cols, "cm_hiv",          "  HIV",                   type = "yn"),
    build_rows(cols, "cm_asthma",       "  Asthma",                type = "yn"),
    build_rows(cols, "cm_malnutrition", "  Malnutrition",          type = "yn"),
    build_rows(cols, "cm_copd",         "  COPD",                  type = "yn"),
    build_rows(cols, "cm_renal",        "  Chronic renal disease", type = "yn"),
    build_rows(cols, "cm_cancer",       "  Metastatic cancer",     type = "yn"),
    build_rows(cols, "cm_blood",        "  Blood disorders (e.g. sickle cell)", type = "yn"),

    build_rows(cols, "asa_cat", "ASA physical status",
               levels = c("ASA I","ASA II","ASA III","ASA IV","ASA V")),

    # ── SECTION 2: Surgical & perioperative profile ──────────────────────────
    section_row("SURGICAL & PERIOPERATIVE PROFILE"),

    build_rows(cols, "procedure_group", "Surgical procedure group"),

    build_rows(cols, "urgency_cat", "Urgency of surgery",
               levels = c("Elective","Urgent","Emergency")),

    build_rows(cols, "anaes_group", "Anaesthesia type",
               levels = c("General anaesthesia","Regional anaesthesia",
                          "Local anaesthesia","MAC / Sedation","WALANT")),

    build_rows(cols, "wound_cat", "Wound classification",
               levels = c("Class I: Clean","Class II: Clean-contaminated",
                          "Class III: Contaminated","Class IV: Dirty/infected")),

    build_rows(cols, "surgeon_level", "Most senior surgeon",
               levels = c("Superspecialist","Specialist surgeon",
                          "COSECSA fellow","Resident/registrar")),

    build_rows(cols, "anaes_level", "Most senior anaesthesia provider",
               levels = c("Specialist anaesthetist","Non-specialist physician",
                          "Non-physician anaesthetist","Anaesthesia trainee","Surgeon")),

    build_rows(cols, "fasting_cat", "Preoperative fasting",
               levels = c("Adequate","Prolonged","No fasting")),

    build_rows(cols, "prophy_abx_f",     "Prophylactic antibiotics given",   type = "yn"),
    build_rows(cols, "intraop_abx_f",    "Intraoperative antibiotics given",  type = "yn"),
    build_rows(cols, "vte_prophy_f",     "VTE pharmacological prophylaxis",   type = "yn"),
    build_rows(cols, "who_checklist_f",  "WHO Surgical Safety Checklist completed", type = "yn"),
    build_rows(cols, "intraop_oximeter_f","Pulse oximeter monitoring",        type = "yn"),
    build_rows(cols, "intraop_ecg_f",    "Continuous ECG monitoring",         type = "yn"),
    build_rows(cols, "intraop_capnography_f","Capnography monitoring",         type = "yn"),
    build_rows(cols, "eras_protocol_f",  "ERAS protocol used",                type = "yn"),
    build_rows(cols, "minimal_invasive_f","Minimally invasive approach", type = "cat",
               levels = c("Yes","Yes (converted to open)","No")),
    build_rows(cols, "intraop_transfusion_f","Intraoperative blood transfusion", type = "yn"),
    build_rows(cols, "field_prep_f",     "Surgical field preparation per standards", type = "yn"),

    build_rows(cols, "surg_duration_min",
               "Duration of surgery (minutes), median (IQR)", type = "cont"),
    build_rows(cols, "blood_loss_ml",
               "Estimated blood loss (mL), median (IQR)", type = "cont"),

    build_rows(cols, "disposition_cat", "Disposition after surgery",
               levels = c("General ward","HDU","ICU","Discharge home")),

    # ── SECTION 3: Postoperative & 30-day outcomes ───────────────────────────
    section_row("POSTOPERATIVE & 30-DAY OUTCOMES"),

    build_rows(cols, "any_complication",
               "Any postoperative complication (Clavien >=I)", type = "yn"),

    build_rows(cols, "clavien_group", "Clavien-Dindo classification",
               levels = c("Minor (I-II)","Major (III-IV)","Death (V)")),

    build_rows(cols, "clavien_grade", "Clavien-Dindo grade",
               levels = c("I","II","III","IIIa","IIIb","IV","V")),

    build_rows(cols, "poms_any",
               "Any POMS morbidity", type = "yn"),
    build_rows(cols, "poms_pulmonary",    "  Pulmonary",           type = "yn"),
    build_rows(cols, "poms_infectious",   "  Infectious (fever/antibiotics)", type = "yn"),
    build_rows(cols, "poms_renal",        "  Renal",               type = "yn"),
    build_rows(cols, "poms_gi",           "  Gastrointestinal",    type = "yn"),
    build_rows(cols, "poms_cardiovascular","  Cardiovascular",     type = "yn"),
    build_rows(cols, "poms_neurological", "  Neurological",        type = "yn"),
    build_rows(cols, "poms_haematological","  Haematological",     type = "yn"),
    build_rows(cols, "poms_wound",        "  Wound",               type = "yn"),
    build_rows(cols, "poms_pain",         "  Pain",                type = "yn"),

    build_rows(cols, "ssi_yn",           "Surgical site infection (SSI)", type = "yn"),
    build_rows(cols, "ssi_type_cat", "SSI type",
               levels = c("Superficial SSI","Deep SSI","Other SSI")),

    build_rows(cols, "ngt_required_f",   "Nasogastric tube decompression", type = "yn"),

    build_rows(cols, "return_theatre_yn","Return to theatre (in-hospital)", type = "yn"),
    build_rows(cols, "return_30d_yn",    "Return to theatre (30 days)",     type = "yn"),

    build_rows(cols, "death_inhospital", "In-hospital mortality", type = "yn"),
    build_rows(cols, "cause_death_cat",  "Cause of death"),

    build_rows(cols, "death_30d",        "30-day mortality", type = "yn"),

    build_rows(cols, "los_days",
               "Length of stay (days), median (IQR)", type = "cont"),
    build_rows(cols, "los_cat", "Length of stay category",
               levels = c("<=3 days","4-7 days","8-14 days",">14 days"))
  )

  rows
}

# =============================================================================
# RENDER TO FLEXTABLE
# =============================================================================
render_flextable <- function(tbl_df, title, footnote) {

  # Rename columns for display
  display <- tbl_df |>
    select(-is_header) |>
    rename(
      "Variable"                   = label,
      "All patients"               = All,
      "With complications"         = Comp,
      "Without complications"      = No_comp,
      "Died"                       = Died,
      "Survived"                   = Survived
    )

  ft <- flextable(display) |>
    set_header_labels(
      Variable            = "Variable",
      `All patients`      = "All patients",
      `With complications`= "With complications",
      `Without complications` = "Without complications",
      Died                = "Died",
      Survived            = "Survived"
    ) |>
    # Column widths (inches) — total ~9in for A4 landscape
    width(j = 1, width = 3.0) |>
    width(j = 2:6, width = 1.1) |>
    # Font
    font(fontname = "Arial", part = "all") |>
    fontsize(size = 9, part = "all") |>
    fontsize(size = 10, part = "header") |>
    # Header styling
    bold(part = "header") |>
    bg(bg = "#2F4F6F", part = "header") |>
    color(color = "white", part = "header") |>
    align(align = "center", part = "header") |>
    align(j = 2:6, align = "center", part = "body") |>
    # Borders
    border_remove() |>
    hline_top(border = fp_border(color = "#2F4F6F", width = 2), part = "header") |>
    hline_bottom(border = fp_border(color = "#2F4F6F", width = 2), part = "header") |>
    hline_bottom(border = fp_border(color = "#2F4F6F", width = 1.5), part = "body") |>
    # Padding
    padding(padding.top = 3, padding.bottom = 3, part = "all") |>
    padding(j = 1, padding.left = 6, part = "body")

  # Section header rows (is_header == NA) — dark background full-width
  sec_rows <- which(is.na(tbl_df$is_header))
  if (length(sec_rows) > 0) {
    ft <- ft |>
      bold(i = sec_rows, part = "body") |>
      bg(i = sec_rows, bg = "#D9E1F2", part = "body") |>
      color(i = sec_rows, color = "#1F3864", part = "body") |>
      merge_h(i = sec_rows, part = "body")
  }

  # Variable header rows (is_header == TRUE) — bold label, blank values
  hdr_rows <- which(tbl_df$is_header == TRUE)
  if (length(hdr_rows) > 0) {
    ft <- ft |> bold(i = hdr_rows, j = 1, part = "body")
  }

  # Alternating row shading (skip section & header rows)
  body_rows <- which(!is.na(tbl_df$is_header))
  alt_rows  <- body_rows[seq(2, length(body_rows), 2)]
  if (length(alt_rows) > 0) {
    ft <- ft |> bg(i = alt_rows, bg = "#F5F7FA", part = "body")
  }

  # Caption & footnote
  ft <- ft |>
    set_caption(
      caption   = as_paragraph(as_b(title)),
      fp_p      = fp_par(text.align = "left")
    ) |>
    add_footer_lines(
      values = c(
        footnote,
        "Data presented as n (%) for categorical variables and median (IQR) for continuous variables.",
        "Denominators vary with data completeness. BMI available in patients with both weight and height recorded.",
        "30-day follow-up data available for 80/449 patients (18%). LOS outliers (>180 days or negative) set to missing."
      )
    ) |>
    fontsize(size = 8, part = "footer") |>
    italic(part = "footer")

  ft
}

# =============================================================================
# PRODUCE BOTH TABLES
# =============================================================================

# ── Adults ────────────────────────────────────────────────────────────────────
cat("Building adults table...\n")
tbl_adults_df <- build_table(adults, cohort = "adult")

ft_adults <- render_flextable(
  tbl_adults_df,
  title    = "Table 1. Description of adult patients undergoing gastrointestinal and related surgery across COSECSA-accredited hospitals (COSurgCo Audit, 2022-2024)",
  footnote = "Adult patients (>=18 years) undergoing elective gastrointestinal, oncologic, orthopaedic, urological, cardiothoracic, or neurosurgical procedures."
)

# ── Paediatrics ───────────────────────────────────────────────────────────────
cat("Building paediatrics table...\n")
tbl_paeds_df <- build_table(paeds, cohort = "paeds")

ft_paeds <- render_flextable(
  tbl_paeds_df,
  title    = "Table 2. Description of paediatric patients undergoing oncology and congenital anomaly surgery across COSECSA-accredited hospitals (COSurgCo Audit, 2022-2024)",
  footnote = "Paediatric patients (<18 years) undergoing elective oncological or congenital anomaly surgical procedures."
)

# =============================================================================
# EXPORT TO WORD
# =============================================================================
export_docx <- function(ft, path, landscape = TRUE) {
  sec <- prop_section(
    page_size = if (landscape) {
      page_size(width = 11.7, height = 8.3, orient = "landscape")
    } else {
      page_size(width = 8.3,  height = 11.7, orient = "portrait")
    },
    page_margins = page_mar(
      top = 0.5, bottom = 0.5, left = 0.5, right = 0.5
    )
  )
  doc <- read_docx() |>
    body_add_flextable(ft) |>
    body_set_default_section(value = sec)
  print(doc, target = path)
  cat("Saved:", path, "\n")
}

adults_path <- file.path(out_dir, "Table1_Adults.docx")
paeds_path  <- file.path(out_dir, "Table2_Paediatrics.docx")

export_docx(ft_adults, adults_path)
export_docx(ft_paeds,  paeds_path)

cat("\n✓ Phase 3 complete.\n")
cat("  Table1_Adults.docx        — adults      (", nrow(adults), "patients)\n", sep="")
cat("  Table2_Paediatrics.docx   — paediatrics (", nrow(paeds),  "patients)\n", sep="")
