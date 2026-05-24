# =============================================================================
# COSurgCo — Script 04c: Summary Tables — Combined Dataset
# Input:  data/processed/combined_adults.rds
#         data/processed/combined_paeds.rds
# Output: outputs/Table1_Adults_Combined.docx
#         outputs/Table2_Paeds_Combined.docx
#
# Formatting matches your revised Table1_Adults.docx exactly:
#   Col widths: 5103 / 1871 / 1871 / 1871 / 1871 / 1874 twips
#               (3.54 / 1.30 / 1.30 / 1.30 / 1.30 / 1.30 inches)
#   Font: Arial 10pt
#   Header: #2F4F6F bg, white bold centred
#   Section rows: #D9E1F2 bg, #1F3864 text, bold, 2-cell merged
#   Category header rows: bold col1 only, no data
#   Continuous rows: bold col1 only
#   Data rows: not bold, alternating #FFFFFF / #F5F7FA
#   Cols 2-6: centred
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr)
  library(purrr); library(flextable); library(officer)
})

root_dir <- file.path("/Users","matemba","Library","CloudStorage",
                      "OneDrive-Personal","cosecsa_surgical_audits")
proc_dir <- file.path(root_dir, "data", "processed")
out_dir  <- file.path(root_dir, "outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

adults <- readRDS(file.path(proc_dir, "combined_adults.rds"))
paeds  <- readRDS(file.path(proc_dir, "combined_paeds.rds"))

cat("Combined adults:", nrow(adults), "| Combined paeds:", nrow(paeds), "\n")

# =============================================================================
# TABLE ENGINE (identical to 03_summary_tables.R)
# =============================================================================
make_cols <- function(d) list(
  All      = d,
  Comp     = d |> filter(any_complication == 1),
  No_comp  = d |> filter(any_complication == 0),
  Died     = d |> filter(death_30d == 1),        # use 30d mortality (available in both)
  Survived = d |> filter(death_30d == 0)
)

fmt_cont <- function(x) {
  x <- as.numeric(x); x <- x[!is.na(x)]
  if (!length(x)) return("\u2014")
  sprintf("%.1f (%.1f\u2013%.1f)", median(x), quantile(x,.25), quantile(x,.75))
}
denom <- function(x) sum(!is.na(x) & as.character(x) != "")
fmt_cat <- function(x, level, total_n) {
  n <- sum(!is.na(x) & as.character(x) == level, na.rm=TRUE)
  if (!total_n) return("\u2014")
  sprintf("%d (%.1f)", n, 100*n/total_n)
}

build_rows <- function(cols, var, label, type="cat", levels=NULL, level_labels=NULL) {
  if (type == "cont") {
    vals <- map_chr(cols, ~fmt_cont(.x[[var]]))
    return(tibble(label=paste0("  ",label), All=vals[1], Comp=vals[2],
                  No_comp=vals[3], Died=vals[4], Survived=vals[5], is_header=FALSE,
                  bold_label=TRUE))
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
      if (!tot) "\u2014" else sprintf("%d (%.1f)", n, 100*n/tot)
    })
    return(tibble(label=paste0("  ",label), All=vals[1], Comp=vals[2],
                  No_comp=vals[3], Died=vals[4], Survived=vals[5], is_header=FALSE,
                  bold_label=TRUE))
  }
  x_all <- cols$All[[var]]
  if (is.null(levels)) levels <- sort(unique(na.omit(as.character(x_all))))
  if (is.null(level_labels)) level_labels <- levels
  hdr <- tibble(label=label, All="", Comp="", No_comp="", Died="", Survived="",
                is_header=TRUE, bold_label=TRUE)
  lvl_rows <- map2_dfr(levels, level_labels, function(lv, lb) {
    vals <- map_chr(cols, function(d) {
      x <- d[[var]]; tot <- denom(x); fmt_cat(x, lv, tot)
    })
    tibble(label=paste0("  ",lb), All=vals[1], Comp=vals[2],
           No_comp=vals[3], Died=vals[4], Survived=vals[5],
           is_header=FALSE, bold_label=FALSE)
  })
  bind_rows(hdr, lvl_rows)
}

section_row <- function(title)
  tibble(label=title, All="", Comp="", No_comp="", Died="", Survived="",
         is_header=NA, bold_label=TRUE)

n_row <- function(cols) {
  ns <- map_chr(cols, ~paste0("n = ", nrow(.x)))
  tibble(label="", All=ns[1], Comp=ns[2], No_comp=ns[3], Died=ns[4], Survived=ns[5],
         is_header=FALSE, bold_label=FALSE)
}

# NOTE: 'Died' and 'Survived' columns use 30-day mortality (available in both
# pilot and implementation). A footnote clarifies this.

# =============================================================================
# BUILD TABLE DATA FRAME
# =============================================================================
build_table <- function(d, cohort = c("adult","paeds")) {
  cohort <- match.arg(cohort)
  cols   <- make_cols(d)

  bind_rows(
    n_row(cols),
    section_row("PATIENT CASE-MIX"),
    build_rows(cols, "age_years", "Age (years), median (IQR)", type="cont"),
    if (cohort == "adult") {
      build_rows(cols, "age_group", "Age group",
                 levels=c("Young adulthood (18-44)","Middle adulthood (45-64)",
                          "Older adulthood (65+)"))
    } else {
      build_rows(cols, "age_group", "Age group",
                 levels=c("Neonate (0-28 days)","Infant (29 days-<2 yrs)",
                          "Child/adolescent (2-17 yrs)"))
    },
    build_rows(cols, "sex", "Sex", levels=c("Male","Female")),
    build_rows(cols, "bmi", "BMI (kg/m\u00b2), median (IQR)", type="cont"),
    build_rows(cols, "bmi_cat", "BMI category",
               levels=c("Underweight","Normal weight","Overweight","Obese")),
    build_rows(cols, "hb_gdl", "Haemoglobin (g/dL), median (IQR)", type="cont"),
    if (cohort == "adult") {
      build_rows(cols, "facilities_prior",
                 "Facilities visited prior, median (IQR)", type="cont")
    } else {
      tibble(label="  Facilities visited prior, median (IQR)",
             All="\u2014", Comp="\u2014", No_comp="\u2014", Died="\u2014",
             Survived="\u2014", is_header=FALSE, bold_label=TRUE)
    },
    build_rows(cols, "payment_cat", "Mode of surgical payment",
               levels=c("Health insurance","Self-payment","Other")),
    build_rows(cols, "any_comorbidity", "Any pre-existing comorbidity", type="yn"),
    build_rows(cols, "cm_htn",          "  Hypertension",          type="yn"),
    build_rows(cols, "cm_dm",           "  Diabetes mellitus",     type="yn"),
    build_rows(cols, "cm_hiv",          "  HIV",                   type="yn"),
    build_rows(cols, "cm_asthma",       "  Asthma",                type="yn"),
    build_rows(cols, "cm_malnutrition", "  Malnutrition",          type="yn"),
    build_rows(cols, "cm_copd",         "  COPD",                  type="yn"),
    build_rows(cols, "cm_renal",        "  Chronic renal disease", type="yn"),
    build_rows(cols, "cm_cancer",       "  Metastatic cancer",     type="yn"),
    build_rows(cols, "cm_blood",        "  Blood disorders",       type="yn"),
    build_rows(cols, "asa_cat", "ASA physical status",
               levels=c("ASA I","ASA II","ASA III","ASA IV","ASA V")),

    section_row("SURGICAL & PERIOPERATIVE PROFILE"),
    build_rows(cols, "procedure_group", "Surgical procedure group"),
    build_rows(cols, "urgency_cat", "Urgency of surgery",
               levels=c("Elective","Urgent","Emergency")),
    build_rows(cols, "anaes_group", "Anaesthesia type",
               levels=c("General anaesthesia","Regional anaesthesia",
                        "Local anaesthesia","MAC / Sedation","WALANT","Other/unknown")),
    build_rows(cols, "wound_cat", "Wound classification",
               levels=c("Class I: Clean","Class II: Clean-contaminated",
                        "Class III: Contaminated","Class IV: Dirty/infected")),
    build_rows(cols, "surgeon_level", "Most senior surgeon",
               levels=c("Superspecialist","Specialist surgeon",
                        "COSECSA fellow","Resident/registrar")),
    build_rows(cols, "anaes_level", "Most senior anaesthesia provider",
               levels=c("Specialist anaesthetist","Non-specialist physician",
                        "Non-physician anaesthetist","Anaesthesia trainee","Surgeon")),
    build_rows(cols, "fasting_cat", "Preoperative fasting",
               levels=c("Adequate","Prolonged","No fasting")),
    build_rows(cols, "prophy_abx_f",        "Prophylactic antibiotics",     type="yn"),
    build_rows(cols, "intraop_abx_f",       "Intraoperative antibiotics",   type="yn"),
    build_rows(cols, "vte_prophy_f",        "VTE prophylaxis",              type="yn"),
    build_rows(cols, "who_checklist_f",     "WHO Surgical Safety Checklist",type="yn"),
    build_rows(cols, "intraop_oximeter_f",  "Pulse oximeter monitoring",    type="yn"),
    build_rows(cols, "intraop_ecg_f",       "Continuous ECG monitoring",    type="yn"),
    build_rows(cols, "intraop_capnography_f","Capnography monitoring",       type="yn"),
    build_rows(cols, "eras_protocol_f",     "ERAS protocol used",           type="yn"),
    build_rows(cols, "minimal_invasive_f",  "Minimally invasive approach",  type="cat",
               levels=c("Yes","Yes (converted to open)","No")),
    build_rows(cols, "intraop_transfusion_f","Intraoperative blood transfusion", type="yn"),
    build_rows(cols, "field_prep_f",        "Surgical field prep per standards", type="yn"),
    build_rows(cols, "surg_duration_min",
               "Duration of surgery (minutes), median (IQR)", type="cont"),
    build_rows(cols, "blood_loss_ml",
               "Estimated blood loss (mL), median (IQR)", type="cont"),
    build_rows(cols, "disposition_cat", "Disposition after surgery",
               levels=c("General ward","HDU","ICU","Discharge home","Death on table")),

    section_row("POSTOPERATIVE & 30-DAY OUTCOMES"),
    build_rows(cols, "any_complication",
               "Any postoperative complication (Clavien \u2265I)", type="yn"),
    build_rows(cols, "clavien_group", "Clavien-Dindo classification",
               levels=c("Minor (I-II)","Major (III-IV)","Death (V)")),
    build_rows(cols, "clavien_grade", "Clavien-Dindo grade",
               levels=c("I","II","III","IIIa","IIIb","IV","V")),
    build_rows(cols, "poms_any",            "Any POMS morbidity",           type="yn"),
    build_rows(cols, "poms_pulmonary",      "  Pulmonary",                  type="yn"),
    build_rows(cols, "poms_infectious",     "  Infectious",                 type="yn"),
    build_rows(cols, "poms_renal",          "  Renal",                      type="yn"),
    build_rows(cols, "poms_gi",             "  Gastrointestinal",           type="yn"),
    build_rows(cols, "poms_cardiovascular", "  Cardiovascular",             type="yn"),
    build_rows(cols, "poms_neurological",   "  Neurological",               type="yn"),
    build_rows(cols, "poms_haematological", "  Haematological",             type="yn"),
    build_rows(cols, "poms_wound",          "  Wound",                      type="yn"),
    build_rows(cols, "poms_pain",           "  Pain",                       type="yn"),
    build_rows(cols, "ssi_yn",  "Surgical site infection (SSI)", type="yn"),
    build_rows(cols, "ssi_type_cat", "SSI type",
               levels=c("Superficial SSI","Deep SSI","Organ/space SSI","Other SSI")),
    build_rows(cols, "ngt_required_f", "Nasogastric tube decompression\u00b9", type="yn"),
    build_rows(cols, "return_theatre_yn",
               "Return to theatre in-hospital\u00b9", type="yn"),
    build_rows(cols, "return_30d_yn", "Return to theatre (30 days)", type="yn"),
    build_rows(cols, "death_inhospital",
               "In-hospital mortality\u00b9", type="yn"),
    build_rows(cols, "death_30d",  "30-day mortality", type="yn"),
    build_rows(cols, "cause_death_cat", "Cause of death"),
    build_rows(cols, "los_days",
               "Length of stay (days), median (IQR)", type="cont"),
    build_rows(cols, "los_cat", "Length of stay category",
               levels=c("<=3 days","4-7 days","8-14 days",">14 days"))
  )
}

# =============================================================================
# RENDER TO FLEXTABLE — exact formatting from revised Table1_Adults.docx
# =============================================================================
render_ft <- function(tbl_df, title_text, footnote_specific) {

  display <- tbl_df |>
    select(-is_header, -bold_label) |>
    rename(
      "Variable"               = label,
      "All patients"           = All,
      "With complications"     = Comp,
      "Without complications"  = No_comp,
      "Died"                   = Died,
      "Survived"               = Survived
    )

  ft <- flextable(display) |>
    # ── Column widths: exactly from revised adults docx (in inches) ────────
    # 5103 twips = 3.543in; 1871 twips = 1.299in; 1874 twips = 1.301in
    width(j = 1, width = 3.54) |>
    width(j = 2, width = 1.30) |>
    width(j = 3, width = 1.30) |>
    width(j = 4, width = 1.30) |>
    width(j = 5, width = 1.30) |>
    width(j = 6, width = 1.30) |>
    # ── Font: Arial 10pt ────────────────────────────────────────────────────
    font(fontname = "Arial", part = "all") |>
    fontsize(size = 10, part = "all") |>
    # ── Header ──────────────────────────────────────────────────────────────
    bold(part = "header") |>
    bg(bg = "#2F4F6F", part = "header") |>
    color(color = "white", part = "header") |>
    align(align = "center", part = "header") |>
    # ── Body defaults ───────────────────────────────────────────────────────
    bold(bold = FALSE, part = "body") |>
    align(j = 1, align = "left",   part = "body") |>
    align(j = 2:6, align = "center", part = "body") |>
    # ── Borders ─────────────────────────────────────────────────────────────
    border_remove() |>
    hline_top(border = fp_border(color = "#2F4F6F", width = 2), part = "header") |>
    hline_bottom(border = fp_border(color = "#2F4F6F", width = 2), part = "header") |>
    hline_bottom(border = fp_border(color = "#2F4F6F", width = 1.5), part = "body") |>
    # ── Padding ─────────────────────────────────────────────────────────────
    padding(padding.top = 3, padding.bottom = 3, part = "all") |>
    padding(j = 1, padding.left = 6, part = "body")

  # Section rows (is_header == NA): blue tint, bold, 2-cell merged
  sec_rows <- which(is.na(tbl_df$is_header))
  if (length(sec_rows) > 0) {
    ft <- ft |>
      bg(i = sec_rows, bg = "#D9E1F2", part = "body") |>
      color(i = sec_rows, color = "#1F3864", part = "body") |>
      bold(i = sec_rows, bold = TRUE, part = "body") |>
      merge_h(i = sec_rows, part = "body")
  }

  # Bold label rows (category headers + continuous variable labels)
  # bold_label == TRUE and is_header != NA
  bold_rows <- which(tbl_df$bold_label == TRUE & !is.na(tbl_df$is_header))
  if (length(bold_rows) > 0) {
    ft <- ft |> bold(i = bold_rows, j = 1, bold = TRUE, part = "body")
  }

  # Alternating row shading on all body rows
  n_rows <- nrow(tbl_df)
  even_rows <- seq(2, n_rows, 2)
  odd_rows  <- seq(1, n_rows, 2)
  ft <- ft |>
    bg(i = even_rows, bg = "#F5F7FA", part = "body") |>
    bg(i = odd_rows,  bg = "#FFFFFF", part = "body")

  # Section rows override shading (applied after alternating)
  if (length(sec_rows) > 0) {
    ft <- ft |> bg(i = sec_rows, bg = "#D9E1F2", part = "body")
  }

  # ── Caption & footnotes ──────────────────────────────────────────────────
  ft <- ft |>
    set_caption(
      caption = as_paragraph(as_b(title_text)),
      fp_p    = fp_par(text.align = "left")
    ) |>
    add_footer_lines(values = c(
      footnote_specific,
      "Data presented as n (%) for categorical variables and median (IQR) for continuous variables.",
      "Denominators vary with data completeness.",
      "\u00b9 Available in implementation phase data only (n=449); not collected in pilot phase (n=1,066).",
      "Died/Survived columns defined by 30-day mortality (available in both phases).",
      "LOS outliers (>180 days or negative values) set to missing.",
      paste0("Pilot sites: 7, 8, 9, 11. Implementation sites: 13, 14, 18, 19, 20, 21, 22.")
    )) |>
    fontsize(size = 8, part = "footer") |>
    italic(part = "footer") |>
    font(fontname = "Arial", part = "footer")

  ft
}

# =============================================================================
# PRODUCE TABLES
# =============================================================================
cat("Building combined adults table...\n")
tbl_adults_df <- build_table(adults, cohort = "adult")
ft_adults <- render_ft(
  tbl_adults_df,
  title_text = paste0(
    "Table 1. Description of adult patients undergoing surgery across COSECSA-accredited ",
    "hospitals \u2014 combined pilot and implementation phases (COSurgCo Audit, 2022\u20132024)"
  ),
  footnote_specific = paste0(
    "Adult patients (\u226518 years). Pilot phase: 978 adults from 4 sites (2022\u20132023). ",
    "Implementation phase: 263 adults from 7 sites (2024)."
  )
)

cat("Building combined paeds table...\n")
tbl_paeds_df <- build_table(paeds, cohort = "paeds")
ft_paeds <- render_ft(
  tbl_paeds_df,
  title_text = paste0(
    "Table 2. Description of paediatric patients undergoing surgery across COSECSA-accredited ",
    "hospitals \u2014 combined pilot and implementation phases (COSurgCo Audit, 2022\u20132024)"
  ),
  footnote_specific = paste0(
    "Paediatric patients (<18 years). Pilot phase: 86 paediatric patients from 4 sites. ",
    "Implementation phase: 186 paediatric patients from 7 sites."
  )
)

# =============================================================================
# EXPORT
# =============================================================================
export_docx <- function(ft, path) {
  sec <- prop_section(
    page_size    = page_size(width = 11.7, height = 8.3, orient = "landscape"),
    page_margins = page_mar(top = 0.5, bottom = 0.5, left = 0.5, right = 0.5)
  )
  doc <- read_docx() |>
    body_add_flextable(ft) |>
    body_set_default_section(value = sec)
  print(doc, target = path)
  cat("Saved:", path, "\n")
}

export_docx(ft_adults, file.path(out_dir, "Table1_Adults_Combined.docx"))
export_docx(ft_paeds,  file.path(out_dir, "Table2_Paeds_Combined.docx"))

cat("\n✓ Script 04c complete.\n")
cat("  Adults combined: n =", nrow(adults), "\n")
cat("  Paeds combined:  n =", nrow(paeds),  "\n")
