###############################################################################
# Author: Annabel Smith
# Date:   2026‑05‑02
# Title:  ADaM ADSL Dataset Creation
#
# Purpose:
#   In this script, I create the Subject‑Level Analysis Dataset (ADSL) using
#   standard SDTM input data from {pharmaversesdtm}.  I follow the general
#   {admiral} ADSL example, adding custom derivations for variables requested
#   in this assignment.
#
# Inputs:
#   - pharmaversesdtm::dm   (Demographics)
#   - pharmaversesdtm::vs   (Vital Signs)
#   - pharmaversesdtm::ex   (Exposure)
#   - pharmaversesdtm::ds   (Disposition)
#   - pharmaversesdtm::ae   (Adverse Events)
#
# Outputs:
#   - A dataset named 'adsl' that contains all derived variables:
#     AGEGR9, AGEGR9N, TRTSDTM, ITTFL, ABNSBPFL, LSTALVDT, CARPOPFL
###############################################################################
# ---- Prepare folder structure ----
setwd("/cloud/project")
dir.create("question_3_adam", recursive = TRUE)

# ---- Load Required Packages ----
pkgs <- c("tidyverse", "lubridate", "devtools")
install.packages(setdiff(pkgs, rownames(installed.packages())))
# Pharmaverse packages from GitHub
if (!"admiral" %in% rownames(installed.packages()))
  devtools::install_github("pharmaverse/admiral")
if (!"pharmaversesdtm" %in% rownames(installed.packages()))
  devtools::install_github("pharmaverse/pharmaversesdtm")
library(admiral)
library(tidyverse)
library(lubridate)
library(pharmaversesdtm)

# ---- Load Source Data ----
dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

# ---- Start ADSL using DM ----
# I begin with the DM domain as the foundation for ADSL.
adsl <- dm

# ---- Derive AGEGR9 and AGEGR9N ----
# I group AGE into <18, 18–50, >50 as text and numeric categories.
adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE <= 50 ~ "18 - 50",
      AGE > 50 ~ ">50",
      TRUE ~ NA_character_
    ),
    AGEGR9N = case_when(
      AGE < 18 ~ 1,
      AGE >= 18 & AGE <= 50 ~ 2,
      AGE > 50 ~ 3,
      TRUE ~ NA_real_
    )
  )

# ---- Derive TRTSDTM: Treatment Start Date‑Time ----
# Treatment start = first valid exposure record per USUBJID
# Impute missing time components (00:00:00) where allowed.
trt <- ex %>%
  filter(EXDOSE > 0 | (EXDOSE == 0 & str_detect(toupper(EXTRT), "PLACEBO"))) %>%
  mutate(TRTSDTM = convert_dtc_to_dtm(EXSTDTC, highest_imputation = "h", date_imputation = "first"),
         TRTSDTM = if_else(is.na(TRTSDTM),
                           ymd_hms(paste(substr(EXSTDTC, 1, 10), "00:00:00")),
                           TRTSDTM)) %>%
  group_by(USUBJID) %>%
  slice_min(TRTSDTM, with_ties = FALSE) %>%
  ungroup() %>%
  select(USUBJID, TRTSDTM)

adsl <- left_join(adsl, trt, by = "USUBJID")

# ---- Derive ITTFL ----
# Flag = "Y" if randomized (ARM not missing)
adsl <- adsl %>%
  mutate(ITTFL = if_else(!is.na(ARM) & ARM != "", "Y", "N"))

# ---- Derive ABNSBPFL ----
# Identify patients with supine systolic BP outside 100–139 mmHg range.
abn_vs <- vs %>%
  filter(VSTESTCD == "SYSBP", VSSTRESU == "mmHg") %>%
  group_by(USUBJID) %>%
  summarize(ABNSBPFL = if_else(any(VSSTRESN < 100 | VSSTRESN >= 140, na.rm = TRUE), "Y", "N"))

adsl <- left_join(adsl, abn_vs, by = "USUBJID")

# ---- Derive LSTALVDT ----
# The last date patient known alive drawn from VS, AE, DS, and EX.
last_alive_sources <- list(
  vs_date = vs %>%
    filter(!is.na(VSDTC), (!is.na(VSSTRESC) | !is.na(VSSTRESN))) %>%
    mutate(date = convert_dtc_to_dt(VSDTC)) %>%
    group_by(USUBJID) %>%
    summarize(vs_last_date = max(date, na.rm = TRUE), .groups = "drop"),
  ae_date = ae %>%
    filter(!is.na(AESTDTC)) %>%
    mutate(date = convert_dtc_to_dt(AESTDTC)) %>%
    group_by(USUBJID) %>%
    summarize(ae_last_date = max(date, na.rm = TRUE), .groups = "drop"),
  ds_date = ds %>%
    filter(!is.na(DSSTDTC)) %>%
    mutate(date = convert_dtc_to_dt(DSSTDTC)) %>%
    group_by(USUBJID) %>%
    summarize(ds_last_date = max(date, na.rm = TRUE), .groups = "drop"),
  ex_date = ex %>%
    filter(EXDOSE > 0 | (EXDOSE == 0 & str_detect(toupper(EXTRT), "PLACEBO"))) %>%
    mutate(date = convert_dtc_to_dt(EXSTDTC)) %>%
    group_by(USUBJID) %>%
    summarize(ex_last_date = max(date, na.rm = TRUE), .groups = "drop")
)

# Merge and take max across all sources
lstalvdt <- reduce(last_alive_sources, full_join, by = "USUBJID") %>%
  mutate(LSTALVDT = pmax(vs_last_date, ae_last_date, ds_last_date, ex_last_date, na.rm = TRUE)) %>%
  select(USUBJID, LSTALVDT)

adsl <- left_join(adsl, lstalvdt, by = "USUBJID")

# ---- Derive CARPOPFL ----
# Cardiac disorder flag based on AE SOC.
cardiac <- ae %>%
  filter(toupper(AESOC) == "CARDIAC DISORDERS") %>%
  distinct(USUBJID) %>%
  mutate(CARPOPFL = "Y")

adsl <- left_join(adsl, cardiac, by = "USUBJID")

# ---- Final Review ----
message("Preview of ADSL derivations:")
glimpse(adsl)

# ---- Save Output ----
write.csv(adsl, "question_3_adam/adsl_output.csv", row.names = FALSE)

# ---- Summary of Logic ----
# 1. Start with Demographics (DM) as base.
# 2. Derive AGEGR9 and AGEGR9N for analysis age groupings.
# 3. Derive TRTSDTM from first valid exposure record (EX).
# 4. Derive ITTFL flag using randomization variable (ARM).
# 5. Derive ABNSBPFL using systolic blood pressure thresholds from VS.
# 6. Derive LSTALVDT by consolidating last known dates from VS, AE, DS, EX.
# 7. Derive CARPOPFL flag for cardiac adverse events.
###############################################################################
