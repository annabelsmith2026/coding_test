###############################################################################
# Author: Annabel Smith
# Title:  Question 4 - Summary Table for Treatment-Emergent AEs
# Purpose:
#   In this script, I create a summary table showing the frequency and percentage
#   of treatment‑emergent adverse events (TEAEs) by treatment group using
#   the {gtsummary} package. This mirrors FDA Table 10 style summaries.
#
# Inputs:
#   - ADAE (Adverse Events Dataset)
#   - ADSL (Subject-Level Dataset)
#
# Output:
#   - HTML summary table saved as ae_summary_table.html
###############################################################################

# ---- Load Required Packages ----
library(dplyr)
library(gtsummary)
library(pharmaverseadam)
library(gt)

# ---- Load Data ----
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl
# ---- Filter for Treatment-Emergent AEs ----
adae_teae <- adae_teae %>%
  mutate(ACTARM = coalesce(ACTARM.x, ACTARM.y))

# ---- Derive total TEAE subjects per arm ----
# This row will represent subjects with at least one TEAE.
total_row <- adae_teae %>%
  distinct(USUBJID, ACTARM) %>%                # 1 row per subject-arm
  count(ACTARM, name = "n_subj_teae") %>%      # how many subjects per arm
  mutate(
    AETERM = "Treatment‑Emergent AEs",         # row label we want
    USUBJID = NA_character_
  )

# ---- Combine totals with event-level data ----
adae_combined <- adae_teae %>%
  select(AETERM, ACTARM, USUBJID) %>%
  bind_rows(total_row %>% select(AETERM, ACTARM, USUBJID))

# ---- Sort AEs by descending frequency (real events first) ----
freq_order <- adae_teae %>%
  count(AETERM, sort = TRUE) %>%
  pull(AETERM)
adae_combined <- adae_combined %>%
  mutate(AETERM = factor(AETERM,
                         levels = c("Treatment‑Emergent AEs", freq_order)))
# ---- Build AE Summary Table ----
adae_teae <- adae_teae %>%
  mutate(ACTARM = coalesce(ACTARM.x, ACTARM.y))

# Count frequency across all arms to order by most frequent AEs first
freq_order <- adae_teae %>%
  count(AETERM, sort = TRUE) %>%
  pull(AETERM)

ae_summary <- adae_teae %>%
  mutate(AETERM = factor(AETERM, levels = freq_order)) %>%  # preserve ordered factor
  select(AETERM, ACTARM, USUBJID) %>%
  tbl_summary(
    by = ACTARM,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  modify_header(label ~ "Adverse Event Term") %>%
  add_overall(last = TRUE) %>%
  modify_spanning_header(starts_with("stat_") ~ "Treatment Group") %>%
  bold_labels() %>%
  as_gt() %>%
  tab_header(title = md("**Treatment‑Emergent Adverse Events (TEAEs)**"),
             subtitle = md("Count (n) and Percentage (%) by Treatment Group"))

# ---- Export the Table ----
gtsave(ae_summary, "question_4_tlg/ae_summary_table.html")
###############################################################################
