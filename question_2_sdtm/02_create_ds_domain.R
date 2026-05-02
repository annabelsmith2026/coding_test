###############################################################################
# Author: Annabel Smith
# Date:  2026-05-02
# Title: SDTM DS Domain Creation
#
# Purpose:
#   In this script, I create the SDTM Disposition (DS) domain from the raw
#   clinical trial dataset provided in {pharmaverseraw}, using the helper
#   functions from {sdtm.oak}. I also apply study‑specific controlled
#   terminology to map raw terms into the expected CDISC values.
#
# Inputs:
#   - pharmaverseraw::ds_raw (mock raw disposition data)
#   - study_ct object (study controlled terminology, created below)
#
# Expected Output:
#   - A clean DS domain dataset containing variables:
#     STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT,
#     VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
#
# Notes:
#   - This approach follows the AE (Adverse Event) example from the Pharmaverse
#     examples and adapts it for the Disposition (DS) domain.
#   - I ensure that each key variable is created according to SDTM structure and
#     terminology alignment.
###############################################################################
# Set project structure 
setwd("/cloud/project")
dir.create("question_2_sdtm", recursive = TRUE)
#save file name as 02_create_ds_domain

# ---- Load Required Libraries ----
# If these aren't installed in my environment yet, I can uncomment the install lines.
# install.packages("devtools")
devtools::install_github("pharmaverse/sdtm.oak")
devtools::install_github("pharmaverse/pharmaverseraw")

library(sdtm.oak)
library(pharmaverseraw)
library(dplyr)

# ---- Load Raw Data ----
# This dataset represents the raw disposition data collected in the trial.
ds_raw <- pharmaverseraw::ds_raw

# ---- Prepare Study Controlled Terminology (CT) ----
# If I can’t access the CT from Pharmaverse GitHub, I recreate it manually here.
# This data frame maps collected values in ds_raw to CDISC‑coded terms and preferred terms.
study_ct <- data.frame(
  stringsAsFactors = FALSE,
  codelist_code = c("C66727","C66727","C66727","C66727","C66727",
                    "C66727","C66727","C66727","C66727","C66727"),
  term_code = c("C41331","C25250","C28554","C48226","C48227",
                "C48250","C142185","C49628","C49632","C49634"),
  term_value = c("ADVERSE EVENT","COMPLETED","DEATH","LACK OF EFFICACY",
                 "LOST TO FOLLOW-UP","PHYSICIAN DECISION","PROTOCOL VIOLATION",
                 "SCREEN FAILURE","STUDY TERMINATED BY SPONSOR",
                 "WITHDRAWAL BY SUBJECT"),
  collected_value = c("Adverse Event","Complete","Dead",
                      "Lack of Efficacy","Lost To Follow-Up",
                      "Physician Decision","Protocol Violation",
                      "Trial Screen Failure","Study Terminated By Sponsor",
                      "Withdrawal by Subject"),
  term_preferred_term = c(
    "AE","Completed","Died",NA,NA,NA,"Violation",
    "Failure to Meet Inclusion/Exclusion Criteria",NA,"Dropout"
  ),
  term_synonyms = c("ADVERSE EVENT","COMPLETE","Death",NA,NA,NA,NA,NA,NA,
                    "Discontinued Participation")
)

# ---- Create DS Domain ----
# Build DS domain from raw data and controlled terminology
ds_domain <- ds_raw %>%
  # Join controlled terminology based on the collected term
  left_join(study_ct, by = c("IT.DSTERM" = "collected_value")) %>%
  mutate(
    STUDYID = STUDY,                     # Map STUDY to SDTM STUDYID
    DOMAIN = "DS",
    USUBJID = paste0(STUDY, "-", PATNUM), # Create unique subject ID
    DSSEQ = dplyr::row_number(),
    DSTERM = IT.DSTERM,                 # Original collected term
    DSDECOD = ifelse(!is.na(term_value), term_value, IT.DSDECOD),
    DSCAT = "DISPOSITION EVENT",
    VISIT = INSTANCE,                   # INSTANCE often represents visit
    VISITNUM = dplyr::dense_rank(INSTANCE),
    DSDTC = as.Date(IT.DSSTDAT, format = "%d-%m-%Y"),
    DSSTDTC = DSDTC,
    DSSTDY = NA                         # placeholder; would require study day calc
  ) %>%
  select(all_of(c(
    "STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD",
    "DSCAT","VISITNUM","VISIT","DSDTC","DSSTDTC","DSSTDY"
  )))
glimpse(ds_domain)

# ---- Validate Variables ----
# Here I confirm that the required SDTM DS variables exist in my dataset.
required_vars <- c("STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD",
                   "DSCAT","VISITNUM","VISIT","DSDTC","DSSTDTC","DSSTDY")

missing_vars <- setdiff(required_vars, names(ds_domain))

if (length(missing_vars) > 0) {
  warning("Some expected variables are missing: ",
          paste(missing_vars, collapse = ", "))
} else {
  message("All expected DS variables are present.")
}

# ---- Review Output ----
# I use glimpse() to visually confirm that the data types and sample records look correct.
glimpse(ds_domain)

# ---- Save Output (Optional) ----
# I save the final DS domain to a CSV file so it can be reviewed or included in my submission.
write.csv(ds_domain, "question_2_sdtm/ds_domain_output.csv", row.names = FALSE)

# ---- Summary of Logic ----
# - Step 1: Load raw data from {pharmaverseraw}
# - Step 2: Define and use the study controlled terminology (study_ct)
# - Step 3: Join raw data to CT on IT.DSTERM to standardize terms
# - Step 4: Derive required SDTM variables (STUDYID, DOMAIN, USUBJID, etc.)
# - Step 5: Validate that all required DS fields are present
# - Step 6: Inspect, then save the output for review
###############################################################################

