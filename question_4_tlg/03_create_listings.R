###############################################################################
# Author: Annabel Smith
# Title:  Question 4 - AE Listings
# Purpose:
#   Generate a detailed listing of all treatment-emergent adverse events
#   (TEAEs), sorted chronologically within each subject.
#   Output: HTML listing file (ae_listings.html)
###############################################################################

library(gt)              # for row-level table formatting and HTML export
library(dplyr)           # for data wrangling (filter, mutate, select, arrange)
library(pharmaverseadam) # for the ADAE and ADSL example clinical datasets

# ---- Load Data ---------------------------------------------------------------
# ADAE: one row per adverse event per subject
# ADSL: one row per subject, contains treatment arm assignment (ACTARM)
adae <- pharmaverseadam::adae
adsl  <- pharmaverseadam::adsl

# ---- Prepare Listing Data ----------------------------------------------------
ae_listing <- adae %>%
  
  # Step 1: Keep only treatment-emergent AEs
  # TRTEMFL == "Y" flags events that started after the first dose of treatment.
  # This is a regulatory requirement — we exclude pre-treatment events.
  filter(TRTEMFL == "Y") %>%
  
  # Step 2: Bring in the treatment arm from ADSL
  # ADAE already has ACTARM but it can be missing for some records.
  # We join from ADSL (the definitive subject-level dataset) to fill any gaps.
  # The join produces ACTARM.x (from ADAE) and ACTARM.y (from ADSL).
  left_join(adsl %>% select(USUBJID, ACTARM), by = "USUBJID") %>%
  
  # Step 3: Resolve the duplicate ACTARM columns
  # coalesce() takes the first non-NA value across ACTARM.x and ACTARM.y,
  # so we always get a treatment arm even if ADAE's copy was missing.
  mutate(ACTARM = coalesce(ACTARM.x, ACTARM.y)) %>%
  
  # Step 4: Sort rows for readability
  # Primary sort: subject ID (groups all events for the same patient together)
  # Secondary sort: AE start date (chronological order within each subject)
  arrange(USUBJID, AESTDTC) %>%
  
  # Step 5: Select and rename columns for the final listing
  # We rename to the full regulatory display labels matching the sample output.
  # Only these 7 columns are needed — all intermediate columns are dropped.
  select(
    "Unique Subject Identifier"              = USUBJID,
    "Description of Actual Arm"             = ACTARM,
    "Reported Term for the Adverse Event"   = AETERM,
    "Severity/Intensity"                    = AESEV,
    "Causality"                             = AEREL,
    "Start Date/Time of Adverse Event"      = AESTDTC,
    "End Date/Time of Adverse Event"        = AEENDTC
  )

# ---- Build the gt Table ------------------------------------------------------
# We use {gt} directly here rather than {gtsummary}.
# gtsummary::tbl_summary() is designed for aggregated summary statistics.
# Since this is a row-level listing (every AE gets its own row), gt() is
# the correct tool — it formats a data frame as a display table without
# collapsing or summarising any rows.
ae_list_tbl <- ae_listing %>%
  
  # Convert the data frame into a gt table object
  gt() %>%
  
  # Add a title and subtitle to match the sample output header
  tab_header(
    title    = "Listing of Treatment-Emergent Adverse Events by Subject",
    subtitle = "Excluding Screen Failure Patients"
  ) %>%
  
  # Style the body cells with a monospace font at a small size,
  # matching the typewriter appearance of the sample clinical listing
  tab_style(
    style     = cell_text(font = "Courier New", size = px(11)),
    locations = cells_body()
  ) %>%
  
  # Bold the column header labels so they stand out from the data rows
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  
  # Set explicit column widths so longer text columns (like AE term)
  # have more space and shorter columns (dates, severity) stay compact.
  # everything() catches any remaining columns not explicitly named.
  cols_width(
    "Unique Subject Identifier"            ~ px(140),
    "Description of Actual Arm"            ~ px(120),
    "Reported Term for the Adverse Event"  ~ px(220),
    everything()                           ~ px(100)
  ) %>%
  
  # Add alternating row shading to make it easier to read across long rows
  opt_row_striping()

# ---- Export ------------------------------------------------------------------
# Create the output folder if it doesn't already exist.
# showWarnings = FALSE suppresses the message if the folder is already there.
dir.create("question_4_tlg", showWarnings = FALSE)

# Save the gt table as a self-contained HTML file.
# gtsave() handles the HTML rendering automatically.
gtsave(ae_list_tbl, "question_4_tlg/ae_listings.html")