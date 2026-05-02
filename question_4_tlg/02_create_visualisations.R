###############################################################################
# Author: Annabel Smith
# Title:  AE Severity Counts by Treatment Arm
# Purpose:
#   I want to show the actual number of treatment‑emergent AEs (TEAEs)
#   within each treatment arm, stacked by severity (Mild / Moderate / Severe).
###############################################################################

library(dplyr)
library(ggplot2)
library(pharmaverseadam)

# ---- Load and prepare the data ----
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Keep only treatment‑emergent AEs and attach treatment arm
adae_teae <- adae %>%
  filter(TRTEMFL == "Y") %>%
  left_join(adsl %>% select(USUBJID, ACTARM), by = "USUBJID") %>%
  mutate(ACTARM = coalesce(ACTARM.x, ACTARM.y))

# ---- Plot: actual AE counts by severity ----
# Each bar represents the number of AEs in that treatment arm,
# with colors showing how many were Mild, Moderate, or Severe.
p1 <- adae_teae %>%
  ggplot(aes(x = ACTARM, fill = AESEV)) +
  geom_bar(position = "stack") +
  labs(
    title = "AE severity distribution by treatment",
    x = "Treatment Arm",
    y = "Count of AEs",
    fill = "Severity"
  ) +
  theme_minimal()

# ---- Save output ----
ggsave("question_4_tlg/ae_severity_plot.png", p1, width = 8, height = 5)
###############################################################################
