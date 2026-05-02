###############################################################################
# Author: Annabel Smith
# Title:  Visualisations of Treatment‑Emergent Adverse Events (TEAEs)
# Purpose:
#   1. Show counts of treatment‑emergent AEs by severity across treatment arms.
#   2. Show the top 10 most frequent TEAEs (subject‑level incidence with 95% CI).
###############################################################################

library(dplyr)
library(ggplot2)
library(pharmaverseadam)
library(scales)

# ---- Load and prepare the data ----
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Keep only treatment‑emergent AEs and attach treatment arm
adae_teae <- adae %>%
  filter(TRTEMFL == "Y") %>%
  left_join(adsl %>% select(USUBJID, ACTARM), by = "USUBJID") %>%
  mutate(ACTARM = coalesce(ACTARM.x, ACTARM.y))

###############################################################################
# 1. AE Severity Counts by Treatment Arm
###############################################################################
# Each bar shows the total number of AEs per treatment arm,
# with colours representing AE severity (Mild, Moderate, Severe).

p1 <- adae_teae %>%
  ggplot(aes(x = ACTARM, fill = AESEV)) +
  geom_bar(position = "stack") +
  labs(
    title = "AE Severity Distribution by Treatment",
    x = "Treatment Arm",
    y = "Count of AEs",
    fill = "Severity"
  ) +
  theme_minimal()

# Save bar chart
ggsave("question_4_tlg/ae_severity_plot.png", p1, width = 8, height = 5)

###############################################################################
# 2. Top 10 Treatment‑Emergent AEs (Subject‑Level Incidence + 95% CI)
###############################################################################

# Total number of subjects in the safety population
total_subjects <- n_distinct(adsl$USUBJID)

# Identify the 10 AE terms reported by the greatest number of subjects
top10_terms <- adae_teae %>%
  distinct(USUBJID, AETERM) %>%          # one record per subject per AE term
  count(AETERM, name = "n_subj") %>%
  arrange(desc(n_subj)) %>%
  slice_head(n = 10) %>%
  pull(AETERM)
# Compute ae_ci first
ae_ci <- adae_teae %>%
  filter(AETERM %in% top10_terms) %>%
  distinct(USUBJID, AETERM) %>%
  count(AETERM, name = "n_subj") %>%
  mutate(
    total = total_subjects,
    prop  = n_subj / total,
    lower = mapply(function(x, n) binom.test(x, n)$conf.int[1], n_subj, total),
    upper = mapply(function(x, n) binom.test(x, n)$conf.int[2], n_subj, total)
  )

# reorder — after all columns exist
ae_ci <- ae_ci %>%
  mutate(AETERM = factor(AETERM, levels = AETERM[order(prop)]))
## Prepare the plot
p2 <- ggplot(ae_ci, aes(x = prop, y = AETERM)) +
  geom_segment(
    aes(x = lower, xend = upper, y = AETERM, yend = AETERM),
    linewidth = 0.7,
    colour = "black"
  ) +
  geom_point(size = 3, shape = 21, fill = "black", colour = "black") +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.1))
  ) +
  labs(
    title    = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", total_subjects, " subjects; 95% Clopper-Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.background   = element_rect(fill = "grey95", colour = NA),
    plot.background    = element_rect(fill = "grey95", colour = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks.y       = element_blank(),
    plot.title         = element_text(face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(hjust = 0.5)
  )
# ---- Save output ----
ggsave("question_4_tlg/ae_top10_plot.png", p2, width = 8, height = 6, dpi = 300)
##########