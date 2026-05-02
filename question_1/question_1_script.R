install.packages("devtools")
library(devtools)

# Setting working directory
setwd("/cloud/project")

# Create parent folder structure
dir.create("question_1/descriptive_stats", recursive = TRUE)

# Create the package
library(usethis)
create_package("/cloud/project/question_1/descriptive_stats/descriptiveStats")

