# ============================================================================ #
# MInD Aim 2.1
# Title: Race and ethnicity tables for supplement
# Author: Thi Mui Pham, tmpham@hsph.harvard.edu
# ---------------------------------------------------------------------------- #
# Description:
# Make tables for supplement that contain the absolute numbers and percentages
# for the race and ethnicity categories. 
# ============================================================================ #
# Working directory
setwd("/Users/tm-pham/academia/hsph/mind/")

# Load packges
source("code/packages.R")

# Organisms of interest
bugs_ordered <- c("Staphylococcus aureus", 
                  "Enterococcus faecalis", 
                  "Enterococcus faecium", 
                  "Escherichia coli", 
                  "Klebsiella pneumoniae", 
                  "Enterobacter cloacae", 
                  "Serratia marcescens", 
                  "Pseudomonas aeruginosa", 
                  "Acinetobacter Sp.")


# Load data 
df_race <- read.csv("publications/aim2-1/data/descriptives/mind_aim2-1_race_descriptives_table.csv")
df_ethnicity <- read.csv("publications/aim2-1/data/descriptives/mind_aim2-1_ethnicity_descriptives_table.csv")

# ---------------------------------------------------------------------------- #
# Race
# ---------------------------------------------------------------------------- #
(df_race_formatted <- df_race %>% 
    select(organismofinterest, race, n_formatted) %>% 
    mutate(race = factor(race, levels = c("white", 
                                          "black or african american", 
                                          "american indian or alaska native", 
                                          "asian", 
                                          "native hawaiian or other pacific islander", 
                                          "mixed", 
                                          "missing")), 
           organismofinterest = factor(organismofinterest, levels = bugs_ordered)) %>% 
    arrange(organismofinterest, race) %>% 
    pivot_wider(id_cols = "organismofinterest", 
                values_from = "n_formatted",
                names_from = "race")) 
write.csv(df_race_formatted, file = "publications//aim2-1//data/descriptives/mind_aim2-1_race_descriptives_table_S3.csv", 
          row.names = F)

# ---------------------------------------------------------------------------- #
# Ethnicity
# ---------------------------------------------------------------------------- #
unique(df_ethnicity$ethnicity)
df_ethnicity$ethnicity <- str_to_title(df_ethnicity$ethnicity)

# Note: Merge 'Conflict' category into 'Missing' 
df_ethnicity <- df_ethnicity %>% 
  mutate(ethnicity = ifelse(ethnicity=="Conflict", "Missing", ethnicity)) %>% 
  group_by(organismofinterest) %>% 
  mutate(n_total = sum(n)) %>%
  group_by(organismofinterest, ethnicity) %>% 
  mutate(n = sum(n), 
         perc = round(100*n/n_total, 1), 
         n_formatted = paste0(n, " (",perc, "%)")) %>% 
  unique()

(df_ethnicity_formatted <- df_ethnicity %>% 
    select(organismofinterest, ethnicity, n_formatted) %>% 
    mutate(ethnicity = factor(ethnicity, levels = c("Not Hispanic Or Latino", 
                                                    "Hispanic Or Latino", 
                                                    "Missing")), 
           organismofinterest = factor(organismofinterest, levels = bugs_ordered)) %>% 
    arrange(organismofinterest, ethnicity) %>% 
    pivot_wider(id_cols = "organismofinterest", 
                values_from = "n_formatted",
                names_from = "ethnicity")) 
write.csv(df_ethnicity_formatted, file = "publications//aim2-1//data/descriptives/mind_aim2-1_ethnicity_descriptives_table_S3.csv", 
          row.names = F)
