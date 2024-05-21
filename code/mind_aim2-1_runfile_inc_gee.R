# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# Infection incidence regression runfile
# ============================================================================ #
# This file uses mind_aim2-1_function_inc_gee.R to run the GEE analysis for 
# organisms of interest. 
# Note that the code cannot be run as such since the underlying data is not 
# publicly available. 
# The code 
# 1. Prepare input and output data (lines 41ff.). 
# 2. Run GEE analysis for each organism (lines 59ff.). 
# 3. Compute multiplicative Average Annual Percentage Change (lines 113ff.). 
# 4. Save output data (lines 157ff.). 
#    (Output is an Excel sheet with mean values and 95% confidence intervals) 
# ============================================================================ #
setwd("/Users/tm-pham/academia/hsph/mind/publications/aim2-1/")
source("code/packages.R")
source("code/plotting_template.R")
source("code/mind_aim2-1_function_df_inc.R")
source("code/mind_aim2-1_function_inc_gee.R")
source("code/mind_aim2-1_function_aapc.R")

# Load data
load("data/bugs_drugs.RData")                  # List of organisms and corresponding antibiotic (classes)
# Note that the following three data sets are not publicly available 
load("data/micro_data_inc30d.RData")           # micro_data_inc30, microbiology data
load("data/mind_n_hosp.RData")                 # Number of hospital admissions data 
load("data/mind_fac_characteristics_yr.RData") # Facility characteristics per year

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

# ---------------------------------------------------------------------------- #
# Prepare input data 
# Scale numeric variables
micro_data_inc30 <- micro_data_inc30 %>%
  mutate_at(vars(starts_with("perc_")), ~scale(.*100, scale = F)) %>%
  mutate_at(vars(starts_with("median_")), ~scale(., scale = F))

# Onset filter
onset_filter <- micro_data_inc30$onset2 %>% unique()

# Minimum and maximum date for analysis
min_date <- min(micro_data_inc30$date_year)
max_year <- 2022

# Prepare output data
inc_gee_models_org <- list()
df_gee_results <- NULL

# ---------------------------------------------------------------------------- #
# 2. Run GEE analysis for each organism
for(i in 1:length(bugs)){
  bug <- bugs[i]
  # Overall
  inc_gee_models_org[[bug]] <- inc.gee.regression(
    df = micro_data_inc30 %>%
      filter(organismofinterest==bug, date_year<=max_year) %>%
      mutate(sta6a = factor(sta6a),
             time = as.numeric(date_year - min_date),
             time_1 = as.numeric(time*(1-pandemic_year)),
             time_2 = as.numeric(ifelse(pandemic_year==1, time-max(time_1), 0))),
    df_hosp = n_hosp, 
    org = bug, 
    # Fully stratified model
    model_formula = "n_inc ~ time_1 + time_2 + pandemic_year*onset2 + time_1*onset2 + time_2*onset2 +
  census_region*onset2 + facility_rurality*onset2 + complexitylevel*onset2 +
  median_los*onset2 + perc_female*onset2 + perc_white*onset2 + perc_nonhisp*onset2 +
  perc90above*onset2 + perc70_89*onset2 + perc50_69*onset2 + perc30_49*onset2 +
  maxPD*onset2 + fac_icu_at_admission*onset2 +
  fac_mi*onset2 + fac_chf*onset2 + fac_pvd*onset2 + fac_cvd*onset2 + fac_dem*onset2 + fac_cpd*onset2 + fac_rheum*onset2 +
  fac_pud*onset2 + fac_mliverd*onset2 + fac_diabnc*onset2 + fac_diabc*onset2 + fac_hpplegia*onset2 + fac_renald*onset2 +
  fac_cancer*onset2 + fac_msliver*onset2 + fac_mcancer*onset2 + fac_aids*onset2 +
  offset(log(n_adm/1000))",
    # Partially stratified model
    # model_formula = "n_inc ~ time_1 + time_2 + pandemic_year*onset2 + time_1*onset2 + time_2*onset2 +
    # census_region + facility_rurality + complexitylevel +
    # median_los + perc_female + perc_white + perc_nonhisp +
    # perc90above + perc70_89 + perc50_69 + perc30_49 +
    # maxPD + fac_icu_at_admission +
    # fac_mi + fac_chf + fac_pvd + fac_cvd + fac_dem + fac_cpd + fac_rheum +
    # fac_pud + fac_mliverd + fac_diabnc + fac_diabc + fac_hpplegia + fac_renald +
    # fac_cancer + fac_msliver + fac_mcancer + fac_aids +
    # offset(log(n_adm/1000))",
    categs = c("date_year",
               "time",
               "time_1",
               "time_2",
               "onset2",
               "pandemic_year",
               "sta6a"),
    arrange_vars = c("time", "id"), 
    fac_data = fac_data_yr %>% 
      select(date_year, sta6a, all_of(fac_vars)) %>% 
      ungroup() %>% 
      group_by(date_year, sta6a) %>% 
      arrange(sta6a, date_year) %>% 
      filter(row_number()==1), 
    id.var = "sta6a", 
    package = "geeasy",
    gee_corstr = "ar1")
}

# ---------------------------------------------------------------------------- #
# 3. Average annual change (%)
df_gee_results <- NULL
for(bug in bugs_ordered){
  (overall_aapc <- aapc.function(inc_gee_models_org[[bug]], specs = "~time_1", var="time_1",
                                 quarter = F))
  df_gee_results <- rbind(df_gee_results, cbind(org = bug, 
                                                variable = "pre-pandemic", 
                                                onset="overall", 
                                                overall_aapc))
  (overall_pandemic_aapc <- aapc.function(inc_gee_models_org[[bug]], specs = "~time_2", var="time_2", 
                                          quarter=F))
  df_gee_results <- rbind(df_gee_results, cbind(org = bug, 
                                                variable = "pandemic", 
                                                onset="overall", 
                                                overall_pandemic_aapc))
  (onset_aapc <- aapc.function(inc_gee_models_org[[bug]], specs = "~onset2", var="time_1", 
                               quarter=F))
  df_gee_results <- rbind(df_gee_results, cbind(org = rep(bug, 2), 
                                                variable = "pre-pandemic", 
                                                onset=c("Community-onset", "Hospital-associated"), 
                                                onset_aapc))
  
  (p_onset_aapc <- aapc.function(inc_gee_models_org[[bug]], specs = "~onset2", var="time_2", 
                                 quarter = F))
  df_gee_results <- rbind(df_gee_results, cbind(org = rep(bug, 2), 
                                                variable="pandemic", 
                                                onset=c("Community-onset", "Hospital-associated"), 
                                                p_onset_aapc))
}

df_gee_results$p_value <- round(df_gee_results$p_value, 4)

# Create AAPC output table
output_table <- cbind(df_gee_results, text = apply(df_gee_results, 1, 
                                                   function(x) paste0(x[4], "% (", x[5], ", ", x[6], ")"))) %>% 
  mutate(org = factor(org, levels=bugs_ordered), 
         p_value = round(p_value, 4)) %>% 
  arrange(org)

for(i in 1:nrow(output_table)){
  if(output_table[i, "asymp.LCL"]<=0 & 0<=output_table[i, "asymp.UCL"]){
    output_table[i, "text.star"] <- output_table[i, "time.trend"]
  }else{
    if(output_table[i, "p_value"]>0.000){
      output_table[i, "text"] <- paste0(output_table[i, "text"], "* p-value = ", output_table[i, "p_value"])
    }else{
      output_table[i, "text"] <- paste0(output_table[i, "text"], "*")
    }

  }
}

View(output_table %>% 
  select(org, variable, onset, text) %>% 
  pivot_wider(id_cols = c(org, variable), values_from = text, names_from = onset) %>% 
  mutate(variable = factor(variable, levels = c("pre-pandemic", "pandemic"))))


# ---------------------------------------------------------------------------- #
# 4. Save Data
# Save as Excel sheet
write.xlsx(output_table, file = "results/mind_inc_gee_results_by_org_onset_geeasy_year_May2024.xlsx", row.names=F, col.names=T)

# Save as RData 
save(inc_gee_models_org, file="data/mind_inc_gee_models_orgs_ar1_geeasy_year.RData")
save(output_table, file="data/mind_inc_gee_results_by_org_onset_geeasy_year.RData")