setwd("P:/ORD_Samore_202109019D/Mui/")
source("code/packages.R")
source("code/plotting_template.R")
source("code/mind_colors.R")
source("code/tmp_mind_drug_subset.R")
source("code/mind_aim2-1_function_df_inc.R")
source("code/mind_aim2-1_function_inc_gee.R")
source("code/mind_aim2-1_function_aapc.R")
source("code/mind_aim2-1_function_res_prop_gee.R")

# Load data
load("data/bugs_drugs.RData")
load("data/mind_drug_subset.RData")
load("data/micro_data_inc30d.RData")
load("data/mind_n_hosp.RData")
load("data/mind_fac_characteristics_yr.RData")

# Onset filter
onset_filter <- micro_data_inc30$onset2 %>% unique()

micro_data_inc30 <- micro_data_inc30 %>%
  mutate_at(vars(starts_with("perc_")), ~scale(.*100, scale = F)) %>%
  mutate_at(vars(starts_with("median_")), ~scale(., scale = F)) 

min_date <- min(micro_data_inc30$date_year)
max_year <- 2022

res_prop_gee_models <- list()
for(i in 1:length(bugs)){
  bug <- bugs[i]
  org_short <- bugs_short[i]
  cat(bug, "\n")
  res_prop_gee_models[[bug]] <- map(
    .x = drug_subset[[bug]], 
    .f = ~prop.gee.regression(
      df = micro_data_inc30 %>%
        filter(organismofinterest==bug, date_year<=max_year) %>%
        mutate(sta6a = factor(sta6a),
               time = as.numeric(date_year - min_date),
               time_1 = as.numeric(time*(1-pandemic_year)),
               time_2 = as.numeric(ifelse(pandemic_year==1, time-max(time_1), 0))),
      org = bug, 
      model_formula = "n ~ time_1  + time_2 + pandemic_year*onset2 + time_1*onset2 + time_2*onset2 +
  census_region*onset2 + facility_rurality*onset2 + complexitylevel*onset2 +
  median_los*onset2 + perc_female*onset2 + perc_white*onset2 + perc_nonhisp*onset2 +
  perc90above*onset2 + perc70_89*onset2 + perc50_69*onset2 + perc30_49*onset2 +
  maxPD*onset2 + fac_icu_at_admission*onset2 +
  fac_mi*onset2 + fac_chf*onset2 + fac_pvd*onset2 + fac_cvd*onset2 + fac_dem*onset2 + fac_cpd*onset2 + fac_rheum*onset2 +
  fac_pud*onset2 + fac_mliverd*onset2 + fac_diabnc*onset2 + fac_diabc*onset2 + fac_hpplegia*onset2 + fac_renald*onset2 +
  fac_cancer*onset2 + fac_msliver*onset2 + fac_mcancer*onset2 + fac_aids*onset2 +
  offset(log(n_total))",  
      drug = .x,
      categs = c("date_year",
                 "time",
                 "time_1",
                 "time_2",
                 "onset2",
                 "pandemic_year",
                 "sta6a"), 
      arrange_vars =  c("time_1", "id"), 
      fac_data = fac_data_yr %>% 
        select(date_year, sta6a, all_of(fac_vars)) %>% 
        ungroup() %>% 
        group_by(date_year, sta6a) %>% 
        arrange(sta6a, date_year) %>% 
        filter(row_number()==1), 
      id.var = "sta6a", 
      gee_corstr = "ar1", 
      package = "geeasy", 
      restrict_fac=1))
  names(res_prop_gee_models[[bug]]) <- drug_subset[[bug]]
  # res_prop_gee_model <- res_prop_gee_models[[bug]]
  # save(res_prop_gee_model,
  #      file=paste0("data/mind_", org_short,"_res_prop_gee_models_geeasy_year.RData"))
}

save(res_prop_gee_models, file="data/mind_res_prop_gee_models_geeasy_year.RData")


load("/Users/tm-pham/academia/hsph/mind/data/aim2-1/resistance_proportion/mind_res_prop_gee_models_geeasy_year.RData")
df_res_prop_results <- list()
for(i in 1:length(bugs)){
  bug <- bugs[i]
  org_short <- bugs_short[i]
  # load(paste0("data/mind_", org_short,"_res_prop_gee_models_", gee_corstr_vec[i], ".RData"))
  res_prop_gee_model <- res_prop_gee_models[[bug]]
  drugs <- names(res_prop_gee_model)
  for(j in 1:length(drugs)){
    drug <- drugs[j]
    cat(drug, "\n")
    error_occurred <- FALSE
    tryCatch({prepandemic_aapc <- aapc.function(res_prop_gee_model[[drug]], specs = "~time_1", var="time_1", quarter=F)}, 
             error = function(e){error_occurred <<- TRUE
             message(paste("Error occurred:", e))})
    tryCatch({pandemic_aapc <- aapc.function(res_prop_gee_model[[drug]], specs = "~time_2", var="time_2", quarter=F)}, 
             error = function(e){error_occurred <<- TRUE
             message(paste("Error occurred:", e))})
    tryCatch({pp_onset_aapc <- aapc.function(res_prop_gee_model[[drug]], specs = "~onset2", var="time_1", quarter=F)},
             error = function(e){error_occurred <<- TRUE
             message(paste("Error occurred:", e))})
    tryCatch({p_onset_aapc <- aapc.function(res_prop_gee_model[[drug]], specs = "~onset2", var="time_2", quarter=F)}, 
             error = function(e){error_occurred <<- TRUE
             message(paste("Error occurred:", e))})
    if(!error_occurred){
      df_res_prop_results[[drug]] <- rbind(df_res_prop_results[[drug]],
                                           cbind(organismofinterest=bug, time_period = "pre-pandemic", variable = "overall",
                                                 prepandemic_aapc, gee_corstr = res_prop_gee_model[[drug]]$corstr),
                                           cbind(organismofinterest=bug, time_period = "pandemic", variable = "overall",
                                                 pandemic_aapc, gee_corstr = res_prop_gee_model[[drug]]$corstr),
                                           cbind(organismofinterest=rep(bug, 2), time_period =rep("pre-pandemic", 2),
                                                 variable=c("Community-onset", "Hospital-associated"), pp_onset_aapc, 
                                                 gee_corstr = rep(res_prop_gee_model[[drug]]$corstr,2)),
                                           cbind(organismofinterest=rep(bug, 2), time_period =rep("pandemic", 2),
                                                 variable=c("Community-onset", "Hospital-associated"), p_onset_aapc, 
                                                 gee_corstr = rep(res_prop_gee_model[[drug]]$corstr,2)))
    }
  }
}

# Create excel sheet
library(openxlsx)
wb_prop=createWorkbook()
for(drug in unique(unlist(drug_subset))){
  output_table <- cbind(df_res_prop_results[[drug]], 
                        text = apply(df_res_prop_results[[drug]], 1, 
                                     function(x) paste0(x[4], "% (", x[5], ", ", x[6], ")"))) %>% 
    mutate(organismofinterest = factor(organismofinterest, levels=bugs_ordered), 
           p_value = round(p_value, 4), 
           time_period = factor(time_period, levels = c("pre-pandemic", "pandemic")), 
           variable = factor(variable, levels = c("overall", "Community-onset", "Hospital-associated"))) %>% 
    arrange(organismofinterest, variable, time_period)
  
  for(i in 1:nrow(output_table)){
    if(output_table[i, "asymp.LCL"]<=0 & 0<=output_table[i, "asymp.UCL"]){
      output_table[i, "text.star"] <- output_table[i, "time.trend"]
    }else{
      if(output_table[i, "p_value"]>0.000){
        output_table[i, "text"] <- paste0(output_table[i, "text"], " p-value = ", output_table[i, "p_value"])
      }else{
        output_table[i, "text"] <- paste0(output_table[i, "text"], "*")
      }
    }
  }
  
  sheet_name <- paste0(str_trunc(drug,10))
  addWorksheet(wb_prop, sheet_name)
  writeData(wb_prop, sheet_name, output_table)
}
saveWorkbook(wb_prop, "results/mind_manuscript_tab_res_prop_drug_bug_geeasy_year_May2024.xlsx")
