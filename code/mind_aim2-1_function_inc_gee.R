# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# Infection incidence regression function
# ============================================================================ #
# This function performs a Generalized Estimating Equation (GEE) incidence time 
# trend analysis. 
# 
# It can be applied to incidence (count) data and we assumed a Poisson 
# distribution with a log-link for the outcome variable. This function was 
# developed with two examples of outcome variables in mind: infection incidence 
# and incidence of resistant isolates.
# The function can roughly be divided into 2 sections:
# Section 1 (line 42-141): Code for infection incidence
# Section 2 (line 144-205): Code for resistance incidence 
# Each section performs roughly the following steps:
# 1. Compute the incidence based on the categories provided by the input user 
# 2. Add information on facility characteristics (provided by user)
# 3. Add the number of hospitalizations (used as offset in GEE analysis)
# 4. Run the GEE model
# 5. Output: return the GEE model
#
# Notes:
# We generally assumed clustering by facility, though another cluster is 
# possible by changing "id.var". 
# 
# Two packages can be used for GEE analysis: geeasy and geepack
# - geeasy is much faster than geepack for big data sets
# - geepack is more known and there may be more functions for post-hoc analyses 
# available
# ============================================================================ #
setwd("P:/ORD_Samore_202109019D/Mui/")
source("code/mind_aim2-1_function_df_inc.R")

# ---------------------------------------------------------------------------- #
# GEE regression function
inc.gee.regression <- function(
    df,                             # Input data set
    df_hosp = NULL,                 # Number of hospitalization, used for offset
    org = NULL,                     # Organism of interest 
    model_formula,                  # GEE model formula
    drug = NULL,                    # Antibiotic if resistance incidence is outcome
    categs = c("date_year",         # Categories by which incidence is summarized
               "sta6a"),
    arrange_vars = c("id", "time"), # Variables to arrange data frame
    fac_data = NULL,                # Data frame with facility characteristics
    id.var = "sta6a",               # Variable used as cluster in GEE
    gee_corstr = "ar1",             # Correlation structure for GEE
    package = "geeasy"              # Package for GEE analysis, two options: geepack or geeasy 
){
  
  # Prepare data frame for input to GEE
  cat("Preparing incidence data frame for regression.\n")
  
  # -------------------------------------------------------------------------- #
  # Section 1: GEE for infection incidence
  if(is.null(drug)){
    if(is.null(org)){
      # Restrict input data to facilities with data for at least 'n_years' years
      n_years <- length(unique(df$date_year))
      fac_years <- df %>% 
        select(sta6a, date_year) %>% 
        unique() %>% 
        group_by(sta6a) %>% 
        summarize(n=n(), 
                  include=ifelse(n==n_years, 1, 0))
      fac_ids <- fac_years %>% filter(include==1) %>% select(sta6a) %>% unlist() %>% unname()
      df <- df %>% filter(sta6a%in%fac_ids)
    }
    
    # Show the number of clusters (facilities) used in GEE analysis
    cat("Number of facilities: ", length(unique(df$sta6a)), "\n")
    
    # Compute number of 30-day incident isolates according to input categories
    df_inc <- unique(df %>% 
                       ungroup() %>%
                       group_by(across(all_of(c(categs)))) %>%
                       summarize(n_inc = sum(incident30d)))
    
    # Input data frame for GEE analysis
    # Compute incidence rate (number of incident isolates divided by number of 
    # hospitalizations)
    # Function from: tmp_mind_df_inc_function.R
    df_regr <- df.inc.function(df_inc, 
                               columns = categs, 
                               n_hosp = df_hosp, 
                               patient_time = NULL, 
                               y="n_inc")
    
    # If facility data is provided, add respective facility characteristics to
    # df_regr
    if(!is.null(fac_data)){
      join_cols <- intersect(colnames(df_regr), colnames(fac_data))
      df_regr <- left_join(df_regr, fac_data) %>% unique()
    }
    
    # Final input data set for GEE analysis
    df_regr <- df_regr %>% 
      mutate(id = factor(eval(parse(text=id.var)))) %>% 
      filter(!is.na(id)) %>%
      ungroup() %>% 
      na.omit() %>% 
      unique() %>% 
      arrange(eval(parse(text=(arrange_vars))))

    # GEE analysis
    if(package == "geepack"){
      cat("Running GEE analysis using geepack package.\n")
      gee_model <- geepack::geeglm(formula = as.formula(model_formula),
                                   data = df_regr,
                                   id = id,
                                   family = poisson(link="log"),
                                   corstr = gee_corstr)
      qic_gee <- geepack::QIC(gee_model)
    }else{
      cat("Running GEE analysis using geeasy package.\n")
      tryCatch({gee_model <- geeasy::geelm(formula = as.formula(model_formula), 
                                           data = as.data.frame(df_regr), 
                                           id = id, 
                                           family = poisson(link="log"), 
                                           corstr = gee_corstr)}, 
               # Error and warning handling
               # If error or warning when using more complex correlation 
               # structure, use independence correlation structure
               error = function(e){message(paste("An Error occurred:", e))
                 message("Running GEE analysis with independence structure.")
                 gee_model<<- geeasy::geelm(formula = as.formula(model_formula), 
                                            data = as.data.frame(df_regr), 
                                            id = id, 
                                            family = poisson(link="log"), 
                                            corstr = "independence")}, 
               warning = function(w) {message(paste("An Error occurred:", w))
                 message("Running GEE analysis with independence structure.")
                 gee_model<<- geeasy::geelm(formula = as.formula(model_formula), 
                                            data = as.data.frame(df_regr), 
                                            id = id, 
                                            family = poisson(link="log"), 
                                            corstr = "independence")})
      cat(geepack::QIC(gee_model))
    }
    
  # -------------------------------------------------------------------------- #
  # Section 2: GEE for resistance incidence
  }else{ 
    cat(drug,"\n")
    df <- df %>% mutate_at(drug, ~ifelse(.=="I", "R", .x))
    susc_categs <- df[,drug] %>% unlist() %>% unique()
    # Determine the columns that are in common
    join_vars <- intersect(colnames(df_hosp), categs)
    
    # Input data frame for GEE analysis
    df_regr <- df %>% 
      ungroup() %>% 
      filter(organismofinterest == org) %>% 
      select(all_of(c(categs, drug))) %>% 
      group_by_at(c(categs, drug)) %>% 
      reframe(n_inc = n(), 
              id = factor(eval(parse(text=id.var)))) %>% 
      unique() %>% 
      left_join(df_hosp %>% 
                  group_by_at(join_vars) %>% 
                  summarize(n_adm = sum(hospitalizations)), by = join_vars) %>% 
      mutate(inc = 1000*n_inc/n_adm) %>% 
      na.omit() %>% 
      unique()
    
    # If facility data is provided, add respective facility characteristics to
    # df_regr
    if(!is.null(fac_data)){
      join_cols <- intersect(colnames(df_regr), colnames(fac_data))
      df_regr <- left_join(df_regr, fac_data) %>% unique()
    }
    
    # Final input data frame for GEE analysis
    df_regr <- df_regr %>% 
      arrange(eval(parse(text=(arrange_vars)))) %>% 
      na.omit() %>% 
      unique()
    
    # GEE analysis
    cat("Running GEE analysis.\n")
    gee_model <- list()
    for(susc in setdiff(susc_categs, "N")){
      cat(paste(drug, susc,"\n"))
      tryCatch({gee_model[[susc]] <- geeasy::geelm(formula = as.formula(model_formula), 
                                                   data = as.data.frame(df_regr %>% filter_at(drug, ~.==susc)), 
                                                   id = id, 
                                                   family = poisson(link="log"), 
                                                   corstr = gee_corstr)}, 
               # Error and warning handling
               # If error or warning when using more complex correlation 
               # structure, use independence correlation structure
               error = function(e){message(paste("An error occurred:", e))
                 message("Running GEE analysis with independence structure.")
                 gee_model[[susc]] <<- geeasy::geelm(formula = as.formula(model_formula), 
                                                     data = as.data.frame(df_regr %>% filter_at(drug, ~.==susc)), 
                                                     id = id, 
                                                     family = poisson(link="log"), 
                                                     corstr = "independence")}, 
               warning = function(w) {message(paste("An warning occurred:", w))
                 message("Running GEE analysis with independence structure.")
                 gee_model[[susc]] <<- geeasy::geelm(formula = as.formula(model_formula), 
                                                     data = as.data.frame(df_regr %>% filter_at(drug, ~.==susc)), 
                                                     id = id, 
                                                     family = poisson(link="log"), 
                                                     corstr = "independence")})
      
    }
    
  }
  return(gee_model)
}
