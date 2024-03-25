# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# Incidence function
# ============================================================================ #
# This function computes the incidence rate based on provided categories in a 
# data set. 
# By default it calculates the incidence rate defined by the number of incident 
# isolates divided by 1,000 admissions. There is an option to choose patient 
# days as the denominator instead. 
# ============================================================================ #
df.inc.function <- function(df,                               # Input data set
                            columns = c("organismofinterest", # Columns to be used for incidence calculation
                                        "onset", 
                                        "date_year"), 
                            n_hosp,                           # Number of hospitalizations
                            patient_time = NULL,              # Data frame with patient days
                            y = "n_inc"                       # 
                            ){
  library(dplyr)
  # column names of input data set
  df_names <- names(df %>% ungroup() %>% select(all_of(columns)))
  # Section 1: Incidence rate based on number of admissions
  if(is.null(patient_time)){
    n_hosp_names <- names(n_hosp)
    col_group <- intersect(df_names, n_hosp_names)
    # Compute number of hospitalization based on categories provided by input
    n_hosp_grp <- n_hosp %>% 
      group_by(across(all_of(col_group))) %>% 
      summarize(n_adm=sum(hospitalizations))
    # Final data frame with incidence rate
    result <- unique(left_join(df %>% 
                                 ungroup() %>% 
                                 group_by(across(all_of(columns))) %>% 
                                 summarize(n_inc = sum(eval(parse(text=y)))), 
                               n_hosp_grp, by = col_group))%>% 
      mutate(inc=n_inc*1000/n_adm, prop_inc = n_inc/n_adm)
  }else{ # Section 2: Incidence rate based on patient days
    pt_names <- names(patient_time)
    col_group <- intersect(df_names, pt_names)
    # Compute number of patient days based on categories provided by input
    patient_time_grp <- patient_time %>% 
      group_by(across(all_of(col_group))) %>% 
      summarize(patient_days=sum(patient_days))
    # Final data frame with incidence rate
    result <- unique(left_join(df %>% 
                                 ungroup() %>% 
                                 group_by(across(all_of(columns))) %>% 
                                 summarize(n_inc = sum(eval(parse(text=y)))), 
                               patient_time, by = col_group))%>% 
      mutate(inc=n_inc*1000/patient_days)
  }
  
  return(result)
}

# ============================================================================ #
# Function using purrr to compute list of incidences
# This function applies df.inc.function to a list of category vectors 
# The result is a list of data frames wth computed incidence rates
# ============================================================================ #
df.inc.function.map <- function(df, 
                                baseline, 
                                categories, # Compute incidence for each of the categories
                                list_names, 
                                n_hosp, 
                                patient_time = NULL, 
                                y="n_inc"){
  col_list <- lapply(categories, function(x) c(baseline, x))
  col_list <- lapply(col_list, function(x) stringi::stri_remove_empty(x))
  
  res_list <- purrr::map(.x=col_list, 
                         ~ df.inc.function(df=df, columns=.x, n_hosp, patient_time, y=y))
  names(res_list) <- list_names
  return(res_list)
}