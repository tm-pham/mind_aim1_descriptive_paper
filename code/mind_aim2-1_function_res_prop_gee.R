# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# Infection incidence regression function
# ============================================================================ #
# Function to compute GEE incidence trend analysis
source("code/mind_aim2-1_function_df_inc.R")

prop.gee.regression <- function(
    df, # microbiology data set
    df_hosp = NULL, # for offset
    org, 
    model_formula, 
    drug = NULL, 
    categs = c("date_year", 
               "sta6a"), 
    arrange_vars = c("id", "time"),
    fac_data = NULL, 
    id.var = "sta6a", 
    gee_corstr = "ar1", 
    package = "geeasy", 
    restrict_fac = 5 
){
  
  # -------------------------------------------------------------------------- #
  # Prepare data frame
  cat("Preparing incidence data frame for regression.\n")
  cat(drug,"\n")
  df <- df %>% mutate_at(drug, ~ifelse(.=="I", "R", .x))
  susc_categs <- df[,drug] %>% unlist() %>% unique()
  
  if(restrict_fac>0){ 
    n_years <- length(unique(df$date_year))
    
    fac_years <- df %>% 
      select(sta6a, date_year) %>% 
      unique() %>% 
      group_by(sta6a) %>% 
      summarize(n=n(), 
                include=ifelse(n>=restrict_fac, 1, 0))
    
    fac_ids <- fac_years %>% filter(include==1) %>% select(sta6a) %>% unlist() %>% unname()
    cat("Number of facilities: ", length(fac_ids), "\n")
    df <- df %>% filter(sta6a%in%fac_ids)
  }
  
  df_regr <- df %>% 
    ungroup() %>% 
    filter(organismofinterest == org) %>% 
    mutate_at(drug,  ~factor(., levels=c("N", "S", "R"))) %>%
    select(all_of(c(categs, drug))) %>%
    group_by_at(c(categs, drug), .drop=F) %>% 
    count() %>%
    unique() %>% 
    ungroup() %>% 
    group_by_at(categs, .drop=F) %>% 
    mutate(n_total = sum(n, na.rm = F)) %>% 
    ungroup() %>% 
    group_by_at(c(categs, drug), .drop=F) %>% 
    mutate(prop=n/n_total,
            n_total = n_total,
            id = factor(eval(parse(text=id.var)))) %>%
    na.omit() %>%
    unique() %>% 
    arrange(id, time) %>%
    filter_at(drug, ~.=="R")

  
  if(!is.null(fac_data)){
    join_cols <- intersect(colnames(df_regr), colnames(fac_data))
    df_regr <- left_join(df_regr, fac_data) %>% unique()
  }
  
  df_regr <- df_regr %>% 
    mutate(id = factor(eval(parse(text=id.var)))) %>% 
    ungroup() %>% 
    na.omit() %>% 
    unique() %>% 
    arrange(eval(parse(text=(arrange_vars))))
  
  # -------------------------------------------------------------------------- #
  # GEE analysis
  
  if(package == "geepack"){
    cat("Running GEE analysis using geepack package.\n")
    gee_model <- geepack::geeglm(formula = as.formula(model_formula),
                                 data = df_regr,
                                 id = id,
                                 family = poisson(link=log),
                                 corstr = "independence")
  }else{
    cat("Running GEE analysis using geeasy package.\n")
    tryCatch({gee_model <- geeasy::geelm(formula = as.formula(model_formula), 
                                         data = as.data.frame(df_regr), 
                                         id = id, 
                                         family = poisson(link="log"), 
                                         corstr = gee_corstr)}, 
             error = function(e){message(paste("An Error occurred:", e))
               message("Running GEE analysis with independence structure.")
               tryCatch({gee_model <<- geepack::geeglm(formula = as.formula(model_formula), 
                                                    data = as.data.frame(df_regr), 
                                                    id = id, 
                                                    family = poisson(link="log"), 
                                                    corstr = "independence")}, 
                        error = function(e) {message(paste("An error occurred:", e)) 
                          cat("Running GEE analysis using geepack package with independence structure.\n")
                          gee_model <<- geepack::geeglm(formula = as.formula(model_formula),
                                                       data = df_regr,
                                                       id = id,
                                                       family = poisson(link=log),
                                                       corstr = "independence")})}, 
             warning = function(w) {message(paste("An Error occurred:", w))
               message("Running GEE analysis with independence structure.")
               gee_model<<- geeasy::geelm(formula = as.formula(model_formula), 
                                          data = as.data.frame(df_regr), 
                                          id = id, 
                                          family = poisson(link="log"), 
                                          corstr = "independence")})
  }
  
  return(gee_model)
}