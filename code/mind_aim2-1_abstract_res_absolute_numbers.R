# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# Absolute numbers for resistance proportion in the abstract
# ============================================================================ #
setwd("/Users/tm-pham/academia/hsph/mind/publications/aim2-1/")
source("code/packages.R")
source("code/mind_global_variables.R")


load("/Users/tm-pham/academia/hsph/mind/data/mind_df_inc_res_prop_yr_data.RData")

df_inc_yr$`Staphylococcus aureus`$anti_staphylococcal_beta_lactams$overall %>% 
  ungroup() %>% 
  group_by(organismofinterest, date_year) %>% 
  summarize(n_total = sum(n_inc), 
         res = ifelse(anti_staphylococcal_beta_lactams=="R", n_inc, 0)) %>% 
  filter(res!=0) %>% 
  mutate(perc = res/n_total)


df_inc_yr$`Enterococcus faecium`$VANC$overall %>% 
  ungroup() %>% 
  group_by(organismofinterest, date_year) %>% 
  summarize(n_total = sum(n_inc), 
            res = ifelse(VANC=="R", n_inc, 0)) %>% 
  filter(res!=0) %>% 
  mutate(perc = res/n_total)


df_inc_yr$`Enterococcus faecalis`$VANC$overall %>% 
  ungroup() %>% 
  group_by(organismofinterest, date_year) %>% 
  summarize(n_total = sum(n_inc), 
            res = ifelse(VANC=="R", n_inc, 0)) %>% 
  filter(res!=0) %>% 
  mutate(perc = res/n_total)

df_inc_yr$`Escherichia coli`$CPH_03_class$overall %>% 
  ungroup() %>% 
  group_by(organismofinterest, date_year) %>% 
  summarize(n_total = sum(n_inc), 
            res = ifelse(CPH_03_class=="R", n_inc, 0)) %>% 
  filter(res!=0) %>% 
  mutate(perc = res/n_total)


df_inc_yr$`Enterobacter cloacae`$CPM_class$overall %>% 
  ungroup() %>% 
  group_by(organismofinterest, date_year) %>% 
  summarize(n_total = sum(n_inc), 
            res = ifelse(CPM_class=="R", n_inc, 0)) %>% 
  filter(res!=0) %>% 
  mutate(perc = res/n_total)

df_inc_yr$`Klebsiella pneumoniae`$CPM_class$overall %>% 
  ungroup() %>% 
  group_by(organismofinterest, date_year) %>% 
  summarize(n_total = sum(n_inc), 
            res = ifelse(CPM_class=="R", n_inc, 0)) %>% 
  filter(res!=0) %>% 
  mutate(perc = res/n_total)

df_inc_yr$`Escherichia coli`$CPM_class$overall %>% 
  ungroup() %>% 
  group_by(organismofinterest, date_year) %>% 
  summarize(n_total = sum(n_inc), 
            res = ifelse(CPM_class=="R", n_inc, 0)) %>% 
  filter(res!=0) %>% 
  mutate(perc = res/n_total)

df_inc_yr$`Acinetobacter Sp.`$CPM_class$overall %>% 
  ungroup() %>% 
  group_by(organismofinterest, date_year) %>% 
  summarize(n_total = sum(n_inc), 
            res = ifelse(CPM_class=="R", n_inc, 0)) %>% 
  filter(res!=0) %>% 
  mutate(perc = res/n_total)
