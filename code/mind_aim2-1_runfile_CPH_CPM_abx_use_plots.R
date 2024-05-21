# ============================================================================ #
# Overall antibiotic use for Cephaolosporins and Carbapenems
# ============================================================================ #
setwd("/Users/tm-pham/academia/hsph/mind/")
source("code/packages.R")
source("code/mind_colors.R")
source("code/plotting_template.R")
load("data/bugs_drugs.RData")

# Load antibiotic use data 
load("/Users/tm-pham/academia/hsph/mind/data/abx_use/mind_CPH03_drug_use_year_df.RData") # CPH03_use
load("/Users/tm-pham/academia/hsph/mind/data/abx_use/mind_CPH04_drug_use_year_df.RData") # CPH04_use
load("/Users/tm-pham/academia/hsph/mind/data/abx_use/mind_CPM_drug_use_year_df.RData") 
# Overall FQL use data
CPH03_use_overall_yr <- CPH03_use$overall %>% 
  rename(antibiotic=variable) %>% 
  mutate(variable = "3rd generation cephalosporins") %>% 
  ungroup() %>% 
  group_by(date_year, variable) %>% 
  reframe(DOT = sum(n)*1000/n_pat) %>% 
  unique()

# Overall plot
(CPH03_overall_yr_plot <- ggplot(CPH03_use_overall_yr, aes(x=ymd(date_year, truncated=2), y = DOT)) + 
    facet_wrap(.~variable) + 
    geom_point(size=4.5) + 
    geom_line(linetype="dashed", linewidth=1.5) + 
    geom_smooth(linewidth=2.5) + 
    labs(y="Number of inpatient administrations\n(per 1,000 patient days, per year)") + 
    scale_x_date(date_labels="%Y", breaks = "year") + 
    scale_y_continuous(limits = c(0, NA))+
    theme_template_time() + 
    theme(axis.text.x = element_text(size=24), 
          axis.text.y = element_text(size=24), 
          axis.title.y = element_text(size=24), 
          strip.text = element_text(size=30)))
ggsave(CPH03_overall_yr_plot, file="figures/abx_use/mind_CPH03_overall_use_yr_plot.pdf", 
       width=16, height=9)  

# Plot per antibiotic
(CPH03_yr_plot <- ggplot(CPH03_use$overall, aes(x=ymd(date_year, truncated=2), y = n_abx)) + 
    facet_wrap(.~variable) + 
    geom_point(size=4.5) + 
    geom_line(linetype="dashed", linewidth=1.5) + 
    geom_smooth(linewidth=2.5) + 
    labs(y="Number of inpatient administrations\n(per 1,000 patient days, per year)") + 
    scale_x_date(date_labels="%Y", breaks = "2 years") + 
    theme_template_time() + 
    theme(axis.text.x = element_text(size=22), 
          axis.text.y = element_text(size=24), 
          axis.title.y = element_text(size=24), 
          strip.text = element_text(size=30)))
ggsave(CPH03_yr_plot, file="figures/abx_use/mind_CPH03_per_abx_use_yr_plot.pdf", 
       width=20, height=10)  


(CPH03_overall_yr_barplot <- ggplot(CPH03_use_overall_yr, aes(x=ymd(date_year, truncated=2), y = DOT)) + 
    facet_wrap(.~variable) + 
    geom_bar(stat="identity", color="black") + 
    labs(y="Days of therapy\n(per 1,000 patient days)") + 
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2007-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    # scale_y_continuous(limits = c(0, 20))+
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          axis.text.x = element_text(size=38), 
          axis.text.y = element_text(size=38), 
          axis.title.y = element_text(size=42, face="plain"), 
          strip.text = element_text(size=52, face="bold")))
ggsave(CPH03_overall_yr_barplot, file="figures/abx_use/mind_CPH03_overall_use_yr_barplot.pdf", 
       width=24, height=12)  


# ---------------------------------------------------------------------------- #
# 4th generation Cephalosporins
# ---------------------------------------------------------------------------- #
CPH04_use_overall_yr <- CPH04_use$overall %>% 
  rename(antibiotic=variable) %>% 
  mutate(variable = "4th generation cephalosporins") %>% 
  ungroup() %>% 
  group_by(date_year, variable) %>% 
  reframe(DOT = sum(n)*1000/n_pat) %>% 
  unique()

# Overall plot
(CPH04_overall_yr_plot <- ggplot(CPH04_use_overall_yr, aes(x=ymd(date_year, truncated=2), y = DOT)) + 
    facet_wrap(.~variable) + 
    geom_point(size=4.5) + 
    geom_line(linetype="dashed", linewidth=1.5) + 
    geom_smooth(linewidth=2.5) + 
    labs(y="Number of inpatient administrations\n(per 1,000 patient days, per year)") + 
    scale_x_date(date_labels="%Y", breaks = "year") + 
    scale_y_continuous(limits = c(0, NA))+
    theme_template_time() + 
    theme(axis.text.x = element_text(size=24), 
          axis.text.y = element_text(size=24), 
          axis.title.y = element_text(size=24), 
          strip.text = element_text(size=30)))
ggsave(CPH04_overall_yr_plot, file="figures/abx_use/mind_CPH04_overall_use_yr_plot.pdf", 
       width=16, height=9)  

# Plot per antibiotic
(CPH04_yr_plot <- ggplot(CPH04_use$overall, aes(x=ymd(date_year, truncated=2), y = n_abx)) + 
    facet_wrap(.~variable) + 
    geom_point(size=4.5) + 
    geom_line(linetype="dashed", linewidth=1.5) + 
    geom_smooth(linewidth=2.5) + 
    labs(y="Number of inpatient administrations\n(per 1,000 patient days, per year)") + 
    scale_x_date(date_labels="%Y", breaks = "year") + 
    theme_template_time() + 
    theme(axis.text.x = element_text(size=24), 
          axis.text.y = element_text(size=24), 
          axis.title.y = element_text(size=24), 
          strip.text = element_text(size=30)))
ggsave(CPH04_yr_plot, file="figures/abx_use/mind_CPH04_per_abx_use_yr_plot.pdf", 
       width=16, height=9)  


(CPH04_overall_yr_barplot <- ggplot(CPH04_use_overall_yr, aes(x=ymd(date_year, truncated=2), y = DOT)) + 
    facet_wrap(.~variable) + 
    geom_bar(stat="identity", color="black") + 
    labs(y="Days of therapy\n(per 1,000 patient days)") + 
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2007-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    # scale_y_continuous(limits = c(0, 20))+
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          axis.text.x = element_text(size=38), 
          axis.text.y = element_text(size=38), 
          axis.title.y = element_text(size=42, face="plain"), 
          strip.text = element_text(size=52, face="bold")))
ggsave(CPH04_overall_yr_barplot, file="figures/abx_use/mind_CPH04_overall_use_yr_barplot.pdf", 
       width=24, height=12)  



# ---------------------------------------------------------------------------- #
# Carbapenem use 
load("/Users/tm-pham/academia/hsph/mind/data/abx_use/mind_CPM_drug_use_year_df.RData") 

CPM_use_overall_yr <- CPM_use$overall %>% 
  rename(antibiotic=variable) %>% 
  mutate(variable = "Carbapenems") %>% 
  ungroup() %>% 
  group_by(date_year, variable) %>% 
  reframe(DOT = sum(n)*1000/n_pat) %>% 
  unique()

# Overall plot
(CPM_overall_yr_plot <- ggplot(CPM_use_overall_yr, aes(x=ymd(date_year, truncated=2), y = DOT)) + 
    facet_wrap(.~variable) + 
    geom_point(size=4.5) + 
    geom_line(linetype="dashed", linewidth=1.5) + 
    geom_smooth(linewidth=2.5) + 
    labs(y="Number of inpatient administrations\n(per 1,000 patient days, per year)") + 
    scale_x_date(date_labels="%Y", breaks = "2 years") + 
    scale_y_continuous(limits = c(0, NA))+
    theme_template_time() + 
    theme(axis.text.x = element_text(size=24), 
          axis.text.y = element_text(size=24), 
          axis.title.y = element_text(size=24), 
          strip.text = element_text(size=30)))
ggsave(CPM_overall_yr_plot, file="figures/abx_use/mind_CPM_overall_use_yr_plot.pdf", 
       width=18, height=9)  

(CPM_overall_yr_barplot <- ggplot(CPM_use_overall_yr, aes(x=ymd(date_year, truncated=2), y = DOT)) + 
    facet_wrap(.~variable) + 
    geom_bar(stat="identity", color="black") + 
    labs(y="Days of therapy\n(per 1,000 patient days)") + 
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2007-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    # scale_y_continuous(limits = c(0, 20))+
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          axis.text.x = element_text(size=38), 
          axis.text.y = element_text(size=38), 
          axis.title.y = element_text(size=42, face="plain"), 
          strip.text = element_text(size=52, face="bold")))
ggsave(CPM_overall_yr_barplot, file="figures/abx_use/mind_CPM_overall_use_yr_barplot.pdf", 
       width=18, height=10)  


(CPM_yr_plot <- ggplot(CPM_use$overall, aes(x=ymd(date_year, truncated=2), y = n_abx)) + 
    facet_wrap(.~variable) + 
    geom_point(size=4.5) + 
    geom_line(linetype="dashed", linewidth=1.5) + 
    geom_smooth(linewidth=2.5) + 
    labs(y="Number of inpatient administrations\n(per 1,000 patient days, per year)") + 
    scale_x_date(date_labels="%Y", breaks = "2 years") + 
    theme_template_time() + 
    theme(axis.text.x = element_text(size=24), 
          axis.text.y = element_text(size=24), 
          axis.title.y = element_text(size=24), 
          strip.text = element_text(size=30)))
ggsave(CPM_yr_plot, file="figures/abx_use/mind_CPM_per_abx_use_yr_plot.pdf", 
       width=20, height=10)  
