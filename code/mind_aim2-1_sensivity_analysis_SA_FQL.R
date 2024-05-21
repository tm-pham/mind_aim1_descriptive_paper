# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# Sensitivity analysis for S. aureus and FQL
# Plots
# ============================================================================ #
setwd("/Users/tm-pham/academia/hsph/mind/publications/aim2-1")
source("code/packages.R")
source("code/plotting_template.R")
source("code/mind_global_variables.R")
library(patchwork)
library(gridExtra)
library(ggpubr)

susc_levels = c("S", "R", "N")
susc_colors = c("#35978F", "#800000", "#888888")
fill_color ="#eda361"

# Load data 
load("data/bug_drugs.RData")
df_FQL_res_prop_yr <- read.csv("data/mind_aim2-1_FQL_res_prop_yr.csv") %>% 
  mutate(analysis = "Main analysis")
df_FQL_inc_yr <- read.csv("data/mind_aim2-1_FQL_phenotypic_inc_yr.csv") %>% 
  mutate(analysis = "Main analysis", 
         FQL_class = factor(FQL_class, levels = susc_levels))


# Load data from sensitivity analysis
load("data/mind_aim2-1_SA_FQL_sensitivity/mind_aim2-1_SA_FQL_missing_sensivitity_res_prop_inc_data.RData")
(bug <- bugs[9])


comb_plot <- NULL
scale_sec_axis_vec <- c(25)
drug <- "FQL_class"

# ---------------------------------------------------------------------------- #
# Panel A
# ---------------------------------------------------------------------------- #
(comb_plot_1 <- ggplot() + 
    facet_grid(.~analysis) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_FQL_res_prop_yr %>% filter(organismofinterest==bug), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_FQL_inc_yr %>% filter(organismofinterest==bug), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[1], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_FQL_inc_yr %>% filter(organismofinterest==bug), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[1],
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Fluoroquinolone susceptibility test result", 
         y = "Resistance proportion") + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[1])) +
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2008-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.position = "none", 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'), 
          strip.text = element_text(face = "bold"), 
          axis.text.x = element_text(size=16),
          axis.title.x = element_blank(), 
          axis.title.y = element_text(face = "plain", size = 24), 
          axis.text.y = element_text(size=16),
          plot.margin=unit(c(0,0.1,0,0),"cm")))

# ---------------------------------------------------------------------------- #
# Sensitivity analysis
# ---------------------------------------------------------------------------- #
res_prop_yr[[1]] <- res_prop_yr[[1]] %>% 
  mutate(analysis = "Methicillin resistance as proxy for fluoroquinolone resistance")
for(i in 1:2){
  for(susc_categ in names(res_inc_yr[[1]])){
    res_inc_yr[[i]][[susc_categ]] <- res_inc_yr[[i]][[susc_categ]] %>% 
      mutate(FQL_class = susc_categ)
  }
}
df_SA_FQL_inc_yr_1 <- do.call("rbind", res_inc_yr[[1]]) %>% 
  mutate(analysis = "Methicillin resistance as proxy for fluoroquinolone resistance", 
         FQL_class = factor(FQL_class, levels = susc_levels, labels = c("S", "R", "Missing")))
df_SA_FQL_inc_yr_2 <- do.call("rbind", res_inc_yr[[2]]) %>%
  mutate(analysis = "All missing test results are susceptible", 
         FQL_class = factor(FQL_class, levels = susc_levels, labels = c("S", "R", "Missing")))

# Methicillin resistance as proxy
(comb_plot_2 <- ggplot() + 
    facet_grid(.~analysis) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = res_prop_yr[[1]], 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_SA_FQL_inc_yr_1, 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/22, 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_SA_FQL_inc_yr_1, 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/22,
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Fluoroquinolone susceptibility test result") + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[1])) +
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2008-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.position = "none", 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'), 
          strip.text = element_text(face = "bold"), 
          axis.text.x = element_text(size=16),
          axis.title = element_blank(), 
          axis.text.y = element_text(size=16),
          plot.margin=unit(c(0,0.1,0,0.5),"cm")))

# All missing test results are susceptible
(comb_plot_3 <- ggplot() + 
    facet_grid(.~analysis) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = res_prop_yr[[2]], 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_SA_FQL_inc_yr_2, 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[1], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_SA_FQL_inc_yr_2, 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[1],
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Fluoroquinolone susceptibility test result") + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0),
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[1])) +
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2008-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.position = "none", 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'), 
          strip.text = element_text(face = "bold"), 
          axis.text.x = element_text(size=16),
          axis.title = element_blank(), 
          axis.text.y = element_text(size=16),
          plot.margin=unit(c(0,0.5,0,0.5),"cm")))


# ---------------------------------------------------------------------------- #
# Panel B: GEE results
# ---------------------------------------------------------------------------- #
FQL_results <- list()
FQL_results[["R"]] <- read.xlsx("/Users/tm-pham/academia/hsph/mind/publications/aim2-1/results/mind_aim2-1_gee_AAPC_results_phenotypic_incidence.xlsx", 
                                sheetName = "FQL_class_R")
FQL_results[["S"]] <- read.xlsx("/Users/tm-pham/academia/hsph/mind/publications/aim2-1/results/mind_aim2-1_gee_AAPC_results_phenotypic_incidence.xlsx", 
                                sheetName = "FQL_class_S")
FQL_results[["Rp"]] <- read.xlsx("/Users/tm-pham/academia/hsph/mind/publications/aim2-1/results/mind_aim2-1_gee_AAPC_results_resistance_proportions.xlsx", 
                                 sheetName = "FQL_class")

FQL_results$R <- FQL_results$R %>% 
  mutate(type = "Resistant phenotype")
FQL_results$S <- FQL_results$S %>% 
  mutate(type = "Susceptible phenotype")
FQL_results$Rp <- FQL_results$Rp %>% 
  mutate(type = "Resistance proportion")

# Methicillin as proxy
FQL_results_1 <- list()
FQL_results_1[["R"]] <- read.xlsx("/Users/tm-pham/academia/hsph/mind/publications/aim2-1/data/mind_aim2-1_SA_FQL_sensitivity/mind_aim2-1_SA_FQL_res_inc_gee_sensitivity_1.xlsx", 
                                sheetName = "FQL_class_R")
FQL_results_1[["S"]] <- read.xlsx("/Users/tm-pham/academia/hsph/mind/publications/aim2-1/data/mind_aim2-1_SA_FQL_sensitivity/mind_aim2-1_SA_FQL_res_inc_gee_sensitivity_1.xlsx", 
                                  sheetName = "FQL_class_S")
FQL_results_1[["Rp"]] <- read.csv("/Users/tm-pham/academia/hsph/mind/publications/aim2-1/data/mind_aim2-1_SA_FQL_sensitivity/mind_aim2-1_SA_FQL_res_prop_gee_sensitivity_1.csv")

FQL_results_1$R <- FQL_results_1$R %>% 
  mutate(type = "Resistant phenotype")
FQL_results_1$S <- FQL_results_1$S %>% 
  mutate(type = "Susceptible phenotype")
FQL_results_1$Rp <- FQL_results_1$Rp %>% 
  mutate(type = "Resistance proportion")

# All missings are susceptible
FQL_results_2 <- list()
FQL_results_2[["R"]] <- read.xlsx("/Users/tm-pham/academia/hsph/mind/publications/aim2-1/data/mind_aim2-1_SA_FQL_sensitivity/mind_aim2-1_SA_FQL_res_inc_gee_sensitivity_2.xlsx", 
                                  sheetName = "FQL_class_R")
FQL_results_2[["S"]] <- read.xlsx("/Users/tm-pham/academia/hsph/mind/publications/aim2-1/data/mind_aim2-1_SA_FQL_sensitivity/mind_aim2-1_SA_FQL_res_inc_gee_sensitivity_2.xlsx", 
                                  sheetName = "FQL_class_S")
FQL_results_2[["Rp"]] <- read.csv("/Users/tm-pham/academia/hsph/mind/publications/aim2-1/data/mind_aim2-1_SA_FQL_sensitivity/mind_aim2-1_SA_FQL_res_prop_gee_sensitivity_2.csv")

FQL_results_2$R <- FQL_results_2$R %>% 
  mutate(type = "Resistant phenotype")
FQL_results_2$S <- FQL_results_2$S %>% 
  mutate(type = "Susceptible phenotype")
FQL_results_2$Rp <- FQL_results_2$Rp %>% 
  mutate(type = "Resistance proportion")

# Data frame for plotting
df_FQL_gee <- do.call("rbind", FQL_results) %>% 
  mutate(time_period = factor(time_period, 
                              levels = c("pre-pandemic", "pandemic"), 
                              labels = c("2007-2019", "2020-2022")), 
         organismofinterest = factor(organismofinterest, levels = bugs_ordered), 
         analysis = "Main analysis") %>% 
  filter(organismofinterest == "Staphylococcus aureus") %>% 
  rbind(do.call("rbind", FQL_results_1) %>% 
          mutate(time_period = factor(time_period, 
                                      levels = c("pre-pandemic", "pandemic"), 
                                      labels = c("2007-2019", "2020-2022")), 
                 organismofinterest = factor(organismofinterest, levels = bugs_ordered), 
                 analysis = "Methicillin as proxy for fluoroquinolone resistance"))  %>% 
  rbind(do.call("rbind", FQL_results_2) %>% 
          mutate(time_period = factor(time_period, 
                                      levels = c("pre-pandemic", "pandemic"), 
                                      labels = c("2007-2019", "2020-2022")), 
                 organismofinterest = factor(organismofinterest, levels = bugs_ordered), 
                 analysis = "All missing test results are susceptible")) %>% 
  mutate(analysis = factor(analysis, levels = c("Main analysis", 
                                                "Methicillin as proxy for fluoroquinolone resistance", 
                                                "All missing test results are susceptible")))

# Plot 
(FQL_GEE_results_plot <- ggplot(df_FQL_gee %>% filter(variable == "overall"), 
                                aes(x=time.trend, y=rev(time_period))) +
    facet_grid(rows=vars(analysis), cols = vars(type), switch="y", scales = "free") + 
    geom_vline(xintercept = 0, linetype=2, linewidth=2., color="darkred")+ 
    geom_errorbar(aes(xmin=asymp.LCL, xmax=asymp.UCL, color=time_period), width=0., linewidth=2) +
    geom_point(aes(fill=time_period, color = time_period), stroke=1, size=3.5) + 
    labs(x = "Average annual percentage change (%)", 
         color = "Time period", 
         fill = "Time period") + 
    scale_x_continuous(breaks = seq(-25, 100, by=10)) +
    scale_fill_manual(values=c("black", "#696969")) + 
    scale_color_manual(values=c("black", "#696969")) + 
    theme_template() + 
    theme(legend.position = "right", 
          axis.title.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          strip.text.y.left = element_text(angle = 0),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=18), 
          axis.title.x = element_text(size=26, face="plain"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), 
          plot.margin = margin(1.5,0,0,0, 'cm')))

# ---------------------------------------------------------------------------- #
# Combine to final plot
# ---------------------------------------------------------------------------- #
top_row = ggpubr::ggarrange(comb_plot_1, comb_plot_2, 
                            comb_plot_3,  ncol = 3, labels = "AUTO")
top_plot <- ggpubr::annotate_figure(top_row,
                                    left = ggpubr::text_grob("Resistance proportion", rot = 90, size=20), 
                                    right = ggpubr::text_grob("Phenotypic incidence per 1,000 admissions)", rot=270, size=20, margin(0.5,0.5,0,0.5,unit="cm")))
bottom_row = ggpubr::ggarrange(NULL, FQL_GEE_results_plot, NULL, ncol = 3, 
                               labels = c("", "D", ""), 
                               font.label = list(size=18),
                               widths = c(1,5,1))
final_plot = ggpubr::ggarrange(top_plot, bottom_row, ncol = 1, heights =  c(1, 1.2))
ggsave(final_plot, file = "figures/mind_aim2-1_sensitivity_analysis_SA_FQL.pdf", 
       width=32, height=16)
ggsave(final_plot, file = "figures/mind_aim2-1_sensitivity_analysis_SA_FQL.eps", 
       width=32, height=16)
ggsave(final_plot, file = "figures/mind_aim2-1_sensitivity_analysis_SA_FQL.png", 
       width=32, height=16)

