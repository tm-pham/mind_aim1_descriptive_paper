# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# Sensitivity analysis 
# Covered lives as denominator
# ============================================================================ #
setwd("/Users/tm-pham/academia/hsph/mind/publications/aim2-1")
source("code/packages.R")
source("code/plotting_template.R")
source("code/mind_global_variables.R")

# Load table 
inc_gee_sens <- readxl::read_excel("results/mind_aim2-1_gee_AAPC_results_infection_incidence_COI_sensitivity_analysis.xlsx") %>% 
  rename(time_period = variable, 
         organismofinterest = org) %>% 
  mutate(onset ="Community-onset", 
         analysis = "Covered lives sensitivity analysis", 
         time_period = ifelse(time_period == "pre-pandemic", "2007-2019", "2020-2022"))

  
head(inc_gee_sens)
inc_gee_main <- readxl::read_excel("results/mind_aim2-1_gee_AAPC_results_infection_incidence.xlsx") %>% 
  filter(onset == "Community-onset") %>% 
  rename(time_period = variable, 
         organismofinterest = org) %>% 
  mutate(analysis = "Main analysis", 
         time_period = ifelse(time_period == "pre-pandemic", "2007-2019", "2020-2022"))


df_inc_gee_compare <- as.data.frame(rbind(inc_gee_sens, inc_gee_main)) %>% 
  mutate(analysis = factor(analysis, levels = c("Main analysis", "Covered lives sensitivity analysis")), 
         organismofinterest = factor(organismofinterest, levels = bugs_ordered))


# Plot to compare main analysis and sensitivyt analysis
(gee_compare_plot <- ggplot(df_inc_gee_compare, aes(x=time.trend, y=rev(time_period))) +
    facet_grid(rows=vars(organismofinterest), cols = vars(analysis), switch="y", scales = "free") + 
    geom_vline(xintercept = 0, linetype=2, linewidth=2., color="darkred")+ 
    geom_errorbar(aes(xmin=asymp.LCL, xmax=asymp.UCL, color=time_period), width=0., linewidth=1.5) +
    geom_point(aes(fill=time_period, color = time_period), stroke=2, size=3) + 
    labs(x = "Average annual percentage change (%)", color = "Time period", fill = "Time period") + 
    # scale_x_continuous(limits = c(-100, 100), breaks = seq(-100, 100, by=20)) +
    scale_shape_manual(values=c(19, 19)) +
    scale_fill_manual(values=c("black", "#696969")) + 
    scale_color_manual(values=c("black", "#696969")) + 
    theme_template() + 
    theme(legend.position = "right", 
          axis.title.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          # axis.text.y = element_text(face="bold.italic", size=24, hjust=0.5),
          strip.text.y.left = element_text(angle = 0),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=22), 
          axis.title.x = element_text(size=28, face="plain"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank()))
ggsave(gee_compare_plot, file = "figures/mind_aim2-1_sensitivity_analysis_denominator_infection_incidence.pdf", 
       width = 18, height= 10)
