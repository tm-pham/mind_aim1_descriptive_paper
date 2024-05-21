# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# MInD Aim 2.1 manuscript
# Title: Figure 4 
# Carbapenem resistance
# Panel A: lines 35ff.
# Combined plot of resistance proportion and phenotypic incidence
# 
# Panel B: lines 280ff.
# GEE AAPC results for resistance proportion and phenotypic incidence
# 
# Panel C: lines 332ff.
# Carbapenem use barplot
# 
# Combined plot: lines 360ff.
# ============================================================================ #
setwd("/Users/tm-pham/academia/hsph/mind/publications/aim2-1/")
source("code/mind_global_variables.R")
source("code/packages.R")
source("code/plotting_template.R")

# Load data 
load("data/mind_CPM_drug_use_year_df.RData") 
df_CPM_res_prop_yr <- read.csv("data/mind_aim2-1_CPM_res_prop_yr.csv")
df_CPM_inc_yr <- read.csv("data/mind_aim2-1_CPM_phenotypic_inc_yr.csv")

comb_plot <- NULL
drug = "CPM_class"
scale_sec_axis_vec <- c(23.5, 14.5, 4.6, 2.53, 13.5, 1.87)

# Set levels and labels
df_CPM_inc_yr$CPM_class <- factor(df_CPM_inc_yr$CPM_class, levels = c("S", "R", NA))


# ---------------------------------------------------------------------------- #
# Panel A
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# E. coli
# ---------------------------------------------------------------------------- #
(comb_plot_1 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-08-30", format="%Y-%m-%d")), linetype="dashed", color="black",
               linewidth=1.3) +
    geom_bar(data = df_CPM_res_prop_yr %>% filter(organismofinterest=="Escherichia coli"), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_CPM_inc_yr %>% filter(organismofinterest=="Escherichia coli"), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[1], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_CPM_inc_yr %>% filter(organismofinterest=="Escherichia coli"), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[1], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Carbapenem susceptibility test result") + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[1])) +
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2007-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    theme_template_time() + 
    theme(legend.position = "none", 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y.left = element_text(size=16), 
          axis.text.y.right = element_text(size=16), 
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# Klebsiella pneumoniae
# ---------------------------------------------------------------------------- #
(comb_plot_2 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_CPM_res_prop_yr %>% filter(organismofinterest=="Klebsiella pneumoniae"), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_CPM_inc_yr %>% filter(organismofinterest=="Klebsiella pneumoniae"), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[2], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_CPM_inc_yr %>% filter(organismofinterest=="Klebsiella pneumoniae"), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[2], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Carbapenem susceptibility test result") + 
    scale_x_date(date_labels = "%Y", breaks="2 years") + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[2])) +
    theme_template_time() + 
    theme(legend.position = "none", 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y.left = element_text(size=16), 
          axis.text.y.right = element_text(size=16), 
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# Enterobacter cloacae
# ---------------------------------------------------------------------------- #
(comb_plot_3 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_CPM_res_prop_yr %>% filter(organismofinterest=="Enterobacter cloacae"), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_CPM_inc_yr %>% filter(organismofinterest=="Enterobacter cloacae"), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[3], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_CPM_inc_yr %>% filter(organismofinterest=="Enterobacter cloacae"), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[3], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Carbapenem susceptibility test result") + 
    scale_x_date(date_labels = "%Y", breaks="2 years") + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[3])) +
    theme_template_time() + 
    theme(legend.position = "none", 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y.left = element_text(size=16), 
          axis.text.y.right = element_text(size=16), 
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# Serratia marcescens
# ---------------------------------------------------------------------------- #
(comb_plot_4 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_CPM_res_prop_yr %>% filter(organismofinterest=="Serratia marcescens"), 
             aes(x = ymd(date_year, truncated=2), 
                 y = prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_CPM_inc_yr %>% filter(organismofinterest=="Serratia marcescens"), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[4], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_CPM_inc_yr %>% filter(organismofinterest=="Serratia marcescens"), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[4], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Carbapenem susceptibility test result") + 
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2007-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[4])) +
    theme_template_time() + 
    theme(legend.position = "none", 
          axis.text.y.left = element_text(size=16), 
          axis.text.y.right = element_text(size=16), 
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# Pseudomonas aeruginosa
# ---------------------------------------------------------------------------- #
(comb_plot_5 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_CPM_res_prop_yr %>% filter(organismofinterest=="Pseudomonas aeruginosa"), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_CPM_inc_yr %>% filter(organismofinterest=="Pseudomonas aeruginosa"), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[5], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_CPM_inc_yr %>% filter(organismofinterest=="Pseudomonas aeruginosa"), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[5], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Carbapenem susceptibility test result") + 
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2007-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[5])) +
    theme_template_time() + 
    theme(legend.position = "none", 
          axis.text.y.left = element_text(size=16), 
          axis.text.y.right = element_text(size=16), 
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# Acinetobacter Sp.
# ---------------------------------------------------------------------------- #
(comb_plot_6 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_CPM_res_prop_yr %>% filter(organismofinterest=="Acinetobacter Sp."), 
             aes(x = ymd(date_year, truncated=2), 
                 y = prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_CPM_inc_yr %>% filter(organismofinterest=="Acinetobacter Sp."), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[6], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_CPM_inc_yr %>% filter(organismofinterest=="Acinetobacter Sp."), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[6], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Carbapenem susceptibility test result") + 
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2007-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[6])) +
    theme_template_time() + 
    theme(legend.position = "none", 
          axis.text.y.left = element_text(size=16), 
          axis.text.y.right = element_text(size=16), 
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# Combined plot
# ---------------------------------------------------------------------------- #
together <- ggpubr::ggarrange(comb_plot_1, comb_plot_2, comb_plot_3, 
                              comb_plot_4, comb_plot_5, comb_plot_6,
                              ncol = 3, # 3 columns in the final figure
                              nrow = 2, # 3 rows in the final figure
                              align = 'hv', common.legend = T, legend="bottom")

final_plot <- ggpubr::annotate_figure(together,
                                      left = ggpubr::text_grob("Resistance proportion", rot = 90, size=24), 
                                      right = ggpubr::text_grob("Incidence per 1,000 admissions", rot=270, size=24, margin(0.5,0,0,0.2,unit="cm"))
) + theme(plot.margin = margin(0,0.6,1,0, "cm"), 
          plot.background = element_rect(fill='transparent', color=NA), 
          legend.background = element_rect(fill='transparent', color=NA))

# ---------------------------------------------------------------------------- #
# Panel B: AAPC results
# ---------------------------------------------------------------------------- #
CPM_results <- list()
CPM_results[["R"]] <- read.xlsx("results/mind_aim2-1_gee_AAPC_results_phenotypic_incidence.xlsx", 
                                  sheetName = "CPM_class_R")
CPM_results[["S"]] <- read.xlsx("results/mind_aim2-1_gee_AAPC_results_phenotypic_incidence.xlsx", 
                                  sheetName = "CPM_class_S")
CPM_results[["Rp"]] <- read.xlsx("results/mind_aim2-1_gee_AAPC_results_resistance_proportions.xlsx", 
                                   sheetName = "CPM_class")

CPM_results$R <- CPM_results$R %>% 
  mutate(type = "Resistant phenotype")
CPM_results$S <- CPM_results$S %>% 
  mutate(type = "Susceptible phenotype")
CPM_results$Rp <- CPM_results$Rp %>% 
  mutate(type = "Resistance proportion")

# Data frame for plotting
df_CPM_gee <- do.call("rbind", CPM_results) %>% 
  mutate(time_period = factor(time_period, 
                              levels = c("pre-pandemic", "pandemic"), 
                              labels = c("2007-2019", "2020-2022")), 
         organismofinterest = factor(organismofinterest, levels = bugs_ordered),
         type = factor(type, levels = c("Resistant phenotype", "Susceptible phenotype", "Resistance proportion")))

# Plot 
(CPM_GEE_results_plot <- ggplot(df_CPM_gee %>% filter(variable == "overall", organismofinterest != "Escherichia coli"), aes(x=time.trend, y=rev(time_period))) +
    facet_grid(rows=vars(organismofinterest), cols = vars(type), switch="y", scales = "free") + 
    geom_vline(xintercept = 0, linetype=2, linewidth=2., color="darkred")+ 
    geom_errorbar(aes(xmin=asymp.LCL, xmax=asymp.UCL, color=time_period), width=0., linewidth=2) +
    geom_point(aes(fill=time_period, color = time_period), stroke=1, size=3.5) + 
    labs(x = "Average annual percentage change (%)", 
         shape = "Time period", 
         color = "Time period", 
         fill = "Time period") + 
    scale_x_continuous(n.breaks=6) +
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
          plot.margin = margin(0.5,0,0,0, 'cm')))


# ---------------------------------------------------------------------------- #
# Panel C: CPM antibiotic use
# ---------------------------------------------------------------------------- #
CPM_use_overall_yr <- CPM_use$overall %>% 
  rename(antibiotic=variable) %>% 
  mutate(variable = "Carbapenem antibiotic use") %>% 
  ungroup() %>% 
  group_by(date_year, variable) %>% 
  reframe(DOT = sum(n)*1000/n_pat) %>% 
  unique()

(CPM_overall_yr_barplot <- ggplot(CPM_use_overall_yr, aes(x=ymd(date_year, truncated=2), y = DOT)) + 
    facet_wrap(.~variable) + 
    geom_bar(stat="identity", color="black") + 
    labs(y="Days of therapy\n(per 1,000 patient days)") + 
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2007-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "year")) + 
    # scale_y_continuous(limits = c(0, 20))+
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          axis.text.x = element_text(size=24), 
          axis.text.y = element_text(size=24), 
          axis.title.y = element_text(size=24, face="plain"), 
          strip.text = element_text(size=28, face="bold"), 
          plot.margin = margin(1.5,0,0,0, 'cm')))


# ---------------------------------------------------------------------------- #
# Combined plot for manuscript
figure5 <- ggpubr::ggarrange(final_plot, # Panel A
                             CPM_GEE_results_plot, # Panel B
                             CPM_overall_yr_barplot, # Panel C
                             nrow=3, align="v", vjust = 1.3, 
                             heights = c(1.5, 1.5, 1.),
                             labels="AUTO", font.label=list(size=30))
ggsave(figure5, file="figures/mind_aim2-1_fig5_CPM_ABC.pdf", width=20, height=26)
ggsave(figure5, file="figures/mind_aim2-1_fig5_CPM_ABC.pdf", width=20, height=26)
ggsave(figure5, file="figures/mind_aim2-1_fig5_CPM_ABC.png", width=20, height=26)

