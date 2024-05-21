# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# MInD Aim 2.1 manuscript
# Title: Figure 3
# Fluoroquinolone resistance and utilization
# ---------------------------------------------------------------------------- #
# Panel A: lines 36ff.
# Combined plot of resistance proportion and phenotypic incidence
# 
# Panel B: lines 425ff.
# GEE AAPC results for resistance proportion and phenotypic incidence
# 
# Panel C: lines 445ff.
# Fluoroquinolone use barplot
# 
# Combination of all plots: lines 509ff. 
# ============================================================================ #
setwd("/Users/tm-pham/academia/hsph/mind/publications/aim2-1")
source("code/packages.R")
source("code/plotting_template.R")
source("code/mind_global_variables.R")

# Load data 
load("data/bug_drugs.RData")
df_FQL_res_prop_yr <- read.csv("data/mind_aim2-1_FQL_res_prop_yr.csv")
df_FQL_inc_yr <- read.csv("data/mind_aim2-1_FQL_phenotypic_inc_yr.csv")

# Variables for tis script
comb_plot <- NULL
scale_sec_axis_vec <- c(1.59, 5.4, 11.8, 3.95, 18.38, 15.7, 15.2, 2.95, 17.1)
drug <- "FQL_class"

# Set levels/labels for antibiotic susceptibility test results
df_FQL_inc_yr$FQL_class <- factor(df_FQL_inc_yr$FQL_class, levels = c("S", "R", NA))

# ---------------------------------------------------------------------------- #
# Panel A
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Acinetobacter sp.
# ---------------------------------------------------------------------------- #
(comb_plot_1 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_FQL_res_prop_yr %>% filter(organismofinterest==bugs[1]), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[1]), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[1], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[1]), 
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
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2007-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.position = "none", 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'), 
          axis.text.x = element_text(size=16),
          axis.title = element_blank(), 
          axis.text.y = element_text(size=16),
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# Enterobacter cloacae
# ---------------------------------------------------------------------------- #
(comb_plot_2 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_FQL_res_prop_yr %>% filter(organismofinterest==bugs[2]), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[2]), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[2], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[2]), 
                aes(x = ymd(date_year, truncated=2), 
                    y =   inc/scale_sec_axis_vec[2], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Fluoroquinolone susceptibility test result") + 
    scale_x_date(date_labels = "%Y", breaks="2 years") + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[2])) +
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.position = "none", 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y = element_text(size=16),
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# E. faecium
# ---------------------------------------------------------------------------- #
(comb_plot_4 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_FQL_res_prop_yr %>% filter(organismofinterest==bugs[4]), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[4]), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[4], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[4]), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[4], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Fluoroquinolone susceptibility test result") + 
    scale_x_date(date_labels = "%Y", breaks="2 years") + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[2])) +
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.position = "none", 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y = element_text(size=16),
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# E. coli
# ---------------------------------------------------------------------------- #
(comb_plot_5 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_FQL_res_prop_yr %>% filter(organismofinterest==bugs[5]), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[5]), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[5], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[5]), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[5], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Fluoroquinolone susceptibility test result") + 
    scale_x_date(date_labels = "%Y", breaks="2 years") + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[5])) +
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.position = "none", 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(size=16),
          axis.ticks.x = element_blank(), 
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# E. faecalis
# ---------------------------------------------------------------------------- #
(comb_plot_3 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_FQL_res_prop_yr %>% filter(organismofinterest==bugs[3]), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[3]), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[3], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[3]), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[3], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Fluoroquinolone susceptibility test result") + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    scale_x_date(date_labels = "%Y", breaks="2 years") + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[3])) +
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.position = "none", 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(size=16),
          axis.ticks.x = element_blank(), 
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# Klebsiella
# ---------------------------------------------------------------------------- #
(comb_plot_6 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_FQL_res_prop_yr %>% filter(organismofinterest==bugs[6]), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[6]), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[6], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[6]), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[6], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Fluoroquinolone susceptibility test result") + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    scale_x_date(date_labels = "%Y", breaks="2 years") + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[6])) +
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.position = "none", 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(size=16),
          axis.ticks.x = element_blank(), 
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# Pseudomonas
# ---------------------------------------------------------------------------- #
(comb_plot_7 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_FQL_res_prop_yr %>% filter(organismofinterest==bugs[7]), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[7]), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[7], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[7]), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[7], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Fluoroquinolone susceptibility test result") + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2007-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[7])) +
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.position = "none", 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'), 
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          axis.ticks.x = element_blank(), 
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# Serratia marcescens
# ---------------------------------------------------------------------------- #
(comb_plot_8 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_FQL_res_prop_yr %>% filter(organismofinterest==bugs[8]), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  scale_sec_axis_vec[8]*prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[8]), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[8], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[8]), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[8], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Fluoroquinolone susceptibility test result") + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2007-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[8])) +
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.position = "none", 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'), 
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          axis.ticks.x = element_blank(), 
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# S. aureus
# ---------------------------------------------------------------------------- #
(comb_plot_9 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_FQL_res_prop_yr %>% filter(organismofinterest==bugs[9]), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[9]), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[9], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_FQL_inc_yr %>% filter(organismofinterest==bugs[9]), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[9], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "Fluoroquinolone susceptibility test result") + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2007-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[9])) +
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.position = "none", 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(size=16),
          axis.ticks.x = element_blank(), 
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# Combined plot
# ---------------------------------------------------------------------------- #
together <- ggpubr::ggarrange(comb_plot_9, comb_plot_3, comb_plot_4, 
                              comb_plot_5, comb_plot_6, comb_plot_2, 
                              comb_plot_8, comb_plot_7, comb_plot_1, 
                              ncol = 3, # 3 columns in the final figure
                              nrow = 3, # 3 rows in the final figure
                              align = 'hv', common.legend = T, legend="bottom")

combined_plot <- gridExtra::grid.arrange(together + theme(legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
                                                          legend.box.background = element_rect(fill='transparent'),
                                                          plot.background = element_rect(fill='transparent', color=NA)))

final_plot <- ggpubr::annotate_figure(combined_plot,
                left = ggpubr::text_grob("Resistance proportion", rot = 90, size=24), 
                right = ggpubr::text_grob("Incidence per 1,000 admissions", rot=270, size=24, margin(0.5,0,0,0.2,unit="cm"))
) + theme(plot.margin = margin(0,0.6,1,0, "cm"), 
          plot.background = element_rect(fill='transparent', color=NA), 
          legend.background = element_rect(fill='transparent', color=NA))

# ---------------------------------------------------------------------------- #
# Panel B: FQL use
# ---------------------------------------------------------------------------- #
# load("/Users/tm-pham/academia/hsph/mind/data/mind_drug_use_df/mind_FQL_drug_use_qrt_df.rdata") # FQL_use
load("/Users/tm-pham/academia/hsph/mind/data/mind_drug_use_df/mind_FQL_drug_use_year_df.rdata") # FQL_use
# Overall FQL use data
FQL_use_overall_yr <- FQL_use$overall %>% 
  rename(antibiotic=variable) %>% mutate(variable = "Fluoroquinolone antibiotic use") %>% 
  ungroup() %>% 
  group_by(date_year, variable) %>% 
  reframe(n_abx = sum(n)*1000/n_pat) %>% 
  unique()

(FQL_overall_yr_barplot <- ggplot(FQL_use_overall_yr, aes(x=ymd(date_year, truncated=2), y = n_abx)) + 
    facet_wrap(.~variable) + 
    geom_bar(stat="identity", color="black", size=2) + 
    labs(y="Days of therapy\n(per 1,000 patient days)") + 
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2007-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "year")) + 
    scale_y_continuous(limits = c(0, 130))+
    theme_template_time() + 
    theme(plot.background = element_rect(fill='transparent', color=NA), 
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          axis.text.x = element_text(size=24), 
          axis.text.y = element_text(size=24), 
          axis.title.y = element_text(size=24, face="plain"), 
          strip.text = element_text(size=28, face="bold"), 
          plot.margin = margin(1.5,0,0,0, 'cm')))


# ---------------------------------------------------------------------------- #
# Panel C: FQL GEE results
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

# Data frame for plotting
df_FQL_gee <- do.call("rbind", FQL_results) %>% 
  mutate(time_period = factor(time_period, 
                              levels = c("pre-pandemic", "pandemic"), 
                              labels = c("2007-2019", "2020-2022")), 
         organismofinterest = factor(organismofinterest, levels = bugs_ordered),
         type = factor(type, levels = c("Resistant phenotype", "Susceptible phenotype", "Resistance proportion")))

# Plot 
(FQL_GEE_results_plot <- ggplot(df_FQL_gee %>% filter(variable == "overall"), aes(x=time.trend, y=rev(time_period))) +
    facet_grid(rows=vars(organismofinterest), cols = vars(type), switch="y", scales = "free") + 
    geom_vline(xintercept = 0, linetype=2, linewidth=2., color="darkred")+ 
    geom_errorbar(aes(xmin=asymp.LCL, xmax=asymp.UCL, color=time_period), width=0., linewidth=2) +
    geom_point(aes(shape=time_period, fill=time_period, color = time_period), stroke=1, size=3.5) + 
    labs(x = "Average annual percentage change (%)", 
         linetype="Time period", 
         shape = "Time period", 
         color = "Time period", 
         fill = "Time period") + 
    scale_x_continuous(breaks = seq(-25, 100, by=10)) +
    scale_shape_manual(values=c(19, 19)) +
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
# Combined plot for manuscript
# ---------------------------------------------------------------------------- #
figure3 <- ggpubr::ggarrange(final_plot, FQL_GEE_results_plot, FQL_overall_yr_barplot, 
                             nrow=3, align="v", vjust = 1.3, 
                             heights = c(1.8, 1.5, 0.9),
                             labels="AUTO", font.label=list(size=30))
ggsave(figure3, file="figures/mind_aim2-1_fig3ABC_FQL.pdf", width=20, height=30)
ggsave(figure3, file="figures/mind_aim2-1_fig3ABC_FQL.eps", width=20, height=30)
ggsave(figure3, file="figures/mind_aim2-1_fig3ABC_FQL.png", width=20, height=30)
