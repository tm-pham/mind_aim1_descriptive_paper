# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# MInD Aim 2.1 manuscript
# Title: Figure 3
# Fluoroquinolone resistance and utilization
# ============================================================================ #
setwd("/Users/tm-pham/academia/hsph/mind/publications/aim1")
source("code/mind_colors.R")
source("code/packages.R")

# Load data 
load("data/bugs_drugs.RData")
df_FQL_res_prop_yr <- read.csv("data/mind_aim2-1_FQL_res_prop_yr.csv")
df_FQL_inc_yr <- read.csv("data/mind_aim2-1_FQL_phenotypic_inc_yr.csv")

# Global variables 
susc_colors = c("#35978F", "#800000", "#888888")
fill_color ="#eda361"

comb_plot <- NULL
scale_sec_axis_vec <- c(1.59, 5.4, 11.8, 3.95, 18.38, 15.7, 15.2, 2.95, 17.1)
drug <- "FQL_class"

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
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2008-01-01")), 
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
    scale_x_date(date_labels = "%Y", breaks="2 years") + 
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
    scale_x_date(date_labels = "%Y", breaks="2 years") + 
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
    scale_x_date(date_labels = "%Y", breaks="2 years") + 
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

# ggsave(final_plot, file="figures/mind_epidemics_figure3_FQL_res_prop_comb_plot.pdf", 
#        bg='transparent',
#        width=18, height=10)

# ---------------------------------------------------------------------------- #
# FQL use
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
          strip.text = element_text(size=28, face="bold")))


# ---------------------------------------------------------------------------- #
# Combined plot for manuscript
# ---------------------------------------------------------------------------- #
figure3 <- ggpubr::ggarrange(final_plot, FQL_overall_yr_barplot, nrow=2, align="v",
                             heights = c(1.8, 0.9),
                             labels="AUTO", font.label=list(size=30))
ggsave(figure3, file="figures/mind_aim2-1_fig3_FQL.pdf", width=19, height=17)
