# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# Figure 2 of descriptive paper
# S. aureus and E. faecalis, zE. faecium
# ============================================================================ #
setwd("/Users/tm-pham/academia/hsph/mind/publications/aim2-1/")
source("code/packages.R")
source("code/mind_global_variables.R")
source("code/plotting_template.R")

# Load data 
load("data/bugs_drugs.RData")
load("data/mind_df_inc_res_prop_yr_data.RData")

library(ggh4x) # For facet_nested
library(ggpubr) # For ggarrange

# Global variables 
susc_colors = c("#35978F", "#800000", "#888888")
fill_color ="#eda361"


min_date <- min(df_inc_yr$`Staphylococcus aureus`$anti_staphylococcal_beta_lactams$onset$date_year)
max_date <- 2022
time_var = "date_year"
bar_size = 1.7
time_breaks = "2 years"
x_axis_size = 26
legend_size = 28
axis_tick_length = 0.2

if(str_detect(time_var, "month")) x_var <- "myd(date_month, truncated=1)"
if(str_detect(time_var, "qrt")) x_var <- "lubridate::yq(date_qrt)"
if(str_detect(time_var, "year")) x_var <- "ymd(date_year, truncated=2)"

time_period = c("2007-01-01", "2022-03-31")

y1_var = "inc"
y2_var = "prop_wNA"
y3_var = NULL

# ---------------------------------------------------------------------------- #
# STAPHYLOCOCCUS AUREUS
# ---------------------------------------------------------------------------- #
drug="anti_staphylococcal_beta_lactams"
scale_sec_axis = 15

(SA_onset_plot <- ggplot() + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = res_prop_yr$`Staphylococcus aureus`$onset2 %>%
               mutate(organismofinterest = "Staphylococcus aureus") %>% 
               filter(variable == "anti_staphylococcal_beta_lactams"), 
             aes(x = eval(parse(text=x_var)), 
                 y =  eval(parse(text=y2_var))), 
             stat="identity", color="black", fill=fill_color, size=bar_size) + 
    geom_point(data = df_inc_yr$`Staphylococcus aureus`$anti_staphylococcal_beta_lactams$onset2, 
               aes(x = eval(parse(text=x_var)), 
                   y = eval(parse(text=y1_var))/15, 
                   color = anti_staphylococcal_beta_lactams), size=5.5) + 
    geom_smooth(data = df_inc_yr$`Staphylococcus aureus`$anti_staphylococcal_beta_lactams$onset2, 
                aes(x = eval(parse(text=x_var)), 
                    y = eval(parse(text=y1_var))/15, 
                    color = anti_staphylococcal_beta_lactams), linewidth=2.5) + 
    facet_nested(~ organismofinterest + onset) + 
    labs(color = "Methicillin") + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*15)) +
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0(min_date, "-01-01")), 
                                                       to = as.Date(paste0(max_date, "-01-01")), by = time_breaks)) + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    theme_template_time() + 
    theme(legend.position = "bottom", 
          strip.placement = "outside",
          strip.text.x = element_text(face="bold", size=28),
          strip.text.x.top = element_text(face="bold.italic", size=28, margin = margin(0.4,0,0.4,0, "cm")), 
          legend.title = element_text(size=legend_size),
          legend.text = element_text(size=legend_size),
          axis.ticks.length.x = unit(axis_tick_length, "cm"), 
          axis.text.x = element_text(size=x_axis_size), 
          axis.text.y = element_text(size=x_axis_size),
          axis.title.y = element_blank()))

# ggsave(SA_onset_plot, file = "figures/SA/mind_SA_MRSA_onset2_res_prop_inc_comb_year_plot_2.pdf", width=18, height=8)


# ---------------------------------------------------------------------------- #
# ENTEROCOCCUS FAECALIS
# ---------------------------------------------------------------------------- #
drug="VANC"
scale_sec_axis = 11.3

(ENTFAES_onset_plot <- ggplot() + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    facet_nested(~ organismofinterest + onset) + 
    geom_bar(data = res_prop_yr$`Enterococcus faecalis`$onset2 %>% 
               mutate(organismofinterest = "Enterococcus faecalis") %>% 
               filter(eval(parse(text=x_var))>=as.Date(time_period[1], format="%Y-%m-%d") & 
                        eval(parse(text=x_var))<=as.Date(time_period[2], format="%Y-%m-%d"), 
                      variable == "VANC"), 
             aes(x = eval(parse(text=x_var)), 
                 y =  eval(parse(text=y2_var))), stat="identity", color="black", fill=fill_color, size=bar_size) + 
    geom_point(data = df_inc_yr$`Enterococcus faecalis`$VANC$onset2 %>% 
                 filter(eval(parse(text=x_var))>=as.Date(time_period[1], format="%Y-%m-%d") & eval(parse(text=x_var))<=as.Date(time_period[2], format="%Y-%m-%d")), 
               aes(x = eval(parse(text=x_var)), 
                   y = eval(parse(text=y1_var))/11.3, 
                   color = VANC), size=5.5) + 
    geom_smooth(data = df_inc_yr$`Enterococcus faecalis`$VANC$onset2 %>% 
                  filter(eval(parse(text=x_var))>=as.Date(time_period[1], format="%Y-%m-%d") & eval(parse(text=x_var))<=as.Date(time_period[2], format="%Y-%m-%d")), 
                aes(x = eval(parse(text=x_var)), 
                    y = eval(parse(text=y1_var))/11.3, 
                    color = VANC), linewidth=2.5) + 

    labs(color = drug_dict %>% filter(var==drug) %>% select(plot_label) %>% unlist() %>% unname()) + 
    scale_y_continuous(name = "", 
                       expand = c(0, 0), 
                       limits = c(0,1), 
                       labels=scales::percent, breaks=seq(0,1,by=0.2), 
                       sec.axis = sec_axis(trans=~.*11.3)) +
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0(min_date, "-01-01")), 
                                                       to = as.Date(paste0(max_date, "-01-01")), by = time_breaks)) + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    theme_template_time() + 
    theme(legend.position = "bottom", 
          strip.placement = "outside",
          strip.text.x = element_text(face="bold", size=28),
          strip.text.x.top = element_text(face="bold.italic", size=28, margin = margin(0.4,0,0.4,0, "cm")), 
          legend.title = element_text(size=legend_size),
          legend.text = element_text(size=legend_size),
          axis.ticks.length.x = unit(axis_tick_length, "cm"), 
          axis.text.x = element_text(size=x_axis_size), 
          axis.text.y = element_text(size=x_axis_size),
          axis.title.y = element_blank()))

# ggsave(ENTFAES_onset_plot, file = "figures/ENTFAES/mind_figure2_ENTFAES_VANC_onset2_res_prop_inc_comb_year_plot.pdf", width=16, height=16)

# ---------------------------------------------------------------------------- #
# ENTEROCOCCUS FAECIUM
# ---------------------------------------------------------------------------- #
drug="VANC"
scale_sec_axis = 4.1

(ENTFAEM_onset_plot <- ggplot() + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    facet_nested(~ organismofinterest + onset) + 
    geom_bar(data = res_prop_yr$`Enterococcus faecium`$onset2 %>% 
               mutate(organismofinterest = "Enterococcus faecium") %>% 
               filter(eval(parse(text=x_var))>=as.Date(time_period[1], format="%Y-%m-%d") & eval(parse(text=x_var))<=as.Date(time_period[2], format="%Y-%m-%d"), 
                      variable == drug), 
             aes(x = eval(parse(text=x_var)), 
                 y =  eval(parse(text=y2_var))), 
             stat="identity", color="black", fill=fill_color, size=bar_size) + 
    geom_point(data = df_inc_yr$`Enterococcus faecium`$VANC$onset2  %>% 
        filter(eval(parse(text=x_var))>=as.Date(time_period[1], format="%Y-%m-%d") & eval(parse(text=x_var))<=as.Date(time_period[2], format="%Y-%m-%d")), 
      aes(x = eval(parse(text=x_var)), 
          y = eval(parse(text=y1_var))/4.1, 
          color = VANC), size=5.5) + 
    geom_smooth(data = df_inc_yr$`Enterococcus faecium`$VANC$onset2  %>% 
        filter(eval(parse(text=x_var))>=as.Date(time_period[1], format="%Y-%m-%d") & eval(parse(text=x_var))<=as.Date(time_period[2], format="%Y-%m-%d")), 
      aes(x = eval(parse(text=x_var)), 
          y = eval(parse(text=y1_var))/4.1, 
          color = VANC), linewidth=2.5) + 
    labs(color = "Vancomycin") + 
    scale_y_continuous(name = "", 
                       expand = c(0, 0), 
                       limits = c(0, 1), 
                       labels=scales::percent, breaks=seq(0,1,by=0.2), 
                       sec.axis = sec_axis(trans=~.*4.1)) +
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0(min_date, "-01-01")), 
                                                       to = as.Date(paste0(max_date, "-01-01")), by = time_breaks)) + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    theme_template_time() + 
    theme(legend.position = "bottom", 
          strip.placement = "outside",
          strip.text.x = element_text(face="bold", size=28),
          strip.text.x.top = element_text(face="bold.italic", size=28, margin = margin(0.4,0,0.4,0, "cm")), 
          legend.title = element_text(size=legend_size),
          legend.text = element_text(size=legend_size),
          axis.ticks.length.x = unit(axis_tick_length, "cm"), 
          axis.text.x = element_text(size=x_axis_size), 
          axis.text.y = element_text(size=x_axis_size),
          axis.title.y = element_blank()))

# ggsave(ENTFAEM_onset_plot, file = "figures/ENTFAEM/mind_figure2_ENTFAEM_VANC_onset2_res_prop_inc_comb_year_plot.pdf", width=16, height=16)

# ---------------------------------------------------------------------------- #
# GEE results
# ---------------------------------------------------------------------------- #

Meth_results <- list()
Meth_results[["R"]] <- read.xlsx("results/mind_aim2-1_gee_AAPC_results_phenotypic_incidence.xlsx", 
                                sheetName = "anti_st..._R")
Meth_results[["S"]] <- read.xlsx("results/mind_aim2-1_gee_AAPC_results_phenotypic_incidence.xlsx", 
                                sheetName = "anti_st..._S")
Meth_results[["Rp"]] <- read.xlsx("results/mind_aim2-1_gee_AAPC_results_resistance_proportions.xlsx", 
                                 sheetName = "anti_st...")

Meth_results$R <- Meth_results$R %>% 
  mutate(type = "Resistant phenotype")
Meth_results$S <- Meth_results$S %>% 
  mutate(type = "Susceptible phenotype")
Meth_results$Rp <- Meth_results$Rp %>% 
  mutate(type = "Resistance proportion")

# Vancomycin
VANC_results <- list()
VANC_results[["R"]] <- read.xlsx("results/mind_aim2-1_gee_AAPC_results_phenotypic_incidence.xlsx", 
                                 sheetName = "VANC_R")
VANC_results[["S"]] <- read.xlsx("results/mind_aim2-1_gee_AAPC_results_phenotypic_incidence.xlsx", 
                                 sheetName = "VANC_S")
VANC_results[["Rp"]] <- read.xlsx("results/mind_aim2-1_gee_AAPC_results_resistance_proportions.xlsx", 
                                  sheetName = "VANC")

VANC_results$R <- VANC_results$R %>% 
  mutate(type = "Resistant phenotype")
VANC_results$S <- VANC_results$S %>% 
  mutate(type = "Susceptible phenotype")
VANC_results$Rp <- VANC_results$Rp %>% 
  mutate(type = "Resistance proportion")

# Data frame for plotting
df_gee <- rbind(do.call("rbind", Meth_results), do.call("rbind", VANC_results))  %>% 
  mutate(time_period = factor(time_period, 
                              levels = c("pre-pandemic", "pandemic"), 
                              labels = c("2007-2019", "2020-2022")), 
         drug = ifelse(organismofinterest == "Staphylococcus aureus", "Methicillin", "Vancomycin"), 
         organismofinterest = factor(organismofinterest, levels = bugs_ordered), 
         org = organismofinterest, 
         type = factor(type, levels = c("Resistant phenotype", "Susceptible phenotype", "Resistance proportion")))

# Plot 
(SA_GEE_results_plot <- ggplot(df_gee %>% filter(variable == "overall", organismofinterest == "Staphylococcus aureus"), 
                            aes(x=time.trend, y=rev(time_period))) +
    facet_nested( ~ drug + type, scales = "free", switch="y") + 
    geom_vline(xintercept = 0, linetype=2, linewidth=2., color="darkred")+ 
    geom_errorbar(aes(xmin=asymp.LCL, xmax=asymp.UCL, color=time_period), width=0., linewidth=2.5) +
    geom_point(aes(shape=time_period, fill=time_period, color = time_period), stroke=1, size=5.5) + 
    labs(x = "Average annual percentage change (%)", 
         linetype="Time period", 
         shape = "Time period", 
         color = "Time period", 
         fill = "Time period") + 
    # scale_x_continuous(breaks = seq(-25, 100, by=10)) +
    scale_shape_manual(values=c(19, 19)) +
    scale_fill_manual(values=c("black", "#696969")) + 
    scale_color_manual(values=c("black", "#696969")) + 
    theme_template() + 
    theme(legend.position = "none", 
          axis.title.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          strip.text = element_text(face = "bold", size = 28), 
          strip.text.y.left = element_text(angle = 0),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=26), 
          axis.title.x = element_text(size=28, face="plain"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), 
          plot.margin = margin(0.5,0,0,0, 'cm')))

(ENTFAES_GEE_results_plot <- ggplot(df_gee %>% filter(variable == "overall", organismofinterest == "Enterococcus faecalis"), 
                            aes(x=time.trend, y=rev(time_period))) +
    facet_nested( ~ drug + type, scales = "free", switch="y") + 
    geom_vline(xintercept = 0, linetype=2, linewidth=2., color="darkred")+ 
    geom_errorbar(aes(xmin=asymp.LCL, xmax=asymp.UCL, color=time_period), width=0., linewidth=2.5) +
    geom_point(aes(shape=time_period, fill=time_period, color = time_period), stroke=1, size=5.5) + 
    labs(x = "Average annual percentage change (%)", 
         linetype="Time period", 
         shape = "Time period", 
         color = "Time period", 
         fill = "Time period") + 
    # scale_x_continuous(breaks = seq(-25, 100, by=10)) +
    scale_shape_manual(values=c(19, 19)) +
    scale_fill_manual(values=c("black", "#696969")) + 
    scale_color_manual(values=c("black", "#696969")) + 
    theme_template() + 
    theme(legend.position = "none", 
          axis.title.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          strip.text = element_text(face = "bold", size= 28), 
          strip.text.y.left = element_text(angle = 0),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=26), 
          axis.title.x = element_text(size=28, face="plain"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), 
          plot.margin = margin(0.5,0,0,0, 'cm')))


(ENTFAEM_GEE_results_plot <- ggplot(df_gee %>% filter(variable == "overall", organismofinterest == "Enterococcus faecium"), 
                            aes(x=time.trend, y=rev(time_period))) +
    facet_nested( ~ drug + type, scales = "free", switch="y") + 
    geom_vline(xintercept = 0, linetype=2, linewidth=2., color="darkred")+ 
    geom_errorbar(aes(xmin=asymp.LCL, xmax=asymp.UCL, color=time_period), width=0., linewidth=2.5) +
    geom_point(aes(shape=time_period, fill=time_period, color = time_period), stroke=1, size=5.5) + 
    labs(x = "Average annual percentage change (%)", 
         linetype="Time period", 
         shape = "Time period", 
         color = "Time period", 
         fill = "Time period") + 
    scale_x_continuous(n.breaks = 8) +
    scale_shape_manual(values=c(19, 19)) +
    scale_fill_manual(values=c("black", "#696969")) + 
    scale_color_manual(values=c("black", "#696969")) + 
    theme_template() + 
    theme(legend.position = "bottom", 
          legend.text = element_text(size=28),
          axis.title.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          strip.text = element_text(face = "bold", size = 28), 
          strip.text.y.left = element_text(angle = 0),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=26), 
          axis.title.x = element_text(size=28, face="plain"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), 
          plot.margin = margin(0.5,0,0,0, 'cm')))

# ---------------------------------------------------------------------------- #
# Combine plots
# ---------------------------------------------------------------------------- #
GEE_figure <- ggpubr::ggarrange(SA_GEE_results_plot, ENTFAES_GEE_results_plot, ENTFAEM_GEE_results_plot,
                                ncol = 1, nrow = 3) + theme(plot.margin = margin(0,0,0,1.6, "cm"))

onset_figure <- ggpubr::ggarrange(SA_onset_plot, ENTFAES_onset_plot, ENTFAEM_onset_plot,
                    ncol = 1, nrow = 3) 
figure <- ggpubr::annotate_figure(onset_figure,
                                        left = ggpubr::text_grob("Resistance proportion", rot = 90, size=32), 
                                        right = ggpubr::text_grob("Incidence per 1,000 admissions", rot=270, 
                                                                  size=32, margin(0.5,0,0,0.2,unit="cm"))
) + theme(plot.margin = margin(0,0.6,0,0, "cm"))

final_figure <- ggpubr::ggarrange(figure, GEE_figure, 
                                  ncol = 2, 
                                  labels="AUTO", 
                                  align="v",
                                  font.label=list(size=30), 
                                  widths = c(1.5, 1), 
                                  heights = c(1.5, 0.2))

ggsave(final_figure, file= "figures/mind_aim1_fig2_SA_ENTFAES_ENTFAEM_onset_AB.pdf", 
       width = 48, height=26)
ggsave(final_figure, file= "figures/mind_aim1_fig2_SA_ENTFAES_ENTFAEM_onset_AB.eps", 
       width = 48, height=26)
ggsave(final_figure, file= "figures/mind_aim1_fig2_SA_ENTFAES_ENTFAEM_onset_AB.png", 
       width = 48, height=26)
