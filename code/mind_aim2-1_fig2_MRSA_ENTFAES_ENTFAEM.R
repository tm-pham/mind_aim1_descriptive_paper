# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# Figure 2 of descriptive paper
# S. aureus and E. faecalis, zE. faecium
# ============================================================================ #
setwd("/Users/tm-pham/academia/hsph/mind/publications/aim1/")
source("code/packages.R")
source("code/mind_colors.R")
source("code/plotting_template.R")

# Load data 
load("data/bugs_drugs.RData")
load("data/mind_incidence_data.RData")
load("data/mind_incidence_qrt_data.RData")
load("data/mind_df_inc_res_prop_yr_data_new.RData")

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
# Combine plots
# ---------------------------------------------------------------------------- #

figure <- ggpubr::ggarrange(SA_onset_plot, ENTFAES_onset_plot, ENTFAEM_onset_plot,
                    ncol = 1, nrow = 3) 
final_figure <- ggpubr::annotate_figure(figure,
                                left = ggpubr::text_grob("Resistance proportion", rot = 90, size=32), 
                                right = ggpubr::text_grob("Incidence per 1,000 admissions", rot=270, size=32, margin(0.5,0,0,0.2,unit="cm"))
) + theme(plot.margin = margin(0,0.6,0,0, "cm"))

ggsave(final_figure, file= "figures/mind_aim1_fig2_SA_ENTFAES_ENTFAEM_onset.pdf", 
       width = 26, height=28)
