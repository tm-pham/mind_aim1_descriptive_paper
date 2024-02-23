# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# MInD Aim 2.1 manuscript
# Title: Figure 4
# 3rd gen CPH resistance
# ============================================================================ #
setwd("/Users/tm-pham/academia/hsph/mind/publications/aim1")
source("code/mind_colors.R")
source("code/packages.R")

# Load data 
load("data/bugs_drugs.RData")
load("data/mind_CPH03_res_inc_prop_yr_data.RData") # df_CPH03_inc_yr

# Global variables 
susc_colors = c("#35978F", "#800000", "#888888")
fill_color ="#eda361"

comb_plot <- NULL
drug = "CPH_03_class"
scale_sec_axis_vec <- c(23.5, 15.6, 4.25, 2.95, 11.7, 2.05)

# ---------------------------------------------------------------------------- #
# E. coli
# ---------------------------------------------------------------------------- #
(comb_plot_1 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_CPH03_res_prop_yr %>% filter(organismofinterest=="Escherichia coli"), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_CPH03_inc_yr %>% filter(organismofinterest=="Escherichia coli"), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[1], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_CPH03_inc_yr %>% filter(organismofinterest=="Escherichia coli"), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[1], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "3rd generation cephalosporin susceptibility test result") + 
    scale_y_continuous(name = "", 
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels = scales::percent, breaks = seq(0, 1, 0.2), 
                       sec.axis = sec_axis( trans=~.*scale_sec_axis_vec[1])) +
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2008-01-01")), 
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
# Klebsiella
# ---------------------------------------------------------------------------- #
(comb_plot_2 <- ggplot() + 
    facet_grid(.~organismofinterest) +
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_CPH03_res_prop_yr %>% filter(organismofinterest=="Klebsiella pneumoniae"), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_CPH03_inc_yr %>% filter(organismofinterest=="Klebsiella pneumoniae"), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[2], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_CPH03_inc_yr %>% filter(organismofinterest=="Klebsiella pneumoniae"), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[2], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "3rd generation cephalosporin susceptibility test result") + 
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
    geom_bar(data = df_CPH03_res_prop_yr %>% filter(organismofinterest=="Enterobacter cloacae"), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_CPH03_inc_yr %>% filter(organismofinterest=="Enterobacter cloacae"), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[3], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_CPH03_inc_yr %>% filter(organismofinterest=="Enterobacter cloacae"), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[3], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "3rd generation cephalosporin susceptibility test result") + 
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
    geom_bar(data = df_CPH03_res_prop_yr %>% filter(organismofinterest=="Serratia marcescens"), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_CPH03_inc_yr %>% filter(organismofinterest=="Serratia marcescens"), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[4], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_CPH03_inc_yr %>% filter(organismofinterest=="Serratia marcescens"), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[4], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "3rd generation cephalosporin susceptibility test result") + 
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
          axis.text.x = element_text(size=16), 
          axis.text.y.left = element_text(size=16), 
          axis.text.y.right = element_text(size=16), 
          axis.title = element_blank(), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# Pseudomonas
# ---------------------------------------------------------------------------- #
(comb_plot_5 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_CPH03_res_prop_yr %>% filter(organismofinterest==bugs[7]), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_CPH03_inc_yr %>% filter(organismofinterest==bugs[7]), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[5], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_CPH03_inc_yr %>% filter(organismofinterest==bugs[7]), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[5], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "3rd generation cephalosporin susceptibility test result") + 
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
          axis.title = element_blank(), 
          axis.text.x = element_text(size=16),
          axis.text.y.left = element_text(size=16), 
          axis.text.y.right = element_text(size=16), 
          plot.margin=unit(c(0.1,0.,0.1,0),"cm")))

# ---------------------------------------------------------------------------- #
# Acinetobacter Sp.
# ---------------------------------------------------------------------------- #
(comb_plot_6 <- ggplot() + 
    facet_grid(.~organismofinterest) + 
    geom_vline(xintercept=as.numeric(as.Date("2019-10-30", format="%Y-%m-%d")), 
               linetype="dashed", color="black", linewidth=1.2) + 
    geom_bar(data = df_CPH03_res_prop_yr %>% filter(organismofinterest==bugs[1]), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_CPH03_inc_yr %>% filter(organismofinterest==bugs[1]), 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis_vec[6], 
                   color = eval(parse(text=drug))), 
               size=4.5) + 
    geom_smooth(data = df_CPH03_inc_yr %>% filter(organismofinterest==bugs[1]), 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis_vec[6], 
                    color = eval(parse(text=drug))), 
                linewidth=2) + 
    labs(color = "3rd generation cephalosporin susceptibility test result") + 
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
          axis.title = element_blank(), 
          axis.text.x = element_text(size=16),
          axis.text.y.left = element_text(size=16), 
          axis.text.y.right = element_text(size=16), 
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

# ggsave(final_plot, file="figures/mind_epidemics_figure4_CPH03_res_prop_comb_plot.pdf", 
#        bg='transparent',
#        width=20, height=10)


# E. coli onset plot
scale_sec_axis <- 15
(EC_comb_plot <- ggplot() + 
    facet_nested(~ organismofinterest + onset) + 
    # facet_grid(cols=vars(onset), rows = vars(organismofinterest), scales = "free", switch="y") +
    # facet_wrap(.~onset) +
    geom_vline(xintercept=as.numeric(as.Date("2019-08-30", format="%Y-%m-%d")), linetype="dashed", color="black",
               linewidth=1.3) +
    geom_bar(data = res_prop_yr$`Escherichia coli`$onset2 %>% 
               filter(variable == "CPH_03_class") %>%
               mutate(organismofinterest = "Escherichia coli"), 
             aes(x = ymd(date_year, truncated=2), 
                 y =  prop_wNA), 
             stat="identity", color="black", fill=fill_color, alpha=1.5) + 
    geom_point(data = df_inc_yr$`Escherichia coli`$CPH_03_class$onset2, 
               aes(x = ymd(date_year, truncated=2), 
                   y =  inc/scale_sec_axis, 
                   color = eval(parse(text="CPH_03_class"))), 
               size=4.5) + 
    geom_smooth(data = df_inc_yr$`Escherichia coli`$CPH_03_class$onset2, 
                aes(x = ymd(date_year, truncated=2), 
                    y =  inc/scale_sec_axis, 
                    color = eval(parse(text="CPH_03_class"))), 
                linewidth=2) + 
    labs(color = "3rd generation cephalosporin susceptibility test result") + 
    scale_y_continuous(name = "Resistance proportion",
                       limits = c(0, 1), 
                       expand = c(0, 0), 
                       labels=scales::percent, breaks=seq(0,1,by=0.2), 
                       sec.axis = sec_axis(trans=~.*scale_sec_axis, name="Incidence per 1,000 admissions")) +
    scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0("2007-01-01")), 
                                                       to = as.Date(paste0("2022-12-31")), by = "2 years")) + 
    scale_color_manual(values=susc_colors, labels = c("S", "R", "Missing")) + 
    theme_template_time() + 
    theme(axis.title.y = element_text(size=25), 
          axis.text.y = element_text(size=22),
          axis.text.x = element_text(size=22),
          strip.text.x = element_text(face="bold", size=26),
          strip.text.x.top = element_text(face="bold.italic", size=26, margin = margin(0.4,0,0.4,0, "cm")), 
          legend.text = element_text(size=24), 
          legend.title = element_text(size=24)))

# Combined plot for manuscript
figure4 <- ggpubr::ggarrange(final_plot, EC_comb_plot_2, nrow=2, align="v",
                             heights = c(1.8, 1.3),
                             labels="AUTO", font.label=list(size=30))
ggsave(figure4, file="figures/mind_aim2-1_fig4_CPH03.pdf", 
       width=20, height=18)

