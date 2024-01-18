# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# MInD Aim 1 manuscript
# Title: Figure 1
# ============================================================================ #
# This file reproduces Figure 1 in the main text of the manuscript
# Data Preparation: Lines 12-37
# Figure 1A: Lines 38-78
# Figure 1B: Lines 80-138
# Figure 1C: Lines 142-200
# ============================================================================ #
setwd("/Users/tm-pham/academia/hsph/mind/publications/aim1")
source("code/packages.R")
source("code/plotting_template.R")

# Organisms of interest
bugs_ordered <- c("Staphylococcus aureus", 
                  "Enterococcus faecalis", 
                  "Enterococcus faecium", 
                  "Escherichia coli", 
                  "Klebsiella pneumoniae", 
                  "Enterobacter cloacae", 
                  "Serratia marcescens", 
                  "Pseudomonas aeruginosa", 
                  "Acinetobacter Sp.")

# Colors for each organism
colors_vector <- c("#FAA755",  # Muted Burnt Orange (Gram-positive cocci)
                   "#F2BF77",  # Soft Gold (Gram-positive cocci)
                   "#FAE3AF",  # Soft Cream (Gram-positive cocci)
                   "#336B67",  # Teal (Enterobacterales)
                   "#67A8A6",  # Soft Cyan (Enterobacterales)
                   "#A1D0C9",  # Pale Turquoise (Enterobacterales)
                   "#D4EDEE",  # Very Light Cyan (Enterobacterales)
                   "#E58F8F",  # Muted Salmon (Gram-negative non-fermenters)
                   "#FAC3C3")  # Soft Rose (Gram-negative non-fermenters)

# ---------------------------------------------------------------------------- #
# FIGURE 1A 
# ---------------------------------------------------------------------------- #
# Load data 
# Number of 30-day incident isolates and number of admissions per quarter
df_inc_qrt_overall <- read.csv("data/mind_aim1_incidence_overall_by_org_quarterly.csv")

# Overall incidence per year
df_inc_year_overall <- df_inc_qrt_overall %>% 
  mutate(date_year = year(yq(date_qrt))) %>% 
  group_by(organismofinterest, date_year) %>%
  summarize(n_inc = sum(n_inc), 
            n_adm = sum(n_adm), 
            inc = 1000*n_inc/(n_adm)) %>% 
  mutate(organismofinterest = factor(organismofinterest, levels = rev(bugs_order)))

# Minimum and maximum year, used for figures 1B and C as well
min_date <- min(df_inc_year_total$date_year)
max_date <- max(df_inc_year_total$date_year)

# Plot
figure1A <- ggplot(df_inc_year_overall %>% 
                     filter(organismofinterest != "Total"), 
                   aes(x = ymd(date_year, truncated=2), y = inc, fill = organismofinterest)) + 
  geom_bar(stat="identity", position = "stack") + 
  scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0(min_date, "-01-01")), 
                                                     to = as.Date(paste0(max_date, "-01-01")), by = "year")) + 
  labs(y = "Total number of incident isolates per 1,000 admissions", subtitle="A") + 
  scale_fill_manual(values = rev(colors_vector)) + 
  theme_template_time() + 
  theme(legend.title = element_blank(), 
        legend.position = "none", 
        plot.subtitle = element_text(hjust = -0.1),
        legend.text = element_text(size=24),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25), 
        axis.title.y = element_text(size=25, face = "plain"), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())

ggsave(figure1A, file = "figures/mind_aim1_figure1A.pdf", width=18, height=9.2)

# ---------------------------------------------------------------------------- #
# FIGURE 1B
# ---------------------------------------------------------------------------- #
# Load data (GEE results per species)
# It contains the following variables (among others):
# organismofinterest: species of interest
# pandemic_flag: Time period (2007-2019 - pre-pandemic, 2020-2022 - pandemic)
# onset: Location of infection onset
# time.trend: time trend coefficient from GEE analysis
# lower95: lower bound of 95% confidence interval
# upper95: upper bound of 95% confidence interval
inc_gee_results_org_onset <-readxl::read_excel("data/mind_aim1_inc_gee_results_by_org.xlsx")

# Create data frame for plotting 
df_gee_results <- inc_gee_results_org_onset %>% filter(onset=="overall") %>% 
  rename(organismofinterest = org,
         pandemic_flag = variable, 
         lower95 = asymp.LCL, 
         upper95 = asymp.UCL) %>% 
  mutate(organismofinterest = factor(organismofinterest, levels=rev(bugs_ordered)),
         pandemic_flag = factor(pandemic_flag,
                                levels=c("pre-pandemic", "pandemic"), 
                                labels = c("2007-2019", "2020-2022")))

# Plot (but without colors)
gee_results_plot <- ggplot(df_gee_results, aes(x=time.trend, y=rev(pandemic_flag))) +
  facet_grid(rows=vars(organismofinterest), switch="y") + 
  geom_vline(xintercept = 0, linetype=2, linewidth=2., color="darkred")+ 
  geom_errorbar(aes(xmin=lower95, xmax=upper95, linetype=pandemic_flag, color=pandemic_flag), width=0., linewidth=1.5) +
  geom_point(aes(shape=pandemic_flag, fill=pandemic_flag, color = pandemic_flag), stroke=2, size=5) + 
  labs(x = "Average annual percentage change (%)", 
       linetype="Time period", shape = "Time period", color = "Time period", fill = "Time period") + 
  scale_x_continuous(limits = c(-11, 40), breaks = seq(-11, 60, by=5)) +
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
        axis.text.x = element_text(size=26), 
        axis.title.x = element_text(size=28, face="plain"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) + 
  coord_cartesian(xlim = c(-11, NA), clip = "off")

# Make colored facets
figure1B <- ggplot_gtable(ggplot_build(gee_results_plot))
strips <- which(grepl('strip-', g$layout$name))
pal <- rev(colors_vector)
for (i in seq_along(strips)) {
  k <- which(grepl('rect', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
  l <- which(grepl('titleGrob', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
  g$grobs[[strips[i]]]$grobs[[1]]$children[[k]]$gp$fill <- pal[i]
}

# Save plot
ggsave(figure1B, file = "figures/mind_aim1_figure1B.pdf", width=16, height=10)

# ---------------------------------------------------------------------------- #
# FIGURE 1C
# ---------------------------------------------------------------------------- #
# Load incidence data by location of infection onset and by species
df_inc_qrt_onset <- read.csv("data/mind_aim1_incidence_by_onset_org_quarterly.csv")

# Create yearly incidence data
df_inc_year_onset <- df_inc_qrt_onset %>% 
  mutate(date_year = year(yq(date_qrt))) %>% 
  group_by(organismofinterest, date_year, onset) %>%
  summarize(n_inc = sum(n_inc), 
            n_adm = sum(n_adm), 
            inc = 1000*n_inc/(n_adm))

df_inc_year_onset_total <- df_inc_year_onset %>% 
  group_by(date_year, onset) %>% 
  mutate(organismofinterest = "Total", 
         n_inc = sum(n_inc), 
         n_adm = n_adm[1], 
         inc = 1000*n_inc/n_adm) %>% 
  unique() %>% 
  mutate(organismofinterest = factor(organismofinterest, levels = c(bugs_sorted, "Total")))

# Load data on number of isolates resistant to one or more drugs
df_res_total <-read.csv("data/mind_aim1_n_resistance_one_more_drugs.csv")

df_inc_stack_yr_onset <- df_inc_year_onset_total %>% filter(date_year <2023, date_year>=2007) %>% 
  left_join(df_res_total, by =c("date_year"="year", "onset")) %>% 
  mutate(inc_1R = inc*perc_1R, 
         inc_MR = inc*perc_MR, 
         inc_residual = inc-inc_1R-inc_MR) %>% 
  select(organismofinterest, date_year, onset, inc_1R, inc_MR, inc_residual) %>% 
  melt(id=c("organismofinterest", "date_year", "onset")) %>% 
  mutate(variable = factor(variable, 
                           levels = c("inc_residual", "inc_MR", "inc_1R"), 
                           label = c("Non-resistant to all key antibiotic (classes)", 
                                     "Resistant to two or more",
                                     "Resistant to one")))
# Plot
figure1C <- ggplot(df_inc_stack_yr_onset, 
                   aes(x = ymd(date_year, truncated=2),
                       y = value, group=variable, fill=onset, alpha=variable)) + 
  facet_grid(rows = vars(onset)) + 
  geom_bar(stat="identity", position = "stack") + 
  scale_x_date(date_labels = "%Y", breaks = seq.Date(from = as.Date(paste0(min_date, "-01-01")), 
                                                     to = as.Date(paste0(max_date, "-01-01")), by = "year")) + 
  scale_fill_manual(values = onset_colors) + 
  scale_alpha_discrete(range = c(0.4, 1.0)) + 
  labs(y="Total number of incident isolates per 1,000 admissions") + 
  guides(fill="none") + 
  theme_template_time() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size=22),
        axis.text.x = element_text(size=22), 
        axis.text.y = element_text(size=22), 
        axis.title.y = element_text(size=24))

ggsave(figure1C, file = "figures/mind_aim1_figure1C.pdf", width = 17, height=12)
