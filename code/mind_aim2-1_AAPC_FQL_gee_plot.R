# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# MInD Aim 2.1 manuscript
# Title: Figure 1
# ============================================================================ #
# Plot of Table 1
# ============================================================================ #
setwd("/Users/tm-pham/academia/hsph/mind/")
# Load packages 
source("code/packages.R")

# Load auxiliary files
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

df_FQL_gee <- do.call("rbind", FQL_results) %>% 
  mutate(time_period = factor(time_period, levels = c("pre-pandemic", "pandemic"), labels = c("2007-2019", "2020-2022")), 
         organismofinterest = factor(organismofinterest, levels = bugs_ordered))



# Plot (but without colors)
(FQL_GEE_results_plot <- ggplot(df_FQL_gee %>% filter(variable == "Hospital-associated"), aes(x=time.trend, y=rev(time_period))) +
  facet_grid(rows=vars(organismofinterest), cols = vars(type), switch="y", scales = "free") + 
  geom_vline(xintercept = 0, linetype=2, linewidth=2., color="darkred")+ 
  geom_errorbar(aes(xmin=asymp.LCL, xmax=asymp.UCL, linetype=time_period, color=time_period), width=0., linewidth=1.5) +
  geom_point(aes(shape=time_period, fill=time_period, color = time_period), stroke=2, size=5) + 
  labs(x = "Average annual percentage change (%)", 
       linetype="Time period", shape = "Time period", color = "Time period", fill = "Time period") + 
  scale_x_continuous(limits = c(-100, 100), breaks = seq(-100, 100, by=20)) +
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
  # coord_cartesian(xlim = c(-100, NA), clip = "off"))
ggsave(FQL_GEE_results_plot, file = "figures/mind_aim2-1_FQL_AAPC_gee_plot.pdf", width = 24, height=9)

