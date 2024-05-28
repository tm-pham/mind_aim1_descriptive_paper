# Trends in Infection Incidence and Antimicrobial Resistance in the US Veterans Affairs Healthcare System: A Nationwide Study (2007-2022)

Corresponding author: Thi Mui Pham (tmpham@hsph.harvard.edu)

This repository accompanies the manuscript 

*Trends in Infection Incidence and Antimicrobial Resistance in the US Veterans Affairs Healthcare System: A Nationwide Study (2007-2022)*. 

Thi Mui Pham, PhD<sup>1,2,5</sup>, Yue Zhang, PhD<sup>3</sup>, McKenna Nevers, MS<sup>3,4</sup>, Haojia Li, MS<sup>3</sup>, Karim Khader, PhD<sup>3,4</sup>, Yonatan H. Grad, MD<sup>2,5</sup>, Marc Lipsitch, DPhil<sup>1,2</sup>, Matthew Samore, MD<sup>3,4</sup>

<sub>
1 Department of Epidemiology, Harvard T.H. Chan School of Public Health<br>
2 Center for Communicable Disease Dynamics, Harvard T.H. Chan School of Public Health<br>
3 Department of Internal Medicine, Division of Epidemiology, Spencer Fox Eccles School of Medicine, University of Utah<br>
4 IDEAS Center of Innovation, Veterans Affairs Salt Lake City Health Care System, Salt Lake City, Utah<br>
5 Department of Immunology and Infectious Diseases, Harvard T.H. Chan School of Public Health<br>
</sub>
<br>

We provide the code for

- Main figures of the manuscript
- reproducing the GEE analysis. 

Individual-level patient data cannot be provided due to VA Privacy Practices. A data dictionary that represents the original data will be provided along with supporting documentation (statistical/analytic code) for the generalized estimating equations (GEE) analysis). The corresponding data and scripts can be found in the folders `data` and `code`, respectively. 

## Code for reproducing figures
### Main figures
The files to reproduce the main figures 1-5 are located in the folder `code`. These files can be run with the data provided in the repository. 

`mind_aim2-1_fig1.R`<br>
`mind_aim2-1_fig2_MRSA_ENTFAES_ENTFAEM.R`<br>
`mind_aim2-1_fig3_FQL.R`<br>
`mind_aim2-1_fig4_CPH03.R`<br>
`mind_aim2-1_fig5_CPM.R`<br>

### Figures in appendix
The remaining files are to reproduce the results in the appendix. 

### Helper functions
`plotting_template.R`: This file defines a template (theme) for plotting in R. <br>
`mind_global_variables.R`: This file defines the colors and the order of the pathogens. Mainly used for plotting. <br>

## Code for the GEE analysis
The files that were used for the GEE analysis are located in the folder `code`:

`mind_aim2-1_function_inc_gee`: This function is used to run a GEE analysis for infection incidence and phenotypic incidence. <br>
`mind_aim2-1_function_res_prop_gee`: This function is used to run a GEE analysis for resistance proportion. <br>
`mind_aim2-1_runfile_inc_gee`: This script produces the GEE time trend results for infection incidence for each pathogen. <br>
`mind_aim2-1_runfile_res_inc_gee`: This script produces the GEE time trend results for phenotypic incidence for each pathogen-drug combination. <br>
`mind_aim2-1_runfile_res_prop_gee`: This script produces the GEE time trend results for resistance proportion for each pathogen-drug combination. <br>

### Helper functions
`mind_aim2-1_function_df_inc.R`: This function is a helper function to compute the incidence of a pre-specified outcome. <br>
`mind_aim2-1_function_aapc.R`: This function is a helper function to compute the AAPC based on the output of the GEE analysis. <br>
`packages.R`: This file loads all necessary packages. <br>

## Data for GEE analysis
Individual-level patient and facility data cannot be provided due to VA Privacy Practices. Data scaffolds, i.e., empty dataset with the column names present for the GEE analysis are provided in `data/data_scaffolds/`. They represent the expected data structure for the previously described scripts for running the GEE analyses. Corresponding data dictionaries are provided in `data/data_dictionaries/`. 




