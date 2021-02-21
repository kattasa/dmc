## Srikar Katta
## DMC Network Formation simulations: comparing node degree and age
## Feb 20, 2021

# libraries and packages ----
rm(list = ls())
source('dmc_functions.R')
library(tidyverse)

# set parameters ----
n <- 100 # number of nodes in graph
qm_interval <- 0.05 # interval to increment qm by
qc_interval <- 0.05 # interval to increment qc by

# create dataframe of simulated graphs
dmc_df <- sim_qmqc_values(n, qm_interval, qc_interval)
dmc_age_deg <- graphs_age_deg_cor(dmc_df, n, qm_interval, qc_interval)

## calculate correlation between node age and node degree --- 
age_deg_cor_mat <- ggplot(dmc_age_deg) +
  geom_tile(aes(x = qm, y = qc, fill = cor_age_deg)) +
  labs(x = 'Probability of severing neighbor ties - qm',
       y = 'Probability of forming anchor tie - qc',
       title = 'Correlation Matrix Between Node Age and Degree',
       subtitle = 'Grouped by different qm and qc values',
       fill = 'Correlation:\nAge vs Degree') +
  theme_minimal()
  
  
