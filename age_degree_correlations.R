## Srikar Katta
## DMC Network Formation simulations: comparing node degree and age
## Feb 20, 2021

# libraries and packages ----
rm(list = ls())
source('dmc_functions.R')
library(tidyverse)

# set parameters ----
n <- 100 # number of nodes in graph
qm_interval <- 0.1 # interval to increment qm by
qc_interval <- 0.1 # interval to increment qc by
n_times <- 1 # number of individual graphs to simulate per qm, qc pair

# create dataframe of simulated graphs
dmc_df <- sim_qmqc_values(n, qm_interval, qc_interval, n_times = n_times)
dmc_age_deg <- graphs_age_deg_cor(dmc_df, n, qm_interval, qc_interval)


## calculate correlation between node age and node degree --- 
age_deg_cor_mat <- ggplot(dmc_age_deg) +
  geom_tile(aes(x = qm, y = qc, fill = cor_age_deg)) +
  labs(x = 'Probability of severing neighbor ties - qm',
       y = 'Probability of forming anchor tie - qc',
       title = 'Correlation Matrix Between Node Age and Degree',
       subtitle = 'Grouped by different qm and qc values for networks with 100 nodes',
       caption = '1 Graph per qm, qc pair',
       fill = 'Correlation:\nAge vs Degree') +
  theme_minimal()

## scatter plot of node age against node degree ---
dmc_df %>%
  group_by(from, qm, qc, nth) %>%
  summarise(degree = n()) %>%
ggplot(., aes(x = from, y = degree)) +
  geom_point(size = 0.1) +
  facet_grid(qm ~ qc) +
  labs(x = 'Age', 
       y = 'Degree',
       title = 'Scatter Plots of Node Age and Degree for DMC Simulated Networks',
       subtitle = paste('Grouped by different qm (columns), qc (rows) values for networks with', n, 'nodes'),
       caption = paste0('# Graphs per qm, qc pair: ', n_times)) +
  theme_bw()

## plot of degree distribution ----
ggplot(dmc_df) +
  geom_density(aes(x = from)) +
  facet_grid(qm ~ qc) +
  labs(x = 'Degree', 
       y = 'Density',
       title = 'Degree Distribution for DMC Simulated Networks',
       subtitle = paste('Grouped by different qm (columns), qc (rows) values for networks with', n, 'nodes'),
       caption = paste0('# Graphs per qm, qc pair: ', n_times)) +
  theme_bw()

## scale free network models ----
ba_df <- data.frame(from = NA, to = NA, nth = NA)
for(i in seq(1)){
  ba_df <- sample_pa(1000, power = 1.2, zero.appeal = 1) %>% 
    get.data.frame() %>%
    mutate(nth = i) %>%
    bind_rows(ba_df)
}

## node age vs degree in BA/scale free model ----
ggplot(ba_df) +
  geom_point(aes(x = to), stat = 'count') +
  labs(x = 'Age',
       y = 'Degree',
       title = 'Relating Node Age and Degree in Barabasi Albert Simulated Network',
       subtitle = 'Hyperparameters: 1000 nodes, power = 1.2 and zero.appeal = 1',
       caption = paste0('# Graphs per qm, qc pair: ', n_times)) +
  theme_classic()

## degree distribution of node age vs node degree ----
ba_df %>%
  group_by(to) %>%
  summarise(degree = n()) %>%
ggplot() +
  geom_bar(aes(x = degree), stat = 'count') +
  labs(x = 'Age',
       y = 'Degree',
       title = 'Relating Node Age and Degree in Barabasi Albert Simulated Network',
       subtitle = 'Hyperparameters: 1000 nodes, power = 1.2 and zero.appeal = 1',
       caption = paste0('# Graphs per qm, qc pair: ', n_times)) +
  theme_classic()


  
