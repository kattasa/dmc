## Srikar Katta
## DMC Network Formation functions
## Feb 11, 2021


## packages
# rm(list = ls())
library(igraph)


## turn nested list into a dataframe ----
nest_list_to_df <- function(edge_list) {
  # parameters:
  #   edge_list: nested list
  
  ## initialize lists to make into a dataframe ---
  return_list <- list()
  from <- c() # source node
  to <- c() # destination node
  
  ## turn into a dataframe ---
  for(i in names(edge_list)) {
    for(j in edge_list[[i]]) {
      from <- c(from, i)
      to <- c(to, j)
    }
  }
  ## export edge list ----
  data.frame(from, to) %>% return()
}




# ## algorithm ----
# dmc_algo <- function(n, qm, qc) {
#   # parameters:
#   #   n: number of nodes in network
#   #   qm: probability of breaking link between new/anchor node and neighbors; (1 - qm) is prob(forming link)
#   #   qc: probability of forming link between new and anchor node
#   
#   ## initialize network with 1 node
#   ## network represented as hash table with keys as node labels
#   edge_list <- list()
#   edge_list[['1']] <- c()
#   
#   
#   for(i in seq(2, n)){
#     # i is the new node entering graph
#     new_node <- as.character(i)
#     
#     ## duplication step ----
#     anchor_node <- sample(seq(i - 1), 1, replace = FALSE) %>% as.character() # select anchor at random
#     neighbor_nodes <- edge_list[[anchor_node]] # copy anchor node neighbors to new node
#     
#     ## mutation step ---- 
#     edge_cut <- rbinom(length(neighbor_nodes), 1, qm) # decide which edges to cut; 0 if keep, 1 if cut
#     anchor_new_cut <- rbinom(length(neighbor_nodes), 1, 0.5) # 0 if cut from new node, 1 if cut from anchor node
#     anchor_neighbors_cut <- edge_cut & anchor_new_cut # edge is cut (edge_cut == 1) from anchor's neighbors (anchor_new_cut == 1)
#     new_neighbors_cut <- edge_cut & !anchor_new_cut # edge is cut (edge_cut == 1) from new's neighbors (anchor_new_cut == 0)
#     anchor_node_neighbors <- neighbor_nodes[which(!anchor_neighbors_cut)] # add nodes that we keep 
#     new_node_neighbors <- neighbor_nodes[which(!(new_neighbors_cut))] # new node neighbors
#     
#     ## complementation step ----
#     if(rbinom(1, 1, qc)){ # if forming a link
#       anchor_node_neighbors <- c(anchor_node_neighbors, new_node) # add new node to anchor's neighbor list
#       new_node_neighbors <- c(new_node_neighbors, anchor_node) # add anchor node to new's neighbor list
#     }
#     
#     ## update hashtable ----
#     edge_list[[anchor_node]] <- anchor_node_neighbors
#     edge_list[[new_node]] <- new_node_neighbors
#     
#     
#   }
#   edge_list %>% 
#     # nest_list_to_df() %>%
#     return()
# }

dmc_algo <- function(n, qm, qc){
  # parameters:
  #   n: number of nodes in network
  #   qm: probability of breaking link between new/anchor node and neighbors; (1 - qm) is prob(forming link)
  #   qc: probability of forming link between new and anchor node
  # returns:
  #   adjacency matrix with upper triangle filled in (because of symmetry in undirected networks)
  
  # initialize adjacency matrix and list of anchor nodes
  adjacency_matrix <- matrix(data = 0, nrow = n, ncol = n)
  anchor_seq <- c(NA) # NA for first node to enter graph
  
  for(i in seq(2, n)){
    ## duplication step ----
    anchor_node <- sample(seq(i - 1), 1, replace = FALSE) # select anchor at random
    adjacency_matrix[, i] <- adjacency_matrix[, anchor_node] # copy anchor node neighbors to new node
    
    ## mutation step ----
    edge_cut <- rbinom(n, 1, qm) # decide which edges to cut; 0 if keep, 1 if cut
    anchor_new_cut <- rbinom(n, 1, 0.5) # 0 if cut from new node, 1 if cut from anchor node
    anchor_neighbors_cut <- edge_cut * anchor_new_cut # edge is cut (edge_cut == 1) from anchor's neighbors (anchor_new_cut == 1)
    new_neighbors_cut <- edge_cut * (1 - anchor_new_cut) # edge is cut (edge_cut == 1) from new's neighbors (anchor_new_cut == 0)
    adjacency_matrix[, anchor_node] <- adjacency_matrix[, anchor_node] * (1 - anchor_neighbors_cut) # add nodes that we keep
    adjacency_matrix[, i] <- adjacency_matrix[, i] * (1 - new_neighbors_cut) # new node neighbors
    adjacency_matrix[anchor_node, ] <- adjacency_matrix[, anchor_node] # make matrix symmetric
    adjacency_matrix[i, ] <- adjacency_matrix[, i] # make matrix symmetric
    
    ## complementation step ----
    adjacency_matrix[anchor_node, i] <- rbinom(1, 1, qc) # add edge between new node and anchor node with prob qc
    adjacency_matrix[i, anchor_node] <- adjacency_matrix[anchor_node, i] # make matrix symmetric
    
    # add anchor node to sequence
    anchor_seq <- c(anchor_seq, anchor_node)
  }
  list(adjacency_matrix = adjacency_matrix, anchor_seq = anchor_seq) %>% return()
}

sim_qmqc_values <- function(n, qm_interval, qc_interval, n_times){
  # parameters:
  #   n: number of nodes in graph
  #   qm_interval: interval to increment qm by
  #   qc_interval: interval to increment qc by
  
  # initialize empty dataframe
  dmc_df <- data.frame(from = NULL, to = NULL, qm = NULL, qc = NULL, n = NULL, n_times = NULL)
  # find network for each qm and qc value
  for(i in seq(n_times)){
    for(qm in seq(0, 1, qm_interval)){
      for(qc in seq(0, 1, qc_interval)){
        dmc_df <- dmc_algo(n, qm = qm, qc = qc) %>%
          mutate(qm = qm, # add information to identify graph
                 qc = qc, 
                 n = n,
                 nth = i) %>%
          rbind(dmc_df)
      }
    } 
  }
  return(dmc_df)
}

graphs_age_deg_cor <- function(dmc_df, n, qm_interval, qc_interval){
  # parameters:
  #   n: number of nodes in graph
  #   qm_interval: interval to increment qm by
  #   qc_interval: interval to increment qc by
  
  # create array of all possible nodes and qm/qc values
  dmc_grid <- expand.grid(from = seq(1, n) %>% as.character(),
                          qm = seq(0, 1, qm_interval),
                          qc = seq(0, 1, qc_interval))
  dmc_df %>%
    group_by(qm, qc, from, nth) %>%
    summarise(degree = n()) %>% # calculate degree per node
    full_join(dmc_grid) %>% # expand graphs to include nodes with 0 edges
    replace_na(replace = list(degree = 0)) %>% # replace NA degrees with 0
    mutate(age = as.numeric(from)) %>% # change from to be numeric for correlation calcs
    group_by(qm, qc) %>%
    summarize(cor_age_deg = cor(age, degree)) %>%
    return()
}

