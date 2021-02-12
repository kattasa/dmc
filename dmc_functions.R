## Srikar Katta
## DMC Network Formation functions
## Feb 11, 2021


## packages
rm(list = ls())
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




## algorithm ----
dmc_algo <- function(n, qm, qc) {
  # parameters:
  #   n: number of nodes in network
  #   qm: probability of breaking link between new/anchor node and neighbors; (1 - qm) is prob(forming link)
  #   qc: probability of breaking link between new and anchor node; (1 - qc) is prob(forming link)
  
  ## initialize network with 1 node
  ## network represented as hash table with keys as node labels
  edge_list <- list()
  edge_list[['1']] <- c()
  
  for(i in seq(2, n)){
    # i is the new node entering graph
    new_node <- as.character(i)
    ## duplication step ----
    anchor_node <- sample(seq(i - 1), 1, replace = FALSE) %>% as.character() # select anchor at random
    neighbor_nodes <- edge_list[[anchor_node]] # copy anchor node neighbors to new node
    
    ## mutation step ---- 
    edge_cut <- rbinom(length(neighbor_nodes), 1, qm) # decide which edges to cut; 0 if keep, 1 if cut
    anchor_new_cut <- rbinom(length(neighbor_nodes), 1, 0.5) # 0 if cut from new node, 1 if cut from anchor node
    anchor_neighbors_cut <- edge_cut & anchor_new_cut # edge is cut (edge_cut == 1) from anchor's neighbors (anchor_new_cut == 1)
    new_neighbors_cut <- edge_cut & !anchor_new_cut # edge is cut (edge_cut == 1) from new's neighbors (anchor_new_cut == 0)
    anchor_node_neighbors <- neighbor_nodes[which(!anchor_neighbors_cut)] # add nodes that we keep 
    new_node_neighbors <- neighbor_nodes[which(!(new_neighbors_cut))] # new node neighbors
    
    ## complementation step ----
    if(rbinom(1, 1, 1 - qc)){ # if forming a link (1 - qc because we form a link instead of breaking it)
      anchor_node_neighbors <- c(anchor_node_neighbors, new_node) # add new node to anchor's neighbor list
      new_node_neighbors <- c(new_node_neighbors, anchor_node) # add anchor node to new's neighbor list
    }
    
    ## update hashtable ----
    edge_list[[anchor_node]] <- anchor_node_neighbors
    edge_list[[new_node]] <- new_node_neighbors
  }
  edge_list %>% nest_list_to_df() %>% return()
}

