# DMC network formation

DMC is a network formation algorithm consisting of three steps: duplication, mutation, and complementation. We coded the generative algorithm and developed simulations to explore the network's growth.

## Algorithm
The algorithm can be found in the `functions.R` file.
Let G be an empty undirected graph with N nodes and |E| edges. 

### Step 1: Duplication
Let v be a new node entering G. We roll a fair die with N sides to decide an _anchor_ node, a. We then assign all of the neighbors of $a$ to be neighbors of a. In the real world, this might be a new kid (Srikar) coming to school and randomly sitting next to someone in class (Joe). Srikar gets to meet all of Joe's friends (Tanique), so Srikar forms an edge between Joe's friends.

### Step 2: Mutation
Now, v has the same neighbors as a. For each of the nodes in the set of neighbors, we will break an edge with probability q_m. We then flip a fair coin to decide whether we break the edge between the neighbor and v or the neighbor and a. So, Tanique decides if she wants to keep her friend group at the same size or expand it. If she decides to keep it small, she randomly decides who to break the link with.

### Step 3: Complementation
Flip a coin with probability of heads q_c; if we flip a heads, we add an edge between a and v. Otherwise, we don't add an edge between the two.
