library(igraph)

# http://horicky.blogspot.com/2012/04/basic-graph-analytics-using-igraph.html
# revised by chengjun wang @ Tencent 2014/04/16
##################
# generate graphs
##################
# Create a directed graph
g = graph(c(1,2, 2,3, 2,4, 2,5), directed=TRUE)
g
plot(g)

# Create a directed graph using adjacency matrix
m = matrix(runif(4*4), nrow=4)
m
g = graph.adjacency(m > mean(m))
g

plot(g, layout=layout.fruchterman.reingold)

#Create a full graph
g1 = graph.full(4)
g1

#Create a ring graph
g2 = graph.ring(5)
g2

#Combine 2 graphs
g = g1 %du% g2
g
E(g)
plot(g)

g3 = graph.difference(g, graph(c(2,1,3,1), directed=F))
E(g3)


# Create a lattice
g1 = graph.lattice(c(3,4,2))
# Create a tree
g2 = graph.tree(12, children=2)
plot(g1, layout=layout.fruchterman.reingold)
plot(g2, layout=layout.reingold.tilford)

# Generate random graph, fixed probability
g = erdos.renyi.game(20, 0.3) # n nodes p probability of tie generating in graph g
plot(g, layout=layout.fruchterman.reingold,
       vertex.label=NA, vertex.size=5)

# Generate random graph, fixed number of arcs
g = erdos.renyi.game(20, 15, type='gnm')# n nodes m edges in graph g
plot(g)
# Generate preferential attachment graph
g = barabasi.game(60, power=1, zero.appeal=1.3)
plot(g, layout=layout.fruchterman.reingold,
     vertex.label=NA, vertex.size=5)

#########################
# Minimum Spanning Tree
#########################
# Minimum Spanning Tree algorithm is to find a Tree 
# that connect all the nodes within a connected graph 
# while the sum of edges weight is minimum.

# Create the graph and assign random edge weights
set.seed(2014)
g = erdos.renyi.game(8, 0.35)
E(g)$weight = round(runif(length(E(g))),2) * 50
E(g)$width = E(g)$weight/5 # set the width of edges
plot(g, layout=layout.fruchterman.reingold, 
       edge.label=E(g)$weight)
# Compute the minimum spanning tree
mst = minimum.spanning.tree(g, weights = E(g)$weight)
set.seed(2014);plot(mst, layout=layout.reingold.tilford, 
       edge.label=E(mst)$weight)

# Plot the mst in the graph
eg = get.edgelist(g)
em = get.edgelist(mst)
eg = paste(eg[,1], eg[,2], sep = "--")
em = paste(em[,1], em[,2], sep = "--")
E(g)$color = "grey"
E(g)[match(em, eg)]$color="red"
set.seed(2014); plot(g, layout=layout.fruchterman.reingold, 
     edge.label=E(g)$weight)
#########################
# Minimum Spanning Tree
#########################


# Connected Component algorithms is to find the island 
# of nodes that are interconnected with each other, 
# in other words, one can traverse from one node to 
# another one via a path.  Notice that connectivity 
# is symmetric in undirected graph, it is not the necessary
# the case for directed graph (ie: it is possible that nodeA
# can reach nodeB, then nodeB cannot reach nodeA). 
# Therefore in directed graph, there is a concept of "strong"
# connectivity which means both nodes are considered connected 
# only when it is reachable in both direction.  A "weak" connectivity 
# means nodes are connected

g = graph(c(1, 10, 2, 3, 2, 4, 1, 3, 3, 4, 
               4, 5, 5, 3, 4, 6, 6, 7, 7, 8, 
               8, 6, 9, 10, 10, 11, 11, 9))
# Nodes reachable from node4
subcomponent(g, 4, mode="out")
# Nodes who can reach node4
subcomponent(g, 4, mode="in")
clusters(g, mode="weak")


myc = clusters(g, mode="strong")
myc
col.num = max(unique(myc$membership))
mycolor = colors()[sample(100, col.num)]
V(g)$color = mycolor[myc$membership]
plot(g, layout=layout.fruchterman.reingold)


# network diameter
d = get.diameter(g)
E(g, path=d)$color = "blue"

##################
# Shortest path
##################
# 'Shortest Path is almost the most commonly used algorithm 
# in many scenarios, it aims to find the shortest path from 
# nodeA to nodeB.  In iGraph, it use "breath-first search" 
# if the graph is unweighted (ie: weight is 1) and use 
# Dijkstra's algo if the weights are positive, 
# otherwise it will use Bellman-Ford's algorithm for 
# negatively weighted edges.'

g = erdos.renyi.game(12, 0.25)
plot(g, layout=layout.fruchterman.reingold)
pa = get.shortest.paths(g, 5, 9)[[1]]
pa = unlist(pa)

V(g)[pa]$color = 'green'
E(g)$color = 'grey'
E(g, path=pa)$color = 'red'
E(g, path=pa)$width = 3
plot(g, layout=layout.fruchterman.reingold)

#################
# Graph Statistics
#################
# Create a random graph
set.seed(2004); g = erdos.renyi.game(200, 0.01)
plot(g, layout=layout.fruchterman.reingold, 
       vertex.label=NA, vertex.size=3)
# No of nodes
 length(V(g))
# No of edges
 length(E(g))
# Density (No of edges / possible edges)
 graph.density(g)
# Number of islands
 clusters(g)$no
# Global cluster coefficient:
 #(close triplets/all triplets)
 transitivity(g, type="global")
# Edge connectivity, 0 since graph is disconnected
 edge.connectivity(g)
# Same as graph adhesion
 graph.adhesion(g)
# Diameter of the graph
 diameter(g)
# Reciprocity of the graph
 reciprocity(g)

# Diameter of the graph
 diameter(g)

# Reciprocity of the graph
 reciprocity(g)

degree.distribution(g)
plot(degree.distribution(g), type = "b", xlab="node degree")

# 1. Connectivity between two nodes measure the distinct paths with no shared 
# edges between two nodes. (ie: how much edges need to be removed to disconnect them)
# 2. Shortest path between two nodes
# 3. Trust between two nodes (a function of number of distinct path and distance of each path)

# Create a random graph
set.seed(2014);g = erdos.renyi.game(9, 0.5)
plot(g, layout=layout.fruchterman.reingold)
# Compute the shortest path matrix
shortest.paths(g)

# Compute the connectivity matrix
M = matrix(rep(0, 81), nrow=9)
M
for (i in 1:9) {
     for (j in 1:9) {
         if (i == j) {
             M[i, j] = -1
           } else {
               M[i+, j+1] = edge.connectivity(g, i, j)
             }
       }
   }
M

edge.connectivity(g, 1, 2)
#################
# Centrality Measures
#################

# 1.Degree centrality gives a higher score to a node that has a high in/out-degree
# 2. Closeness centrality gives a higher score to a node that has short path distance to every other nodes
# 3. Betweenness centrality gives a higher score to a node that sits on many shortest path of other node pairs
# 4. Eigenvector centrality gives a higher score to a node if it connects to many high score nodes
# 5. Local cluster coefficient measures how my neighbors are inter-connected with each other, 
# which means the node becomes less important.

g1 = barabasi.game(100, directed=F)
g2 = barabasi.game(100, directed=F)
g = g1 %u% g2

# Degree
deg = degree(g)
# Closeness (inverse of average dist)
clo = closeness(g)

# Betweenness
bet = betweenness(g)

# Local cluster coefficient
tra = transitivity(g, type="local")

# Eigenvector centrality
eig = evcent(g)$vector

net.cen = data.frame(degree = deg, closeness = clo, 
                     betweenness = bet, eigenvector = eig, transtivity = tra)
plot(net.cen)

lay = layout.fruchterman.reingold(g)
# Plot the eigevector and betweenness centrality
plot(evcent(g)$vector, betweenness(g))
text(evcent(g)$vector, betweenness(g), 0:100, 
       cex=0.6, pos=4)

V(g)[12]$color <- 'red'
V(g)[8]$color <- 'green'
plot(g, layout=lay, vertex.size=8, 
     vertex.label.cex=0.6)
