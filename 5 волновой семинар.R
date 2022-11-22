d <- matrix(c(0,1,1,0,0,0,0,0,0,
              1,0,0,1,1,0,0,0,0,
              1,0,0,0,1,1,0,0,0,
              0,1,0,0,1,0,1,0,0,
              0,1,1,1,0,0,1,0,0,
              0,0,1,0,0,0,0,1,0,
              0,0,0,1,1,0,0,0,1,
              0,0,0,0,0,1,0,0,1,
              0,0,0,0,0,0,1,1,0), nrow=9)

install.packages('igraph')
library(igraph)
m <- graph.adjacency(d, mode='undirected')
class(m)
#plot.igraph(m, layout=layout_in_circle)
plot.igraph(m)