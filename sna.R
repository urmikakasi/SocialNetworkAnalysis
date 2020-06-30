if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/navdata")
install.packages(
       c("tidyverse", "igraph", "tidygraph", "ggraph") )
library("navdata")

getwd()
df<-read.table("dataset.txt", header=F)
data()
drop
head(df)
colnames(df)<-c("source", "destination",'timestamp')
library(tidyverse)
#  Get distinct source names
df<-df%>%slice(1:1000)
sources <- df %>%
  distinct(source) %>%
  rename(label = source)
write.csv(sources,"sources.csv")
# Get distinct destination names
destinations <- df %>%
  distinct(destination) %>%
  rename(label = destination)
write.csv(destinations, "destinations.csv")
# Join the two data to create node
# Add unique ID for each country
nodes <- full_join(sources, destinations, by = "label") 
nodes <- nodes %>%
  mutate(id = 1:nrow(nodes)) %>%
  select(id, everything())
head(nodes, 3)
write.csv(nodes, "nodes.csv")
df <- df %>%
  rename(weight = n.call)
# (a) Join nodes id for source column
edges <- df %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)
# (b) Join nodes id for destination column
edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)
# (c) Select/keep only the columns from and to
edges <- select(edges, from, to)
head(edges, 3)
write.csv(edges,"edges.csv")
edges<-edges %>% slice(1:1000)
nodes<-nodes %>% slice(1:1000)
library(igraph)
library(readr)


df1<-within(df, rm('timestamp'))
df1<-df%>%slice(1:1000)
nw<-graph_from_data_frame(d=df1,directed = T)

plot(nw)
par(mar=c(0,0,0,0))
plot(nw,
     vertex.color = "purple", # change color of nodes
     vertex.label.color = "white", # change color of labels
     vertex.label.cex = .75, # change size of labels to 75% of original size
     edge.curved=.25, # add a 25% curve to the edges
     edge.color="grey20") # change edge color to grey

V(nw)$size <- strength(nw)
par(mar=c(0,0,0,0))
plot(nw)
g<-nw
V(g)$label <- ifelse( strength(g)>=10, V(g)$name, NA )
par(mar=c(0,0,0,0)); plot(g)
E(g)$width <- log(E(g)$timestamp) -19
edge_attr(g)

par(mfrow=c(2, 3), mar=c(0,0,1,0))
plot(g,vertex.color = "purple", layout=layout_with_fr, main="Circle")

par(mfrow=c(1,2))
set.seed(777)
fr <- layout_with_fr(g, niter=1000)
par(mar=c(0,0,0,0)); plot(g, layout=fr)
set.seed(666)
fr <- layout_with_fr(g, niter=1000)
par(mar=c(0,0,0,0)); plot(g, layout=fr)

degree(nw, mode="in")
closeness(nw, mode="all", weights=NA, normalized=T)
betweenness(nw, directed=T, weights=NA, normalized = T)
mean(degree(nw))/(vcount(nw)-1) #density
max(eccentricity(g,mode="all")) #diameter
distances(nw, v=V(nw)["9"], to=V(nw), weights=NA)
