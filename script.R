install.packages("igraph")
library(igraph)

install.packages("sna")
library(sna)

email<-read.table("email-EU.edges")
emailmat<-as.matrix(email)
v1<-emailmat[,1]
v2<-emailmat[,2]
df<-data.frame(from=v1,to=v2)
g<-graph_from_data_frame(df,directed = TRUE)

plot(g)

gsimplified<-simplify(g)
gsimplified<-delete_vertices(gsimplified,V(gsimplified)[degree(gsimplified)<10])
gsimplified<-delete_vertices(gsimplified,V(gsimplified)[degree(gsimplified)==0])
E(gsimplified)$weight<-rnorm(ecount(gsimplified))
V(gsimplified)$weight<-rnorm(vcount(gsimplified))
gsimplified[1:10,1:20]
subg<-induced.subgraph(gsimplified,which(V(gsimplified)$weight>1.0))

str(g)

V(g)

E(g)

adjg <- as_adjacency_matrix(g)
adjg

adj_matrix_density <- gden(g)
adj_matrix_density

sg_ego<-ego.extract(adj_matrix_dense)
sg_ego

igraph::degree(g)

sg.between<-igraph::centr_betw(sg)
sg.between

sg.closeness<-igraph::centr_clo(sg)
sg.closeness

sg.sp<-igraph::shortest.paths(sg)
sg.sp<-igraph::distances(sg)
sg.sp

sp<-get.shortest.paths(g,from=1,to=69,mode = "all")
sp

sg.geos<-geodist(sparse_adj_dense)
sg.geos

g.adj<-as_adjacency_matrix(g)
g.np=g.adj%*%g.adj
g.np

#Add before Historgram

hist(igraph::degree(g))
hist(igraph::degree(wgnonnegnoiso))

wgfinal.adj <- as_adjacency_matrix(wgnonnegnoiso)
wgfinal <- igraph::graph_from_adjacency_matrix(wgfinal.adj, mode = "undirected")
wgfinal
plot(wgfinal)

igraph::edge_density(wgnonnegnoiso)
igraph::edge_density(wgnonnegnoiso, loops = TRUE)

wgnonnegnoiso.dia=igraph::diameter(wgnonnegnoiso)
wgnonnegnoiso.dia

node<-c(5)
wgnonnegnoiso.5clique=igraph::max_cliques(wgnonnegnoiso, min=NULL, max=NULL, subset = node)
wgnonnegnoiso.5clique

wgnonnegnoiso.lgcliques=igraph::clique_num(wgfinal)
wgnonnegnoiso.lgcliques

gcolor<-wgnonnegnoiso
V(gcolor)$color <-sample(c("red", "black"),vcount(gcolor), rep=TRUE)
E(gcolor)$color<-"grey"
red<-V(gcolor)[color=="red"]
bl<-V(gcolor)[color=="black"]
E(gcolor)[red%--%red]$color<-"red"
E(gcolor)[bl%--%bl]$color<-"black"
plot(gcolor,vertex.size=5,edge.arrow.size=0.2,layout=layout.fruchterman.reingold)

degree(wgnonnegnoiso)

vertex_attr(wgnonnegnoiso)

adjmatrix<-as_adjacency_matrix(wgnonnegnoiso)
adjmatrix
wgnonnegnoiso[1:20,1:30]

is.simple(wgnonnegnoiso)
gsimplified<-simplify(g)
is.simple(gsimplified)

wt<-walktrap.community(wgnonnegnoiso)
plot(wt,wgnonnegnoiso,layout=layout.fruchterman.reingold)

defwt<-walktrap.community(g)
plot(defwt,g,edge.arrow.size=0.1,layout=layout.fruchterman.reingold)

ac<-alpha_centrality(wgnonnegnoiso)
ac
defac<-alpha_centrality(g)
defac

degreenumber=degree(g,mode="total")
sort(degreenumber)

gsimplified<-simplify(g)
gsimplified<-delete_vertices(gsimplified,V(gsimplified)[degree(gsimplified)<10])
gsimplified<-delete_vertices(gsimplified,V(gsimplified)[degree(gsimplified)==0])
E(gsimplified)$weight<-rnorm(ecount(gsimplified))
V(gsimplified)$weight<-rnorm(vcount(gsimplified))
gsimplified[1:10,1:20]
subg<-induced.subgraph(gsimplified,which(V(gsimplified)$weight>1.0))

sg <- induced_subgraph(g,which(components(g)$membership==1))
V(sg)$degree=degree(sg)
result=dfs(sg,root=1,dist=TRUE)$dist
sort(result, decreasing = TRUE)

V(g)$degree=degree(g)
sg=induced.subgraph(g,which(V(g)$degree>50))
vcount(sg)
cliquess<-max_cliques(sg)
cliquess[1:5]

newgraph=delete.vertices(g,which(degree(g)<400))
res=power_centrality(newgraph,rescale = TRUE)
sort(res)

V(g)$degree=degree(g)
gn=delete.edges(gsimplified,which(E(gsimplified)$weight<=1))
ecount(gn)
for(v in V(gn)){
  edges=incident(gn,v);
  vertex_attr(gn,"weight",index=c(v))<-sum(edges$weight);
}
max_weight=max(V(gn)$weight)
max_weight
target_single_vertex_graph=induced.subgraph(gn,which(V(gn)$weight==max_weight))
V(target_single_vertex_graph)

central_node<-which.max(degree(g,mode="all"))
central_node

between<-betweenness(g)
between_node<-which.max(between)
between_node

largest_cliques(g)

eg<-igraph::ego(g)
eg[1:5]

pc<-power_centrality(sg,exponent=0.8)
sort(pc,decreasing = TRUE)