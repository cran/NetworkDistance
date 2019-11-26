## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----call---------------------------------------------------------------------
library(NetworkDistance)

## ----load---------------------------------------------------------------------
data(graph20)     # use `help(graph20)' to see more details.
typeof(graph20)   # needs to be a list

## ----showgraph, echo=FALSE, fig.align='center'--------------------------------
suppressMessages(library(igraph))
par(mfrow=c(1,2),pty="s")
plot(graph_from_adjacency_matrix(graph20[[7]], mode="undirected"), vertex.size=7, vertex.label=NA, main="graph No.7", edge.arrow.size=.5)
plot(graph_from_adjacency_matrix(graph20[[18]], mode="undirected"), vertex.size=7, vertex.label=NA, main="graph No.18", edge.arrow.size=.5)

## ----run----------------------------------------------------------------------
dist.gdd <- nd.gdd(graph20)  # return as a 'dist' object

## ----run_vis1, echo=FALSE, fig.align='center'---------------------------------
par(pty="s")
dmat = as.matrix(dist.gdd$D)
image(dmat[,20:1],main="pairwise distance matrix",axes=FALSE,col=gray(0:100/100)) 

## ----compare------------------------------------------------------------------
dist.wsd <- nd.wsd(graph20)              # spectrum-weighted distance
dist.dsd <- nd.dsd(graph20, type="SLap") # discrete spectral measure
dist.nfd <- nd.nfd(graph20)              # network flow distance

## ----run_vis2, echo=FALSE-----------------------------------------------------
par(mfrow=c(1,3),pty="s")
image(as.matrix(dist.wsd$D)[,20:1],main="nd.wsd",axes=FALSE,col=gray(0:100/100)) 
image(as.matrix(dist.dsd$D)[,20:1],main="nd.dsd",axes=FALSE,col=gray(0:100/100)) 
image(as.matrix(dist.nfd$D)[,20:1],main="nd.nfd",axes=FALSE,col=gray(0:100/100)) 

## ----embed--------------------------------------------------------------------
gdd2 = stats::cmdscale(dist.gdd$D, k=2)  # 2-d embedding from 'gdd' distance
wsd2 = stats::cmdscale(dist.wsd$D, k=2)  #                    'wsd'
dsd2 = stats::cmdscale(dist.dsd$D, k=2)  #                    'dsd'
nfd2 = stats::cmdscale(dist.nfd$D, k=2)  #                    'nfd'

## ----embed_vis, echo=FALSE, fig.align="center"--------------------------------
par(mfrow=c(2,2))
plot(gdd2, main="embedding with 'gdd'", pch=19, xlab="x", ylab="y", cex=0.25)
plot(wsd2, main="embedding with 'wsd'", pch=19, xlab="x", ylab="y", cex=0.25)
plot(dsd2, main="embedding with 'dsd'", pch=19, xlab="x", ylab="y", cex=0.25)
plot(nfd2, main="embedding with 'nfd'", pch=19, xlab="x", ylab="y", cex=0.25)

