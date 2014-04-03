
geom_node <- function(mapping=NULL, data=NULL, stat="identity", position="identity",
                      na.rm=FALSE,...){
    gdata = get.data.frame(data, what="vertices")
    geom_point(mapping, gdata, stat, position, na.rm,...)
}
    
    
geom_edge <- function(mapping=NULL, data=NULL, graph=NULL, directed=FALSE,...){
    xy <- cbind(
        get.vertex.attribute(data, mapping$x),
        get.vertex.attribute(data, mapping$y)
        )
    edgelist <- get.edgelist(data)
    df <- data.frame(xy[edgelist[, 1],], xy[edgelist[,2],])
    names(df)=c("x","y","xend","yend")
    geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data=df, ...)
}

gplotgraph <- function(g){
    ggplot() + geom_edge(aes(x=x,y=y),col="grey", data=g) +
        geom_node(aes(x=x,y=y,col=state,shape=vaccinated),size=5,data=g) +
            scale_colour_manual(values=c("I"="#E41A1C", "R"="#377EB8", "S"="#4DAF4A")) +
    ggtitle(g$time)
    
}

plotSIR <- function(g, layout=layout.kamada.kawai, seed=1, ...){
    ss = .Random.seed
    set.seed(seed)
    cols = list(S="#80FF80",I="#FF8080",R="#8080FF")
    plot(g, vertex.color = unlist(cols[V(g)$state]), ...)
    set.seed(ss)
}

