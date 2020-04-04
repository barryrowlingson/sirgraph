
SIRcolours = list("I"="#E41A1C", "R"="#377EB8", "S"="#4DAF4A")

##' add layout to graph
##'
##' this function adds x and y coordinate to nodes for plotting
##' @title glayout
##' @param g an SIR graph
##' @param layout a layout function from igraph
##' @param ... further parameters for the layout function
##' @return a modified SIR graph
##' @author Barry S Rowlingson
##' @export
glayout <- function(g,
                    layout=layout.kamada.kawai,
                    ...
                    ){
    xy = layout(g,...)
    V(g)$x=xy[,1]
    V(g)$y=xy[,2]
    g
}
##' geom for nodes
##'
##' get locations from a graph and plot points. See the help for geom_point for more.
##' @title geom_node
##' @param mapping the aesthetic mapping
##' @param data the graph
##' @param stat statistical transformation 
##' @param position position adjustment
##' @param na.rm missing value behaviour
##' @param ... further arguments
##' @return a geom for ggplot
##' @author Barry S Rowlingson
##' @export
geom_node <- function(mapping=NULL, data=NULL, stat="identity", position="identity",
                      na.rm=FALSE,...){
    gdata = get.data.frame(data, what="vertices")
    geom_point(mapping, gdata, stat, position, na.rm,...)
}
    
    
##' geom for edges
##'
##' get edge lines for a graph and plot using geom_segment
##' @title geom_edge
##' @param mapping the aesthetic mapping
##' @param data data frame
##' @param graph the graph
##' @param directed currently ignored
##' @param ... further parameters for geom_segment
##' @return a geom_segment for ggplot
##' @author Barry S Rowlingson
##' @export
geom_edge <- function(mapping=NULL, data=NULL, graph=NULL, directed=FALSE,...){
    xy <- cbind(
        get.vertex.attribute(data, mapping$x),
        get.vertex.attribute(data, mapping$y)
        )
    edgelist <- get.edgelist(data, names=FALSE) # get row numbers not names!
    df <- data.frame(xy[edgelist[, 1],], xy[edgelist[,2],])
    names(df)=c("x","y","xend","yend")
    geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data=df, ...)
}

##' plot an SIR graph
##'
##' use ggplot to show an SIR graph
##' @title gplotgraph
##' @param g an SIR graph
##' @return make a plot
##' @author Barry S Rowlingson
##' @export
gplotgraph <- function(g){
    ggplot() + geom_edge(aes(x=x,y=y),col="grey", data=g) +
        geom_node(aes(x=x,y=y,col=factor(state,levels=c("S","I","R")),shape=vaccinated),size=5,data=g) +
            scale_colour_manual(values=c("I"="#E41A1C", "R"="#377EB8", "S"="#4DAF4A"), drop=FALSE, name="State") +
    ggtitle(g$time)
    
}
##' SIR graph base graphics plot
##'
##' use base graphics to plot an SIR network
##' @title plotSIR
##' @param g an SIR graph
##' @param layout layout algorithm
##' @param seed random seed for consistent layouts
##' @param ... further layout parameters
##' @return nothing
##' @author Barry S Rowlingson
##' @export
plotSIR <- function(g, layout=layout.kamada.kawai, seed=1, ...){
    ss = .Random.seed
    set.seed(seed)
    cols = list(S="#80FF80",I="#FF8080",R="#8080FF")
    plot(g, vertex.color = unlist(cols[V(g)$state]), layout=layout, ...)
    set.seed(ss)
}

##' plot SIR by time
##'
##' produces a line plot of S I R numbers
##' @title timePlot
##' @param g an SIR graph
##' @param t time points for plotting or...
##' @param n number of time points between start and finish or...
##' @param s step size between start and finish
##' @return a ggplot graph object
##' @export
##' @author Barry S Rowlingson
timePlot <- function(g,t,n,s){
    if(missing(t)){
        if(missing(s)){
            t = seq(g$start, g$time, len=n)
        }else{
            t = seq(g$start, g$time, by=s)
        }
    }
    m = stateAt(g, t, melt=TRUE)
    ggplot(m,aes(x=t, y=count)) +
        geom_line(aes(group=state, colour=state, size=state)) + 
        scale_colour_manual(values=unlist(SIRcolours) ) +
        scale_size_manual(values=c("S"=3, "I"=2, "R"=1))
}
