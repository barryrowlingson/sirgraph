##' make a simple graph for SIR modelling
##'
##' make a graph for SIR modelling - it initialises the
##' network and the state parameter, and adds a few more
##' random things such as age, sex, vaccination state. Handy
##' for simple tests
##' @title makedata
##' @param g a simple graph
##' @param s a random seed
##' @param pvac vaccination probability
##' @param start time start
##' @return a modified version of g with extra vertex attributes
##' @author Barry S Rowlingson
##' @export

makedata <- function(g = graph.famous("Zachary"),
                     s=310366,
                     pvac=0.6,
                     start = as.Date("2014-01-01")
                     ){
    set.seed(s)
    number_of_vertices = vcount(g)
    V(g)$vaccinated = runif(number_of_vertices) < pvac
    V(g)$sex = sample(c("M","F"), number_of_vertices, replace=TRUE)
    V(g)$age = as.integer(runif(number_of_vertices, 18, 24))
    V(g)$state = "S"
    g$start = start
    g$time = start
    g
    
}
