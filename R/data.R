makedata <- function(g = graph.famous("Zachary"),
                     s=310366,
                     pvac=0.6,
                     start = as.Date("2014-01-01"),
                     stepsize = 1){
    set.seed(s)
    number_of_vertices = vcount(g)
    V(g)$vaccinated = runif(number_of_vertices) < pvac
    V(g)$sex = sample(c("M","F"), number_of_vertices, replace=TRUE)
    V(g)$age = as.integer(runif(number_of_vertices, 18, 24))
    V(g)$state = "S"
    g$start = start
    g$time = start
    g$stepsize = stepsize
    g
    
}
