##' reset graph
##'
##' reset all nodes to "S", reset clock to start, clear infection/recovery times
##' @title resetG
##' @param g a graph network for SIR modelling
##' @return modified g
##' @author Barry S Rowlingson
##' @export
resetG <- function(g){
    nv = vcount(g)
    V(g)$state = rep("S", nv)
    V(g)$tI = rep(NA,nv)
    V(g)$tR = rep(NA,nv)
    g$time = g$start
    g
}
##' simple infection starter function
##'
##' creates a function that creates a fixed number of cases
##' @title infectN
##' @param N number of cases to create
##' @return a function that resets its graph and adds N initial infections.
##' @author Barry S Rowlingson
##' @export
infectN <- function(N){
    force(N)
    f = function(g){
        g = resetG(g)
        iCases = sample(vcount(g),N)
        V(g)$tI[iCases]=g$time
        V(g)$state[iCases]="I"
        g
    }
    f
}

##' simple two-probability model
##'
##' creates a spreader function with an infection probability and a recovery probability
##' @title spreadP2
##' @param pSI infection probability
##' @param pIR recovery probability
##' @param stepsize size of time step
##' @return a spreader function that uses the probabilities on each edge to proceed with the
##' infection process.
##' @author Barry S Rowlingson
##' @export
spreadP2 <- function(pSI=0.8, pIR=0.2, stepsize=1){
    force(pSI);force(pIR)
    fSI = function(t,v){rep(pSI, length(v))}
    fIR = function(t,v){rep(pIR, length(v))}
    return(spreadF(fSI, fIR, stepsize))
}

##' functional probability epi modelling
##'
##' spread an SIR model using a function of time and attributes
##' @title spreadF
##' @param fpSI a function of t and vertices that returns the infection probability
##' @param fpIR a function of t and vertices that returns the recovery probability
##' @param stepsize time step
##' @return a spreader function
##' @author Barry S Rowlingson
##' @export
spreadF <- function(fpSI, fpIR, stepsize=1){
    force(fpSI);force(fpIR)
    f <- function(g){
        g$stepsize = stepsize
        g$time = g$time + g$stepsize
        ## recover infected:
        I = which(V(g)$state=="I")
        if(length(I)>0){
            recovered = I[runif(length(I))<fpIR(g$time, V(g)[I])]
            if(length(recovered)>0){
                V(g)$state[recovered]="R"
                V(g)$tR[recovered]=g$time
            }       
        }
        ## find infected cases
        for(s in which(V(g)$state=="I")){
            ## get their neighbours
            for(i in V(g)[nei(s)])
                ## maybe infect susceptible neighbours
                if(V(g)$state[i] == "S"){
                    if(runif(1) < fpSI(g$time, V(g)[i])){
                        V(g)$state[i] = "I"
                        V(g)$tI[i] = g$time
                    }
                }
        }
        g
    }
    f
}

    


##' Discrete time-stepping SIR graph model
##'
##' Starting with a graph, infect the initial cases, then run the
##' spreader until the stopper is true. Optionally run the plotter
##' every iteration
##' @title stepSim
##' @param g an SIR graph
##' @param spreader a spreader function
##' @param stopper a stopper function
##' @param after function to call after each spread function call
##' @return the end state of the graph
##' @author Barry S Rowlingson
##' @export
stepSim <- function(g, spreader, stopper, after=force){
    repeat{
        g = spreader(g)
        after(g)
        if(stopper(g)){
            break
        }
    }
    g
}

##' recalculate state of network at given time
##'
##' when a simulation has run, the graph stores the infection and recovery times, and
##' the final states. This function sets the states to the values correct at the given
##' time by comparison with the infection and recovery times. It also sets the time of the
##' graph but does not change the infection and recovery times.
##' @title gAtTime
##' @param g an SIR graph
##' @param t a time point
##' @return the SIR graph at that time
##' @author Barry S Rowlingson
##' @export
gAtTime <- function(g,t){
    g$time=t
    tI = V(g)$tI
    tR = V(g)$tR
    V(g)$state = "S"
    V(g)$state[tI<=t] = "I"
    V(g)$state[tR<=t] = "R"
    g
}

##' Stop after a certain time
##'
##' This is a stopper function generator returns a function that stops a process after a
##' given time. 
##' @title stopAfter
##' @param d anything that can be compared to the time attribute of the graph
##' @return a stopper function that returns whether or not the time is past.
##' @author Barry S Rowlingson
##' @export
stopAfter <- function(d){
    force(d)
    f = function(g){
        if(g$time >= d){
            return(TRUE)
        }else{
            return(FALSE)
        }
    }
    f
}

##' Stop when no more infectious cases
##'
##' This is a stopper function that returns true if there are no infectious cases.
##' @title stopWhenClear
##' @param g an SIR graph
##' @return TRUE if ther are no infectious cases, otherwise FALSE
##' @author Barry S Rowlingson
##' @export
stopWhenClear <- function(g){
    if(any(V(g)$state=="I")){
        return(FALSE)
    }
    return(TRUE)
}

