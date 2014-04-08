##' example SIR with vaccination
##'
##' run an SIR model with vaccination
##' @title runVacc
##' @param ni number of initial cases
##' @return SIR graph after infection has finished
##' @author Barry S Rowlingson
##' @export
##' 
runVacc <- function(ni){
    ## create a graph with infectious cases
    g = infectN(ni)(glayout(makedata()))

    ## define the functions for S-I and I-R
    fSI <- function(t, v){
        ifelse(v$vaccinated, 0, 0.9)
    }

    fIR <- function(t, v){
        return(0.3)
    }

    ## make the spreader function
    spreader = spreadF(fSI, fIR)

    ## run the simulation until clear
    g = stepSim(g, spreader, stopWhenClear)

    g
}
        
