##' example SIR with vaccination
##'
##' run an SIR model with vaccination
##' @title runVacc
##' @param ni number of initial cases
##' @param pVaccine vector of unvaccinated/vaccinated transmission probability
##' @param pIR single probability of recovert
##' @param g graph structure
##' @return SIR graph after infection has finished
##' @author Barry S Rowlingson
##' @export
##' 
runVacc <- function(ni, pVaccine=c(0,0.9), pIR=0.3, g){
    ## create a graph with infectious cases
    ##
    force(pVaccine);force(pIR)
    ## note infectN can infect vaccinated cases!
    if(missing(g)){
        g = infectN(ni)(glayout(makedata()))
    }else{
        g = infectN(ni)(glayout(makedata(g)))
    }

    ## define the functions for S-I and I-R
    fSI <- function(t, v){
        ifelse(v$vaccinated, pVaccine[1], pVaccine[2])
    }

    fIR <- function(t, v){
        return(rep(pIR, length(v)))
    }

    ## make the spreader function
    spreader = spreadF(fSI, fIR)

    ## run the simulation until clear
    g = stepSim(g, spreader, stopWhenClear)

    g
}
        
