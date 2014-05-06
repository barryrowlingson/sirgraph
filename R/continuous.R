##' continuous time spreading function
##'
##' infection and recovery rates are times sampled from
##' an exponential distribution.
##'
##' The function works by simulating all recovery
##' and infection times from the current state, taking
##' the minimum of those and changing the state
##' of the graph for that case only. The other simulation
##' times are then thrown away. The modified graph is
##' returned.
##'
##' Since the recovery times are independent of time
##' and graph state, then these
##' could all be computed at infection time for an
##' efficiency gain. Currently they aren't.
##' 
##' @title independent rate spread
##' @param rSI rate of infections per contact-time
##' @param rIR rate of recovery per unit time
##' @return updated SIR graph 
##' @author Barry Rowlingson
##' @export

cspreadR <- function(rSI, rIR){
    force(rSI)
    force(rIR)
    f = function(g){

        
        ##
        time = g$time

        ## when will infected cases recover?
        I =  which(V(g)$state=="I")
        if(length(I)>0){
            if(any(is.na(V(g)$tR[I]))){
                whichFail = I[is.na(V(g)$tR[I])]
                stop("Recovery time for ",paste(whichFail,collapse=","), " not set")
            }
            gotRecovers = TRUE
            tRecover = V(g)$tR[I]
        }else{
            gotRecovers = FALSE
            tRecover = Inf
        }

        t = 1
        ## when might susceptibles get infected?
        infs = matrix(ncol=3,nrow=0)
        for(i in which(V(g)$state=="I")){
            ## get their neighbours
            for(s in V(g)[nei(i)]){
                ## maybe infect susceptible neighbours
                if(V(g)$state[s] == "S"){
                    infs=rbind(infs,c(i,s,rSI))
                }
            }
        }

        if(nrow(infs)>0){
            gotInfects = TRUE
            tInfect = time + rexp(nrow(infs),infs[,3])
        }else{
            gotInfects = FALSE
            tInfect = Inf
        }
        
        if(!gotRecovers & !gotInfects){
            ## nothing ever happens...
            g$time = g$time + (g$time - g$start)
            return(g)
        }

        
        if(min(tInfect)<min(tRecover)){
            ## we have an infection
            wInfected = which.min(tInfect)
            iInfected = infs[wInfected,2]
            t = tInfect[wInfected]
            V(g)$state[iInfected]="I"
            V(g)$tI[iInfected] = t
### recoveries are independent of everything
            V(g)$tR[iInfected] = t + rexp(1, rate=rIR)
        }else{
            ## we have a recovery
            wRecover = which.min(tRecover)
            iRecover = I[wRecover]
            t = tRecover[wRecover]
            V(g)$state[iRecover]="R"
        }
        g$time = t
        g
    }


    spreader(f,paste("continuous time, infection rate: ", rSI," recovery rate: ",rIR,sep=""))
}

##' add recovery times
##'
##' this function takes a graph and computes recovery times for
##' all infected cases based on an exponential distribution
##' @title Add recovery times
##' @param g an SIR graph
##' @param rate exponential distribution rate per unit time
##' @return an SIR graph with recovery times set for infected cases
##' @export
##' @author Barry Rowlingson
addRecovery <- function(g, rate){
    I=which(V(g)$state=="I")
    n = length(I)
    V(g)$tR[I] = rexp(n, rate) + g$time
    g
}
