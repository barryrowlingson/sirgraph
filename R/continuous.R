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
cspreadF <- function(rSI, rIR){
    force(rSI)
    force(rIR)
    f = function(g){

        ## 

        ## when will infected cases recover?
        I =  which(V(g)$state=="I")
        if(length(I)>0){
            gotRecovers = TRUE
            tRecover = rexp(length(I), rIR)
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
            tInfect = rexp(nrow(infs),infs[,3])
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
            V(g)$tI[iInfected] = g$time + t
            cat("infection of ",iInfected," after ",t,"\n")
        }else{
            ## we have a recovery
            wRecover = which.min(tRecover)
            iRecover = I[wRecover]
            t = tRecover[wRecover]
            V(g)$state[iRecover]="R"
            V(g)$tR[iRecover]=g$time+t
            cat("recovery of ",iRecover," after ",t,"\n")
        }
        g$time = g$time + t
        g
    }


    spreader(f,"continuous time, fixed rates")
}
