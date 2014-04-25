#'
#' continuous-time spreader functions
#'
#'


cspreadF <- function(rSI, rIR){
    force(rSI)
    force(rIR)
    f = function(g){

        ## 

        ## when will infected cases recover?
        I =  which(V(g)$state=="I")
        if(length(I)>0){
            tRecover = rexp(length(I), rIR)
        }else{
            tRecover = Inf
        }

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
            tInfect = rexp(nrow(infs),infs[,3])
        }else{
            tInfect = Inf
        }

        if(min(tInfect)<min(tRecover)){
            ## we have an infection
            wInfected = which.min(tInfect)
            iInfected = infs[wInfected,2]
            t = tInfect[wInfected]
            V(g)$state[iInfected]="I"
            V(g)$tI[iInfected] = g$time + t
        }else{
            ## we have a recovery
            wRecover = which.min(tRecover)
            iRecover = I[wRecover]
            t = tRecover[wRecover]
            V(g)$state[iRecover]="R"
            V(g)$tR[iRecover]=g$time+t
        }
        g$time = g$time + t
        g
    }


    spreader(f,"continuous time, fixed rates")
}
