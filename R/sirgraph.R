
resetG <- function(g){
    nv = vcount(g)
    V(g)$state = rep("S", nv)
    V(g)$tI = rep(NA,nv)
    V(g)$tR = rep(NA,nv)
    g$time = g$start
    g
}

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


spreadP2 <- function(pSI=0.8, pIR=0.2){
    f <- function(g){
        ## recover infected:
        I = which(V(g)$state=="I")
        if(length(I)>0){
            recovered = I[runif(length(I))<pIR]
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
                    if(runif(1) < pSI){
                        V(g)$state[i] = "I"
                        V(g)$tI[i] = g$time
                    }
                }
        }
        g
    }
    f
}
            



stepSim <- function(g, starter=infectN(1), spreader, stopper, plotter=NULL){
    g = glayout(g)
    g = starter(g)
    repeat{
        g$time = g$time + g$stepsize
        g = spreader(g)
        if(!is.null(plotter)){
            print(plotter(g))
        }
        if(stopper(g)){
            break
        }
    }
    g
}


gAtTime <- function(g,t){
    g$time=t
    tI = V(g)$tI
    tR = V(g)$tR
    V(g)$state = "S"
    V(g)$state[tI<=t] = "I"
    V(g)$state[tR<=t] = "R"
    g
}

stopAfter <- function(d){
    f = function(g){
        if(g$time >= d){
            return(TRUE)
        }else{
            return(FALSE)
        }
    }
    f
}

stopWhenClear <- function(g){
    if(any(V(g)$state=="I")){
        return(FALSE)
    }
    return(TRUE)
}


glayout <- function(g,
                    layout=layout.kamada.kawai,
                    ...
                    ){
    xy = layout(g,...)
    V(g)$x=xy[,1]
    V(g)$y=xy[,2]
    g
}
