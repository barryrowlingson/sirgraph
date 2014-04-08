
stateAt <- function(g,t, melt=FALSE){
    sir= ldply(t, function(t){
        gT = gAtTime(g,t)
        table(factor(V(gT)$state, levels=c("S","I","R")))
    })
    d = data.frame(t=t,sir)
    if(melt){
        return(melt(d, id.vars="t",value.name="count", variable.name="state"))
    }else{
        return(d)
    }
    
}
