
stateAt <- function(g,t){
    sir= ldply(t, function(t){
        gT = gAtTime(g,t)
        table(factor(V(gT)$state, levels=c("S","I","R")))
    })
    data.frame(t=t,sir)
}
