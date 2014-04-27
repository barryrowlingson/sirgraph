
## ----startup-------------------------------------------------------------
library(sirgraph)


## ----data----------------------------------------------------------------
g = makedata()
g


## ----attrs---------------------------------------------------------------
head(get.data.frame(g,"vert"))


## ----graphatts-----------------------------------------------------------
g$time
g$start
g$stepsize


## ----setup---------------------------------------------------------------
V(g)$state[1:3]="I"
V(g)$state[4:6]="R"


## ----splot---------------------------------------------------------------
plotSIR(g)


## ----glayout-------------------------------------------------------------
g = glayout(g)


## ----gggplot-------------------------------------------------------------
gplotgraph(g)


## ----plotvars------------------------------------------------------------
ggplot() + geom_edge(aes(x=x,y=y),col="grey", data=g) + geom_node(aes(x=x,y=y,col=age,shape=sex),size=5,data=g)


## ----infectN, fig.width=8, fig.height=4, out.width="1.0\\textwidth"------
g = makedata()
g = glayout(g)
g = infectN(1)(g)
par(mfrow=c(1,2)); par(mar=c(0,0,0,0))
plotSIR(g)
g = infectN(5)(g)
plotSIR(g)


## ----spread1-------------------------------------------------------------
sEvens = spreadP2(pSI=0.5, pIR=0.5)
sEvens


## ----spreadit, fig.width=8, fig.height=8, out.width="0.8\\textwidth"-----
par(mfrow=c(2,2)); par(mar=c(0,0,0,0))
plotSIR(g)
g = sEvens(g)
plotSIR(g)
g = sEvens(g)
plotSIR(g)
g = sEvens(g)
plotSIR(g)


## ----spreadmore,fig.width=8, fig.height=8, out.width="0.8\\textwidth"----
sMore = spreadP2(pSI=0.8, pIR=0.1)
sMore
g = infectN(5)(g)
par(mfrow=c(2,2)); par(mar=c(0,0,0,0))
plotSIR(g)
g = sMore(g)
plotSIR(g)
g = sMore(g)
plotSIR(g)
g = sMore(g)
plotSIR(g)
g$time


## ----pSunday-------------------------------------------------------------
restDay <- function(t, v){
    nv = length(v)
    if(wday(t)==1){
        return(rep(0.5,nv))
    }else{
        return(rep(0.9,nv))
    }
}
restDay("2014-04-06",0)
restDay("2014-04-07",0)


## ----pVacc---------------------------------------------------------------
isVaccinated <- function(t,v){
    ifelse(v$vaccinated,0.2,0.9)
}
# see the first few
isVaccinated("2014-01-01",V(g))[1:8]


## ----fspread-------------------------------------------------------------
fConst = function(t,v){rep(0.3, length(v))}
spreadVaccine = spreadF(isVaccinated, fConst)
spreadVaccine


## ----runsim--------------------------------------------------------------
g = makedata()
g = glayout(g)
g = infectN(1)(g)
g = stepSim(g, sMore, stopAfter("2014-01-12"))
g$time
gplotgraph(g)


## ----stepclear-----------------------------------------------------------
g = makedata()
g = glayout(g)
g = infectN(1)(g)
g = stepSim(g, sMore, stopWhenClear)
g$time
gplotgraph(g)


## ----vdf-----------------------------------------------------------------
d = get.data.frame(g,"vertices")
d$tI = as.Date(d$tI, "1970-01-01")
d$tR = as.Date(d$tR, "1970-01-01")
head(d)
ggplot(d,aes(x=tI,fill=vaccinated))+geom_histogram()


## ----runFspread----------------------------------------------------------
gF = infectN(2)(glayout(makedata(g=graph.tree(150,children=4))))
gF = stepSim(gF, spreadVaccine, stopWhenClear)
gplotgraph(gF)


## ----continuous----------------------------------------------------------
g0 = infectN(10)(glayout(makedata()))

contSpread = cspreadR(0.1, 0.1)
discSpread = spreadP2(0.1, 0.1)
gContinuous = stepSim(g0, contSpread, stopWhenClear)
gDiscrete = stepSim(g0, discSpread, stopWhenClear)

table(V(gContinuous)$state)
table(V(gDiscrete)$state)



## ----timeplot,fig.width=8, fig.height=4, out.width="1.0\\textwidth"------
timePlot(g, s=1)


