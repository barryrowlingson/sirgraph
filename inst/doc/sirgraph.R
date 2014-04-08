
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


## ----timeplot------------------------------------------------------------
timePlot(g, s=1)


