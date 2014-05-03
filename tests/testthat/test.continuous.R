test_that("continuous sim",{
    g = makedata()
    g = infectN(2)(g)
    s = cspreadR(1,.1)

    ## recovery time not set
    expect_error(stepSim(g, s, stopWhenClear))

    g = addRecovery(g,1)

    ## now stepping works
    g = stepSim(g, s, stopWhenClear)
    expect_true(!any(V(g)=="I"))
}
          )

test_that("fixing up infections",{
    g = graph.formula(A-B,B-C,C-D)
    g = makedata(g)
    g = infectN(2)(g)
    g = addRecovery(g,2)
})
