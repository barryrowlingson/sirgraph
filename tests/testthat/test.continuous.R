test_that("continuous sim",{
    g = makedata()
    g = infectN(2)(g)
    s = cspreadR(.1,.1)
    g = stepSim(g, s, stopWhenClear)
    expect_true(!any(V(g)=="I"))
}
          )
