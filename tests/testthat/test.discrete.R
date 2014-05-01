test_that("discrete sim",{
    g = makedata()
    g = infectN(2)(g)
    s = spreadP2()
    g = stepSim(g, s, stopWhenClear)
    expect_true(!any(V(g)=="I"))
}
          )
