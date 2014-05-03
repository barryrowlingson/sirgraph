test_that("continuous sim",{

    s = cspreadR(.1,.1)

    g = makedata()
    s(g)
    
    ## no infected, so all still S
    expect_true(all(V(g)$state=="S"))

    ## infect two
    g = infectN(2)(g)

    ## recovery time not set, so error
    expect_error(s(g))

    ## set recovery using exp rate
    g = addRecovery(g,.1)

    ## now stepping works
    g = s(g)

    ## should now not have two cases (we could have had a recovery
    ## or a new infection, either way, no longer two infections)
    expect_true(sum(V(g)$state=="I")!=2)

    ## at some time after start
    expect_true(g$time > g$start)
}
          )

test_that("fixing up infections",{
    g = graph.formula(A-B,B-C,C-D)
    g = makedata(g)
    g = infectN(2)(g)
    g = addRecovery(g,2)
})
