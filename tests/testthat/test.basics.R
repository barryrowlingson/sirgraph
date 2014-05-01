test_that("basic ops", {
    g = graph.formula(A-B, A-C, C-B)
    expect_error(infectN(1)(g)) # no state
    V(g)$state = "S"
    expect_error(infectN(1)(g)) # no time
    g$start = 0
    g$time = 0
    g = infectN(1)(g)
}
          )

    
