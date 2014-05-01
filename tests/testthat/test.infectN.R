test_that("infection functions", {
    g = makedata()
    g0 = infectN(0)(g)
    expect_true(all(V(g)$state == "S"))
    g2 = infectN(2)(g)
    expect_true(sum(V(g2)$state == "I")==2)
    expect_error(infectN(1000)(g))
})
