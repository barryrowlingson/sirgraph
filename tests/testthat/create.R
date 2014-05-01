test_that("sir graph demo objects can be created", {
    g = makedata()
    expect_is(g, "igraph")
    gLayout = glayout(g)
    expect_is(gLayout, "igraph")
}
          )

