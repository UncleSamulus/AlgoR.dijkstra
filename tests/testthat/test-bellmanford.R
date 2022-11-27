test_that("Bellman-Ford gives the proper shortest path", {

  summits <- c(1, 2, 3)
  edges <- list(
    list(from = 1, to = 2, weight = 10),
    list(from = 1, to = 3, weight = 4),
    list(from = 3, to = 2, weight = 2)
  )

  result <- bellmanford(summits, edges, 1)

  path <- shortestpath(result$pred, 1, 2)

  expect_equal(c(1, 3, 2), path)
})
