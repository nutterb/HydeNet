context("plot.HydeNetwork")

data(BlackJack)

test_that("plot.HydeNetwork returns a plot under default settings",
{
  expect_that(plot(BlackJack), 
              not(throws_error()))
})

test_that("plot.HydeNetwork returns a plot with custome Node settings",
{
  expect_that(plot(BlackJack,
                   customNodes = customNode(node_id = "hit1",
                                            fillcolor = "purple", shape = "circle",
                                            fontcolor = "white", height = "2",
                                            style="filled")),
              not(throws_error()))
})