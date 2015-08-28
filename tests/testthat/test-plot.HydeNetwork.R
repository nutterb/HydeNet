context("plot.HydeNetwork")

data(BlackJack)

test_that("plot.HydeNetwork returns a plot under default settings",
{
  expect_that(plot(BlackJack), 
              not(throws_error()))
})