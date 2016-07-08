context("print.HydeNetwork")

data(BlackJack)

test_that("print.HydeNetwork works for full network",
{
  expect_output(print(BlackJack))
})

test_that("print.HydeNetwork works for selected nodes",
{
  expect_output(print(BlackJack, dealerFinalPoints, payoff, card5))
})