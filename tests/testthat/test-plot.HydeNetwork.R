context("plot.HydeNetwork")

data(BlackJack)

test_that("plot.HydeNetwork returns a plot under default settings",
{
  expect_silent(plot(BlackJack))
})

test_that("plot.HydeNetwork returns a plot with custom Node settings",
{
  expect_silent(plot(BlackJack,
                   customNodes = customNode(node_id = "hit1",
                                            fillcolor = "purple", shape = "circle",
                                            fontcolor = "white", height = "2",
                                            style="filled")))
})

test_that("HydePlotOptions",
{
  expect_silent({
    HydePlotOptions(variable = list(shape = "rect", fillcolor = "#A6DBA0"),
                  determ = list(shape = "rect", fillcolor = "#E7D4E8",
                                fontcolor = "#1B7837", linecolor = "#1B7837"),
                  decision = list(shape = "triangle", fillcolor = "#1B7837",
                                  linecolor = "white"),
                  utility = list(shape = "circle", fillcolor = "#762A83", 
                                 fontcolor = "white"))
    plot(BlackJack)
  })
})

test_that("HydePlotOptions - restoreDefaults",
{
  expect_silent({
    HydePlotOptions(restorePackageDefaults = TRUE)
    plot(BlackJack)})
})

test_that("Remove Deterministic Nodes",
{
  expect_silent({
    plot(BlackJack, removeDeterm = TRUE)})
})