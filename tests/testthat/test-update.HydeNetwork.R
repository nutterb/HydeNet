context("update.HydeNetwork")

carNet <- HydeNetwork(~gear | mpg * am,
                      data = mtcars) 

test_that("update by adding a node",
{
  expect_silent(update(carNet, ~ . + cyl | am))
})
  
test_that("update and lose a parent",
{
  expect_warning( update(carNet, ~ . - am) )
})
    