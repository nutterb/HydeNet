context("writeJagsFormula")

test_that("writeJagsFormula: Poisson Regression",
{
  fit <- glm(gear ~ mpg + am, data = mtcars, family = poisson)
  expect_silent(writeJagsFormula(fit, c("gear", "mpg", "am")))
})

test_that("writeJagsFormula: Multinomial Regression",
{
  fit.gear <- multinom(gear ~ mpg + factor(am), data=mtcars)
  expect_silent(writeJagsFormula(fit.gear))
})
