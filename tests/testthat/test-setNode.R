context("setNode")

Net <- HydeNetwork(~ wells +
                     pe | wells +
                     d.dimer | pregnant*pe +
                     angio | pe +
                     treat | d.dimer*angio +
                     death | pe*treat,
                   data = PE)

test_that("decision argument warning",
{
  expect_warning(setNode(Net, treat, nodeType = "dbern", decision = "yes", p = .5))
})

test_that("utility error: utilties must be deterministic and not have children",
{
  expect_error(setNode(Net, treat, nodeType = "dbern", utility = TRUE, p =.5))
})  

test_that("validation error",
{
  expect_error(setNode(Net, treat, nodeType = "dbern", p = 1.2))
})

test_that("fit the model for dbern",
{
  expect_that(setNode(Net, treat, nodeType = "dbern", p = fromData(),
                 fitModel = TRUE),
              not(throws_error()))
})

test_that("fit the model for dcat",
{
  expect_that(setNode(Net, pregnant, nodeType = "dcat", pi = fromData(),
                      fitModel = TRUE),
              not(throws_error()))
})

test_that("fit the model for dnorm",
{
  expect_that(setNode(Net, d.dimer, nodeType = "dnorm",
                      mu = fromData(), tau = fromData(),
                      fitModel = TRUE),
              not(throws_error()))
})

test_that("fit the model for dpois",
{
  carNet <- HydeNetwork(~gear | mpg + am,
                                  data = mtcars)

  expect_that(
    setNode(carNet, gear, nodeType = "dpois", nodeFitter = "glm",
            fitterArgs = list(family = poisson),
            lambda = fromData(),
            fitModel = TRUE),
    not(throws_error()))
})
  