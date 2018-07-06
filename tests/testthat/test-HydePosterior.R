context("HydeSim")

data(PE, package="HydeNet")
Net <- HydeNetwork(~ wells +
                     pe | wells +
                     d.dimer | pregnant*pe +
                     angio | pe +
                     treat | d.dimer*angio +
                     death | pe*treat,
                   data = PE)
compiledNet <- compileJagsModel(Net, n.chains=5)

test_that("Unbound HydeSim returns object of class HydeSim",
{
  Posterior <- HydeSim(compiledNet,
                             variable.names = c("d.dimer", "death"),
                             n.iter = 1000, 
                             bind = FALSE)  
  expect_equal(class(Posterior),
               "HydeSim")
})

test_that("Bound HydeSim returns object of class data.frame",
{
  Posterior <- HydeSim(compiledNet,
                             variable.names = c("d.dimer", "death"),
                             n.iter = 1000)  
  expect_equal(class(Posterior),
               "data.frame")
})

test_that("Unbound HydeSim print method",
{
  Posterior <- HydeSim(compiledNet,
                             variable.names = c("d.dimer", "death"),
                             n.iter = 1000, 
                             bind = FALSE)  
  expect_output(print(Posterior))
})

test_that("bindSim returns relabeled data",
{
  Posterior <- HydeSim(compiledNet,
                             variable.names = c("d.dimer", "death"),
                             n.iter = 1000, 
                             bind = FALSE) 
  Bound <- bindSim(Posterior)
  expect_equal(class(Bound$death),
               "factor")
})

test_that("bindSim returns relabeled data",
{
  Posterior <- HydeSim(compiledNet,
                             variable.names = c("d.dimer", "death"),
                             n.iter = 1000, 
                             bind = FALSE) 
  Bound <- bindSim(Posterior, relabel_factor = FALSE)
  expect_equal(class(Bound$death),
               "numeric")
})