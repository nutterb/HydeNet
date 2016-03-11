context("print.HydePosterior")

data(PE, package="HydeNet")
Net <- HydeNetwork(~ wells +
                     pe | wells +
                     d.dimer | pregnant*pe +
                     angio | pe +
                     treat | d.dimer*angio +
                     death | pe*treat,
                   data = PE)
compiledNet <- compileJagsModel(Net, n.chains=5, data = list(pe = "Yes"))

test_that("print.HydePosterior with observed values",
{
  expect_output(print(HydePosterior(compiledNet, 
                                    variable.names = c("wells", "death"),
                                    n.iter = 100, bind = FALSE)))
})