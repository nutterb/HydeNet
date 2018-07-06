context("bindSim")

data(PE, package="HydeNet")
Net <- HydeNetwork(~ wells +
                     pe | wells +
                     d.dimer | pregnant*pe +
                     angio | pe +
                     treat | d.dimer*angio +
                     death | pe*treat,
                   data = PE) %>%
  setDecisionNodes(treat, angio)
Post <- compileDecisionModel(Net) %>%
  HydeSim(variable.names = c("wells", "treat", "death"),
                n.iter = 100,
                bind = FALSE) 

test_that("bindSim from Decision Model",
{
  expect_silent(bindSim(Post))
})
