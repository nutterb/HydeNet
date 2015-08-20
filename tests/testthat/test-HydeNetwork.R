context("HydeNetwork")

data(PE, package='HydeNet')
Net <- HydeNetwork(~ wells
                       + pe | wells
                       + d.dimer | pregnant*pe
                       + angio | pe
                       + treat | d.dimer*angio
                       + death | pe*treat,
                       data = PE)

test_that("HydeNetwork.formula returns expected attributes",
{
  expect_equal(names(Net),
               c("nodes", "parents", "nodeType", "nodeFormula", "nodeFitter",
                 "nodeFitterArgs", "nodeParams", "fromData", "nodeData",
                 "nodeModel", "nodeDecision", "nodeUtility", "dag", 
                 "data", "network_formula"))
})

test_that("HydeNetwork.formula assigns correct node types",
{
  expect_equal(Net$nodeType,
               list(wells = "dnorm", 
                    pe = "dbern",
                    d.dimer = "dnorm",
                    pregnant = "dcat",
                    angio = "dcat",
                    treat = "dbern",
                    death = "dcat"))
})