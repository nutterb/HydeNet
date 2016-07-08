context("writeNetworkModel")

Net <- HydeNetwork(~ wells +
                     pe | wells +
                     d.dimer | pregnant*pe +
                     angio | pe +
                     treat | d.dimer*angio +
                     death | pe*treat,
                   data = PE)

test_that("writeNetworkModel with pretty output succeeds",
{
  expect_output(writeNetworkModel(Net, pretty = TRUE))
})

test_that("writeNetworkModel with non-pretty output succeeds",
{
  expect_silent(writeNetworkModel(Net, pretty = FALSE))
})