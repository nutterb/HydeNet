context("writeNetworkModel")

Net <- HydeNetwork(~ wells +
                     pe | wells +
                     d.dimer | pregnant*pe +
                     angio | pe +
                     treat | d.dimer*angio +
                     death | pe*treat,
                   data = PE)

# expect_that("writeNetworkModel with pretty output succeeds",
# {
#   expect_that(writeNetworkModel(Net, pretty = TRUE),
#               not(throws_error()))
# })
# 
# expect_that("writeNetworkModel with non-pretty output succeeds",
# {
#   expect_that(writeNetworkModel(Net, pretty = FALSE),
#               not(throws_error()))
# })