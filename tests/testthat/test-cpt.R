context("cpt")

test_that("cpt.list",
{
  n <- 50000
  data <- data.frame(
    di1 = as.factor(1:6 %*% rmultinom(n,1,prob=c(.4,.3,.15,.10,.03,.02))),
    di2 = as.factor(1:6 %*% rmultinom(n,1,prob=rev(c(.4,.3,.15,.10,.03,.02)))),
    di3 = as.factor(1:6 %*% rmultinom(n,1,prob=c(.15,.10,.02,.3,.4,.03)))
  )
  
  expect_that(cpt(list(y = "di3", x = c("di1", "di2")), data= data),
              not(throws_error()))
})

test_that("cpt with weights",
{
  echodata <- cbind(expand.grid(list(echo = c("Negative", "Positive"),
                                     cad = c("No","Yes"))),
                    data.frame(pr=c(0.83,0.17,0.12,0.88)))
  expect_that(cpt(echo ~ cad, data=echodata, wt=echodata$pr),
              not(throws_error()))
})
  