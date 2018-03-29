context("MCPL LDA")

test_that("Projection to simplex gives correct results",{
  expect_equal(RSSL:::projection_simplex(diag(c(1,2,3))), 
               diag(3))
})

test_that("wlda model does not crash",{
  data(wdbc)
  expect_silent(RSSL:::wlda(wdbc$X,model.matrix(~as.factor(wdbc$y)-1,data.frame(y=wdbc$y))))
})

test_that("Interface gives same result as direct calling of code",{
  data(wdbc)

  res_direct <- RSSL:::minimaxlda(wdbc$X,
              model.matrix(~as.factor(wdbc$y)-1,data.frame(y=wdbc$y)),
              wdbc$X,
              1000)
  
  ll_direct <- RSSL:::wlda_loglik(m=res_direct$m,p=res_direct$p,
                     iW=res_direct$iW,
                     a=wdbc$X,
                     w=model.matrix(~as.factor(wdbc$y)-1,data.frame(y=wdbc$y)))
  
  error_direct <- RSSL:::wlda_error(m=res_direct$m,p=res_direct$p,
                    iW=res_direct$iW,
                    a=wdbc$X,
                    w=model.matrix(~as.factor(wdbc$y)-1,data.frame(y=wdbc$y)))
  
  res_interface <- MCPLDA(wdbc$X,as.factor(wdbc$y),wdbc$X)
  ll_interface <- mean(loss(res_interface,wdbc$X,as.factor(wdbc$y)))
  error_interface <- mean(predict(res_interface,wdbc$X)!=as.factor(wdbc$y))
  
  expect_equivalent(-as.numeric(ll_direct),ll_interface)
  expect_equal(error_direct,error_interface)
})