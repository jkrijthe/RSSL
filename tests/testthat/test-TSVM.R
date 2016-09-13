context("Transductive SVM")

X <- matrix(c(0,0.001,1,-1),nrow=2)
X_u <- matrix(c(-1,-1,-1,0,0,0,-0.4,-0.5,-0.6,1.2,1.3,1.25),ncol=2)
y <- factor(c(-1,1))
ynum <- factor_to_dummy(y)[1,]

g_lin <- LinearTSVM(X=X,y=y,X_u=X_u,C=1,Cstar=1,x_center=TRUE)
g_constraint <- TSVM(X=X,y=y,X_u=X_u,C=1,Cstar=1,
                     balancing_constraint = TRUE,x_center=TRUE)
g_noconstraint <- TSVM(X=X,y=y,X_u=X_u,C=1,Cstar=1,
                       balancing_constraint = FALSE,x_center=TRUE)

test_that("TSVM equal to SVM if Cstar is near zero",{
  # Note the balancing_constraint=FALSE for this to work.
  g_sup <- LinearSVM(X=X,y=y,C=1,scale=FALSE)
  
  g_semi <- LinearTSVM(X=X,y=y,X_u=X_u,C=1,Cstar=0,x_center=FALSE)
  expect_equal(g_sup@w,g_semi@w,tolerance=1e-4)
  expect_equal(decisionvalues(g_sup,X_u),decisionvalues(g_semi,X_u),tolerance=1e-4)
  
  # g_sup <- SVM(X=X,y=y,C=1,scale=FALSE)
  # g_semi <- TSVM(X=X,y=y,X_u=X_u,C=1,Cstar=0.001,x_center=FALSE,balancing_constraint = FALSE)
  # expect_equal(decisionvalues(g_sup,X_u),decisionvalues(g_semi,X_u),tolerance=10e-3)
  
  # g_sup <- SVM(X=X,y=y,C=10000,kernel=kernlab::rbfdot(1),scale=FALSE)
  # g_semi <- TSVM(X=X,y=y,X_u=X_u,C=10000,Cstar=0.003,kernel=kernlab::rbfdot(1),x_center=FALSE,verbose=FALSE,balancing_constraint = FALSE)
  # expect_equal(decisionvalues(g_sup,X_u),decisionvalues(g_semi,X_u),tolerance=10e-3)
  
})

test_that("Example gives same result for Linear and Kernel implementation",{

  w1 <- g_constraint@alpha %*% rbind(X,X_u,X_u,colMeans(X_u))
  w2 <- c(g_noconstraint@bias, 
          g_noconstraint@alpha %*% rbind(X,X_u,X_u))
  w3 <- g_lin@w
  expect_equal(w2,w3,tolerance=10e-5)
})

test_that("Prediction works", {
  decisionvalues(g_constraint,X_u)
  predict(g_constraint,X_u)
  expect_equal(decisionvalues(g_noconstraint,X_u),
               decisionvalues(g_lin,X_u),tolerance=10e-5)
})

test_that("Gradient of TSVM CCCP linear is superficially correct.",{
  for (i in 1:10) {
    w_1 <- runif(2)
    w_2 <- runif(2)
    expect_equal(tsvm_cccp_lin_gradient(w_1,w_2,X,X_u,y=ynum,y_u=rep(c(-1,1),each=nrow(X_u)),
                                        s=0,C=1,Cstar=0.1),
                  numDeriv::grad(tsvm_cccp_lin_objective,w_1,w_now=w_2,X=X,X_u=X_u,y=ynum,
                                 y_u=rep(c(-1,1),each=nrow(X_u)),C=1,Cstar=0.1)
    )
  }
})

test_that("Adaptable hinge loss",{
  expect_equal(hs(c(100,1.5,0.5,-0.5),s=1),c(0,0,0.5,1.5))
})