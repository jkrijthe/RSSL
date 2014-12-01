#' TODO: Document the usage of these functions
model_svmlin <- list(label = "SVMlin",
                          library = c("RSSL"),
                          type = "Classification",
                          ## Tune over both parameters at the same time
                          parameters = data.frame(parameter = c('lambda',"lambda_u"),
                                                  class = c("numeric","numeric"),
                                                  label = c('Lambda',
                                                            'Lambda_u')),
                          grid = function(x, y, len = NULL) {
                            grid <- expand.grid(lambda = c(0.001,0.1,1),
                                                lambda_u=c(0,0.1,0.5,1))
                          },
                          loop = NULL,
                          fit = function(x, y, wts, param, lev, last, classProbs,X_u, ...) {
                            #                  print("Fit!!")
                            #                  print(param)
                            mod <- SVMlin(as.matrix(x),y,X_u,binary_path="~/Dropbox/Code/svmlin/",temp_path="/Volumes/Experiments/")
                          },
                          predict = function(modelFit, newdata, submodels = NULL) {
                            predict(modelFit,as.matrix(newdata))
                            
                          },
                          prob = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
                            predict(modelFit, as.matrix(newdata))
                          },
                          varImp = NULL,
                          predictors = function(x, ...) rownames(x$projection),
                          levels = function(x) colnames(x$mod$w),
                          sort = function(x) x[order(x[,1]),])