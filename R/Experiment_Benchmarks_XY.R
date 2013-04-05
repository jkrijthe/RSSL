library(xtable)

# Chapelle Benchmarks
dnames<-c("Digit1","USPS","COIL2","BCI","g241c","COIL","g241n")
measurements<-6
classname<-"y"
n_ls<-c(10,100)
exp_results<-array(dim=c(2,7,6,12))

# There are nine data sets
for (d in c(5,7)) {
  print(dnames[d])
  X<-as.matrix(read.table(paste("Data/SSL,set=",d,",X.tab",sep="")))
  y<-as.integer(as.factor((read.table(paste("Data/SSL,set=",d,",y.tab",sep="")))$V1))
  
  #pb<-txtProgressbar
  for (n in 1:length(n_ls)) {
    n_l<-n_ls[n]
    splits<-data.matrix(read.table(paste("Data/SSL,set=",d,",splits,labeled=",n_l,".tab",sep="")))
    for (f in 1:nrow(splits)) {
      print(f)
      X<-cbind(rep(1,nrow(X)),X)
      X_l<-X[splits[f,],]
      y_l<-y[splits[f,]]
      X_u<-X[-splits[f,],]
      X_test<-X_u
      y_test<-y[-splits[f,]]
      
#       if (TRUE) {
#         leps<-1.0
#         bestl<-1.0
#         for (l in c(10,1,0.5,0.1,0.05,0.01,0.0)) {
#           eps<-1-mean(y_l==factor(bootstrap::crossval(X_l, y_l, function(x,y) {LeastSquaresClassifierXY(x,y,lambda=l)}, predict, ngroup=5)$cv.fit))
#           if (eps<leps) {
#             bestl<-l
#             leps<-eps
#           }
#         }
#         print(bestl)
#       }
      bestl=100
      
      g_constrained <- ICLeastSquaresClassifierXY(X_l, y_l, X_u, lambda1=bestl,lambda2=bestl)
      g_unconstrained <- LeastSquaresClassifierXY(X_l, y_l, lambda=bestl)
      #g_svm <- svm(modelform, D_l)
      #g_lr <- LogisticRegression(modelform,D_l)
      #g_tsvm <- TransductiveSVM(modelform,D_train)
      #g_erlr <- EntropyRegularizedLogisticRegression(modelform,D_train,init=g_lr@theta)
      
      # Results: Classification errors on test set
      exp_results[n,d,1,f]<-1-mean(y_test==predict(g_constrained,X_test))
      exp_results[n,d,2,f]<-1-mean(y_test==predict(g_unconstrained,X_test))
      exp_results[n,d,3,f]<-loss(g_constrained,X_test,y=y_test)
      exp_results[n,d,4,f]<-loss(g_unconstrained,X_test,y=y_test)
      exp_results[n,d,5,f]<-loss(g_constrained,X_l,y=y_l)
      exp_results[n,d,6,f]<-loss(g_unconstrained,X_l,y=y_l)
      #exp_results[n,d,3,f]<-1-mean(D_test[,classname]==predict(g_svm,D_test))
      #exp_results[n,d,4,f]<-1-mean(D_test[,classname]==predict(g_lr,D_test))
      #exp_results[n,d,5,f]<-1-(predict(g_tsvm,D_test)/100)
      #exp_results[n,d,6,f]<-1-mean(D_test[,classname]==predict(g_erlr,D_test))
    }
  }
}


for (n in 1:2) {
  res_table<-matrix(nrow=7,ncol=6,dimnames=list(dnames,c("SLLS","LS","SVM","LR","TSVM","ERLR")))
  for (d in 1:7) {
    res_table[d,]<-rowMeans(exp_results[n,d,,])
  }
  #Print Latex table
  print(xtable(res_table))
}