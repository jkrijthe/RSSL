# Chapelle Benchmarks

dnames<-c("Digit1","USPS","COIL2","BCI","g241c","COIL","g241n")
measurements<-6
classname<-"y"
modelform<-formula("y~.")
n_ls<-c(10,100)
exp_results<-array(dim=c(2,7,6,12))

# There are nine data sets
for (d in c(7)) {
  print(dnames[d])
  X<-read.table(paste("Data/SSL,set=",d,",X.tab",sep=""))
  y<-read.table(paste("Data/SSL,set=",d,",y.tab",sep=""))
  D<-data.frame(X,y=as.factor(y$V1))
  
  for (n in 1:length(n_ls)) {
    n_l<-n_ls[n]
    splits<-data.matrix(read.table(paste("Data/SSL,set=",d,",splits,labeled=",n_l,".tab",sep="")))
  
  
  for (f in 1:nrow(splits)) {
    print(f)
    D_test<-D[-splits[f,],]
    D_train<-D
    D_train[-splits[f,],classname]<-rep(NA,nrow(D_train)-ncol(splits))
    D_l<-D[splits[f,],]
    
#     if (TRUE) {
#       
#       
#       X <- model.matrix(modelform, D_l)
#       
#       y <- as.factor(data.matrix(D_l[,classname]))
#       
#       leps<-1.0
#       bestl<-1.0
#       for (l in c(1,0.5,0.1,0.05,0.01,0.0)) {
#         
#         eps<-1-mean(D_l[,classname]==factor(bootstrap::crossval(X, y, function(x,y) {FisherClassifier.xy(x,y,modelform,lambda=l,scale=TRUE)}, predictxy, ngroup=2)$cv.fit,levels=1:2,labels=levels(y)))
#         #eps<-1-mean(CrossValidation(function(D){FisherClassifier(modelform,D,lambda=l)},D_l, k=nrow(D_l))==D_l[,classname])
#         #print(eps)
#         if (eps<leps) {
#           bestl<-l
#           leps<-eps
#         }
#       }
#       print(bestl)
#     }
    bestl<-100
    g_constrained <- ICLeastSquaresClassifier(modelform, D_train,lambda1=bestl,lambda2=bestl,scale=FALSE)
    g_unconstrained <- LeastSquaresClassifier(modelform, D_l,lambda=bestl,scale=FALSE)
    #g_svm <- svm(modelform, D_l)
    #g_lr <- LogisticRegression(modelform,D_l)
    #g_tsvm <- TransductiveSVM(modelform,D_train)
    #g_erlr <- EntropyRegularizedLogisticRegression(modelform,D_train,init=g_lr@theta)
    
    # Results: Classification errors on test set
    exp_results[n,d,1,f]<-1-mean(D_test[,classname]==predict(g_constrained,D_test))
    exp_results[n,d,2,f]<-1-mean(D_test[,classname]==predict(g_unconstrained,D_test))
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

# Benchmark all different techniques thoroughly using the data from Chapelle and Schollkopf

# include the supervised method for only labelled data and for all data as an upper and lower bound
# include two clustering followed by supervised procedures
# 
# 
# # for each in list of datasets
# calculate supervised variant
# for size of unlabelled data
#   for number of repeats
#     for each method
#       run method and return test accuracy and optimization function
# 
# selflearning
# 
# # Chapelle Benchmarks (1-7)
# modelform<-formula("factor(classes)~.")
# 
# Ds_benchmark<-list()
# results<- matrix(nrow=7,ncol=2)
# for (i in 1:7) {
#   y<-read.delim(paste('~/Dropbox/Code/RSSL/Data/SSL,set=',i,',y.tab',sep=''),header=FALSE)[,1]
#   X<-read.delim(paste('~/Dropbox/Code/RSSL/Data/SSL,set=',i,',X.tab',sep=''),header=FALSE)
#   splits10<-data.matrix(read.delim(paste('~/Dropbox/Code/RSSL/Data/SSL,set=',i,',splits,labeled=10.tab',sep=''),header=FALSE)[,-11])
#   splits100<-data.matrix(read.delim(paste('~/Dropbox/Code/RSSL/Data/SSL,set=',i,',splits,labeled=100.tab',sep=''),header=FALSE)[,-101])
#   D<-data.frame(classes=y,X[,-ncol(X)])
#   Ds_benchmark[[i]]<-D
#   
#   splits<-splits10
#   iresults<- matrix(nrow=nrow(splits),ncol=2)
#   for (r in 1:nrow(splits)) {
#     D_l<-D[splits[r,],]
#     D_u<-D[-splits[r,],]
#     D_test<-D_u
#     D_u[,classname]<-rep(NA,nrow(D_u))
#     D_train<-rbind(D_l,D_u)
#     
#     g_mcnm<-mcnm(modelform,D_train)
#     g_nm<-nearestmean(modelform,D_l)
#     #g_selflearning<-selflearning(modelform,D_train)
#     
#     iresults[r,1]<-1-mean(D_test$class==predict(g_mcnm,D_test))
#     iresults[r,2]<-1-mean(D_test$class==predict(g_nm,D_test))
#   }
#   results[i,]<-colMeans(iresults)
# }
# 
# lapply(Ds_benchmark,ncol)
# lapply(Ds_benchmark,nrow)
