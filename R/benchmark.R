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

# Chapelle Benchmarks (1-7)
modelform<-formula("factor(classes)~.")

Ds_benchmark<-list()
results<- matrix(nrow=7,ncol=2)
for (i in 1:7) {
  y<-read.delim(paste('~/Dropbox/Code/RSSL/Data/SSL,set=',i,',y.tab',sep=''),header=FALSE)[,1]
  X<-read.delim(paste('~/Dropbox/Code/RSSL/Data/SSL,set=',i,',X.tab',sep=''),header=FALSE)
  splits10<-data.matrix(read.delim(paste('~/Dropbox/Code/RSSL/Data/SSL,set=',i,',splits,labeled=10.tab',sep=''),header=FALSE)[,-11])
  splits100<-data.matrix(read.delim(paste('~/Dropbox/Code/RSSL/Data/SSL,set=',i,',splits,labeled=100.tab',sep=''),header=FALSE)[,-101])
  D<-data.frame(classes=y,X[,-ncol(X)])
  Ds_benchmark[[i]]<-D
  
  splits<-splits10
  iresults<- matrix(nrow=nrow(splits),ncol=2)
  for (r in 1:nrow(splits)) {
    D_l<-D[splits[r,],]
    D_u<-D[-splits[r,],]
    D_test<-D_u
    D_u[,classname]<-rep(NA,nrow(D_u))
    D_train<-rbind(D_l,D_u)
    
    g_mcnm<-mcnm(modelform,D_train)
    g_nm<-nearestmean(modelform,D_l)
    #g_selflearning<-selflearning(modelform,D_train)
    
    iresults[r,1]<-1-mean(D_test$class==predict(g_mcnm,D_test))
    iresults[r,2]<-1-mean(D_test$class==predict(g_nm,D_test))
  }
  results[i,]<-colMeans(iresults)
}

lapply(Ds_benchmark,ncol)
lapply(Ds_benchmark,nrow)
