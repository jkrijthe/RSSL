rm(list=ls())
source("Requirements.R")

library(mlbench)
library(sampling)
library(caret)
library(foreign)
library(parallel)
library(plotrix)
library(ggplot2)
library(gridExtra)

## Settings

# Output
outputdir<-"~/"
current.time<-Sys.time()

# Measurements
# classifiers<-c(LeastSquaresClassifierXY,
#                function(X,y,X_u) {LeastSquaresClassifierXY(X,y,lambda=200)},
#                ICLeastSquaresClassifierXY,
#                function(X,y,X_u){ICLeastSquaresClassifierXY(X, y, X_u, lambda1=200,lambda2=200)},
#                 function(X,y,X_u){ICLeastSquaresClassifierXY(X,y,X_u,lambda1=200,lambda2=0)},
#                function(X,y,X_u){ICLeastSquaresClassifierXY(X,y,X_u,lambda1=0,lambda2=200)}
#                )
# classifiers.names<-c("LS","LS l=1","ICLS","ICLS l=1,1","ICLS l=1,0","ICLS l=0,1")

classifiers<-c(LogisticRegressionXY,
                LeastSquaresClassifierXY,
               ICLeastSquaresClassifierXY,
               function(X,y,X_u) {ICLeastSquaresClassifierXY(X,y,rbind(X))},
               function(X,y,X_u) {ICLeastSquaresClassifierXY(X,y,rbind(X_u,X))})
classifiers.names<-c("LR","LS","ICLS","ICLS2Xl","ICLS2XlXu")
intercept<-TRUE

# classifiers<-c(LeastSquaresClassifierXY,
#                ICLeastSquaresClassifierXY,
#                NearestMeanClassifierXY,
#                MCNearestMeanClassifierXY
# )
# classifiers.names<-c("LS","ICLS","NM","MCNM")



# Experiment: Real NMC
classifiers<-c(function(X,y,X_u) { NearestMeanClassifierXY(X,y,prior=matrix(c(0.5,0.5),2,1)) },
               function(X,y,X_u) { MCNearestMeanClassifierXY(X,y,X_u, prior=matrix(c(0.5,0.5),2,1))},
               function(X,y,X_u) { SelfLearningXY(X,y,X_u, method=function(X,y) { NearestMeanClassifierXY(X,y,prior=matrix(c(0.5,0.5),2,1)) })})
classifiers.names<-c("NM.fixedprior","MCNM.fixedprior","SelfLearning")
intercept<-FALSE



classifiers<-c(function(X,y,X_u) {LeastSquaresClassifierXY(X,y)},
               function(X,y,X_u) {ICLeastSquaresClassifierXY(X,y,X_u)},
               function(X,y,X_u) {LogisticRegressionXY(X,y)})
classifiers.names<-c("LS","ICLS","LR")
intercept<-TRUE

# Experiment: Prior free NMC
classifiers<-c(function(X,y,X_u) { NearestMeanClassifierXY(X,y) },
               function(X,y,X_u) { MCNearestMeanClassifierXY(X,y,X_u) },
               function(X,y,X_u) { SelfLearningXY(X, y, X_u, method=NearestMeanClassifierXY)})
classifiers.names<-c("NM","MCNM","SelfLearning")
intercept<-FALSE

#Experiment: Least Squares classifier
classifiers<-c(function(X,y,X_u) {LeastSquaresClassifierXY(X,y) },
               function(X,y,X_u) {ICLeastSquaresClassifierXY(X,y,X_u) },
               function(X,y,X_u) {LogisticRegressionXY(X,y)},
               function(X,y,X_u) { SelfLearningXY(X, y, X_u, method=LeastSquaresClassifierXY)})
classifiers.names<-c("LS","ICLS","LR","SelfLearning")
intercept<-TRUE

# Data
load("Datasets.RData")

datasets<-datasets[11:16]

# For measurements, see below
measurements.names<-c("Error","Loss Test","Loss Train")
repeats<-5
n_l<-"justenough"
n_test<-1000
sizes<-c(2^(1:12))

LearningCurve<-function(modelform, data, classifiers, repeats=1000, sampling="empirical") {
  
  classname<-all.vars(modelform)[1]
  #results<-data.frame(Classifier=integer(),Size=integer(),Repeat=integer(),setNames(replicate(length(measurements.names),numeric(0), simplify = F), measurements.names))
  results<-array(NA, dim=c(repeats, length(sizes), length(classifiers), 3))
  
  # Data set to design matrix
  list2env(SSLDataFrameToMatrices(modelform,data,intercept=intercept),env=environment())
  
  # Generate the test sample
  sample.test<-sample(1:nrow(X), n_test, replace=TRUE)
  X_test<-X[sample.test,]
  y_test<-y[sample.test]
  
  
  n_l<-ncol(X)+30
  for (i in 1:repeats) {
    print(i) # Print the current repeat
    
    sample.labeled<-strata(data,classname,c(1,1),method="srswr")$ID_unit
    sample.labeled<-c(sample.labeled, sample(1:nrow(data),n_l-2,replace=TRUE))
    X_l<-X[sample.labeled,]
    y_l<-y[sample.labeled]
    
    sample.unlabeled<-sample(1:nrow(data),max(sizes),replace=TRUE)
    X_u<-X[sample.unlabeled,]
    
#     if (TRUE) {
#       leps<-1.0
#       bestl<-1000.0
#       for (l in c(10000,1000,500,200,100,10,1,0.1,0.01,0.001,0.0001,0.0000)) {
#         #eps<-1-mean(y_l==factor(bootstrap::crossval(X_l, y_l, function(X,y) {LeastSquaresClassifierXY(X,y,lambda=l)}, predict, ngroup=5)$cv.fit))
#         eps<-1-mean(y_test==predict(LeastSquaresClassifierXY(X_l,y_l,lambda=l),X_test))
#         
#         if (eps<leps) {
#           bestl<-l
#           leps<-eps
#         }
#       }
#       print(leps)
#       print(bestl)
#     }
    
    for (s in 1:length(sizes)) {
      X_u_s <- X_u[1:sizes[s],]
      for (c in 1:length(classifiers)) {
        try({
        
        
#         classifiers<-c(LeastSquaresClassifierXY,
#                        function(X,y,X_u) {LeastSquaresClassifierXY(X,y,lambda=bestl)},
#                        ICLeastSquaresClassifierXY,
#                        function(X,y,X_u){ICLeastSquaresClassifierXY(X, y, X_u, lambda1=bestl,lambda2=bestl)},
#                        function(X,y,X_u){ICLeastSquaresClassifierXY(X,y,X_u,lambda1=bestl,lambda2=0)},
#                        function(X,y,X_u){ICLeastSquaresClassifierXY(X,y,X_u,lambda1=0,lambda2=bestl)}
#         )
        
        trained_classifier<-do.call(classifiers[[c]],list(X_l, y_l, X_u=X_u_s))
        
        results[i,s,c,1] <- 1-mean(y_test==predict(trained_classifier,X_test))
        results[i,s,c,2] <- loss(trained_classifier, X_test, y_test)
        results[i,s,c,3] <- loss(trained_classifier, X_l, y_l)
        #results[nrow(results)+1,]<-c(c,sizes[s],i,sapply(measurements,do.call,list(trained_classifier)))
        #results[i,s,c,1]<-system.time()[3]
        #results[i,s,c,2:4]<-sapply(measurements,do.call,list(trained_classifier))
        })
        }
    }
    
  }
  # Write Intermediate Results to disk
  #save.image(file=paste(outputdir,"LearningCurves - ",current.time,".RData",sep=""))
  return(results)
}

results<-lapply(names(datasets),function(dname){print(dname); LearningCurve(modelforms[[dname]],datasets[[dname]],classifiers,repeats)})
names(results)<-names(datasets)
save.image(file=paste(outputdir,"LearningCurves - ",current.time,".RData",sep=""))
## In how many repeats is each method the best
count(apply(results$g241c[,3,,1], 1, which.min))


## Visualize 95% confidence bounds

## Visualize
# Visualize Results
for (m in 1:length(measurements.names)) {
  plots<-list()
  for (d in c(1:(length(results)))) {
    
    results.mean<-apply(results[[d]],c(2,3,4),mean)[,,m]
    results.stderror<-apply(results[[d]],c(2,3,4),std.error)[,,m]
    
    results.merged<-merge(melt(results.mean),melt(results.stderror),by=c("X1","X2"))
    names(results.merged)<-c("Size","Classifier","Mean","Std.Error")
    results.merged<-data.frame(results.merged)
    
    results.merged$Classifier<-factor(results.merged$Classifier,labels=classifiers.names)
    h <- ggplot(results.merged, aes(x=Size,y=Mean,group=Classifier,color=Classifier))
    
    h <- h + geom_line(aes(y=Mean,group=Classifier,color=Classifier))
    h <- h + geom_ribbon(aes(ymin=Mean-Std.Error, ymax=Mean+Std.Error,group=Classifier,color=Classifier,fill=Classifier),colour=NA,alpha=0.5)
    h <- h + scale_x_discrete(breaks = 1:length(sizes), labels=sizes) 
    h <- h + xlab("Number of unlabeled objects")
    h <- h + ggtitle(names(datasets)[d])
    
    tmp <- ggplot_gtable(ggplot_build(h))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    
    h <- h + theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_blank(),
      panel.background = element_blank(),
      legend.position="none"
    )
    h<- h + ylab(measurements.names[m])
    plots[[d]]<-h
  }
  
  
  pdf(paste(outputdir,"LearningCurves - ",n_l," - ",measurements.names[m]," - ",current.time,".pdf",sep=""),paper="a4r",height=8,width=11)
  do.call(grid.arrange, c(plots,list(legend)))
  dev.off()
  do.call(grid.arrange, c(plots,list(legend)))
}

#95% confidence bound
for (m in 1:length(measurements.names)) {
  plots<-list()
  for (d in 1:(length(results))) {
    print(d)
    results.mean<-apply(results[[d]],c(2,3,4),median,na.rm=TRUE)[,,m]
    results.top<-apply(results[[d]],c(2,3,4),quantile,0.05,na.rm=TRUE)[,,m]
    results.bottom<-apply(results[[d]],c(2,3,4),quantile,0.95,na.rm=TRUE)[,,m]
    
    results.merged<-merge(merge(melt(results.mean),melt(results.top),by=c("X1","X2")),melt(results.bottom),by=c("X1","X2"))
    names(results.merged)<-c("Size","Classifier","Mean","Top","Bottom")
    results.merged<-data.frame(results.merged)
    
    results.merged$Classifier<-factor(results.merged$Classifier,labels=classifiers.names)
    h <- ggplot(results.merged, aes(x=Size,y=Mean,group=Classifier,color=Classifier))
    
    h <- h + geom_line(aes(y=Mean,group=Classifier,color=Classifier))
    h <- h + geom_ribbon(aes(ymin=Bottom, ymax=Top,group=Classifier,color=Classifier,fill=Classifier),colour=NA,alpha=0.5)
    h <- h + scale_x_discrete(breaks = 1:length(sizes), labels=sizes) 
    h <- h + xlab("Number of unlabeled objects")
    h <- h + ggtitle(names(datasets)[d])
    
    tmp <- ggplot_gtable(ggplot_build(h))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    
    h <- h + theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_blank(),
      panel.background = element_blank(),
      legend.position="none"
    )
    h<- h + ylab(measurements.names[m])
    plots[[d]]<-h
  }
  
  
  pdf(paste(outputdir,"LearningCurves - Confidence Bounds - ",n_l," - ",measurements.names[m]," - ",current.time,".pdf",sep=""),paper="a4r",height=8,width=11)
  do.call(grid.arrange, c(plots,list(legend)))
  dev.off()
  do.call(grid.arrange, c(plots,list(legend)))
}

# for (m in length(measurements.names)) {
#   plots<-list()
#   for (d in 1:(length(results))) {
#     results.differenced<-aperm(abind(lapply(2:length(classifiers),function(x){results[[d]][,,x,]-results[[d]][,,1,]}),along=0),c(2,3,1,4))
#     results.mean<-apply(results.differenced,c(2,3,4),mean)[,,m]
#     results.stderror<-apply(results.differenced,c(2,3,4),std.error)[,,m]
#     
#     results.merged<-merge(melt(results.mean),melt(results.stderror),by=c("X1","X2"))
#     names(results.merged)<-c("Size","Classifier","Mean","Std.Error")
#     results.merged<-data.frame(results.merged)
#     
#     results.merged$Classifier<-factor(results.merged$Classifier,labels=classifiers.names[2:length(classifiers.names)])
#     h <- ggplot(results.merged, aes(x=Size,y=Mean,group=Classifier,color=Classifier))
#     
#     
#     h <- h + geom_line(aes(y=Mean,group=Classifier,color=Classifier))
#     h <- h + geom_ribbon(aes(ymin=Mean-Std.Error, ymax=Mean+Std.Error,group=Classifier,color=Classifier,fill=Classifier),colour=NA,alpha=0.5)
#     h <- h + scale_x_discrete(breaks = 1:length(sizes), labels=sizes) 
#     h <- h + xlab("Number of unlabeled objects")
#     h <- h + ggtitle(names(datasets)[d])
#     h <- h + theme( # remove the vertical grid lines
#       panel.grid.major.x = element_blank() ,
#       # explicitly set the horizontal lines (or they will disappear too)
#       panel.grid.major.y = element_blank(),
#       panel.background = element_blank()
#     )
#     h<- h + ylab(measurements.names[m])
#     plots[[d]]<-h
#   }
#   pdf(paste(outputdir,"LearningCurves - Differences - ",measurements.names[m]," - ",current.time,".pdf",sep=""),paper="a4r",height=8,width=11)
#   do.call(grid.arrange,  plots)
#   dev.off()
#   do.call(grid.arrange,  plots)
# }