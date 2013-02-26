library(mlbench)
library(sampling)
library(caret)
library(foreign)
library(parallel)
library(plotrix)

## Settings

# Output
outputdir<-"~/"
current.time<-Sys.time()

# Measurements
classifiers<-c(NearestMeanClassifierXY,MCNearestMeanClassifierXY)
# For measurements, see below
measurements.names<-c("Error","Loss Test","Loss Train")
repeats<-10
n_l<-10
sizes<-2^(1:10)

#results<-data.frame(matrix(NA, nrow = length(sizes)*(length(classifiers.supervised)+length(classifiers.semisupervised)), ncol = 2+length(measurements.names)))
#names(results)<-c("Size","Classifier",measurements.names)

LearningCurve<-function(modelform, data, classifiers, repeats=1000, sampling="empirical") {
  modelform
  classname<-all.vars(modelform)[1]
  #results<-data.frame(Classifier=integer(),Size=integer(),Repeat=integer(),setNames(replicate(length(measurements.names),numeric(0), simplify = F), measurements.names))
  results<-array(NA, dim=c(repeats, length(sizes), length(classifiers), 3))
  
  # Data set to design matrix
  list2env(SSLDataFrameToMatrices(modelform,data,intercept=FALSE),env=environment())
  
  # Generate the test sample
  sample.test<-sample(1:nrow(D_pop), 1000, replace=TRUE)
  X_test<-X[sample.test,]
  y_test<-y[sample.test]
  
  for (i in 1:repeats) {
    print(i) # Print the current repeat
    
    sample.labeled<-strata(D_pop,classname,c(1,1),method="srswr")$ID_unit
    sample.labeled<-c(sample.labeled, sample(1:nrow(D_pop),n_l-2,replace=TRUE))
    X_l<-X[sample.labeled,]
    y_l<-y[sample.labeled]
    
    sample.unlabeled<-sample(1:nrow(D_pop),max(sizes),replace=TRUE)
    X_u<-X[sample.unlabeled,]
    
    for (s in 1:length(sizes)) {
      X_u_s <- X_u[1:sizes[s],]
      for (c in 1:length(classifiers)) {
        trained_classifier<-do.call(classifiers[[c]],list(X_l, y_l, Xu=X_u_s))
        results[i,s,c,1] <- 1-mean(y_test==predict(trained_classifier,X_test))
        results[i,s,c,2] <- loss(trained_classifier,X_test,y_test)
        results[i,s,c,3] <- loss(trained_classifier,X_l,y_l)
        #results[nrow(results)+1,]<-c(c,sizes[s],i,sapply(measurements,do.call,list(trained_classifier)))
        #results[i,s,c,1]<-system.time()[3]
        #results[i,s,c,2:4]<-sapply(measurements,do.call,list(trained_classifier))
      }
    }
  }
  return(results)
}


results<-lapply(names(datasets),function(dname){print(dname); LearningCurve(modelforms[[dname]],datasets[[dname]],classifiers,repeats)})
names(results)<-names(datasets)

# Write Results to disk
save.image(file=paste(outputdir,"LearningCurves - ",current.time,".RData",sep=""))


apply(results$Haberman,c(2,3,4),mean)[,2,1]
apply(results$Haberman,c(2,3,4),std.error)[,2,1]


## Visualize
# Visualize Results
plots1<-list()
plots2<-list()

for (m in length(measurements)) {
  measurement.name<-names(results[[d]])[(3+m)]  
  for (d in length(results)) {
    Res<-merge(cast(results[[d]],"Size+Classifier~.",fun.aggregate=mean,value=measurement.name),cast(results[[d]],"Size+Classifier~.",fun.aggregate=std.error, value=measurement.name),by=c("Size","Classifier"))
    names(Res)[3:4]<-c("Mean","Std")
    h <- ggplot(Res, aes(x=Size,y=Mean,grouping=Classifier))
    h <- h + geom_ribbon(aes(ymin=SLerror-SLerrorSTD, ymax=SLerror+SLerrorSTD),alpha=0.5,fill="red") + geom_line(aes(y=SLerror),color="red")
    h <- h + geom_ribbon(aes(ymin=SSLerror-SSLerrorSTD, ymax=SSLerror+SSLerrorSTD),alpha=0.5,fill="green") + geom_line(aes(y=SSLerror),color="green")
    h <- h + scale_x_discrete(breaks = 1:length(sizes), labels=sizes) 
    h <- h + xlab("Number of unlabeled objects")
    h <- h + ggtitle(dname)
    h <- h + theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_blank(),
      panel.background = element_blank()
    )
    h<-h + ylab("Classification error")
  }
}

do.call(grid.arrange,  plots1)
do.call(grid.arrange,  plots2)