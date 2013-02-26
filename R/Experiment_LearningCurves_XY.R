## Settings

# Output
outputdir<-"~/"
current.time<-Sys.time()

# Measurements
classifiers.supervised<-c(LeastSquaresClassifierXY)
classifiers.semisupervised<-c(ICLeastSquaresClassifierXY)
# For measurements, see below
measurements.names<-c("Error","Loss Test","Loss Train")
repeats<-10
n_l<-10
sizes<-2^(1:10)

results<-data.frame(matrix(NA, nrow = length(sizes)*length(classifiers), ncol = 2+length(measurements)))
names(results)<-c("Size","Classifier",measurements.names)


LearningCurve<-function(dataset, modelform, sampling="empirical", repeats) {
  mf <- model.frame(formula=modelform, data=dataset)
  X <- model.matrix(attr(mf, "terms"), data=mf)
  y <- model.response(mf)
  
  classname<-all.vars(modelform)[1]
  
  results<-data.frame(Classifier=integer(),Size=integer(),Repeat=integer(),setNames(replicate(length(measurements),numeric(0), simplify = F), measurements.names))
  
  for (i in 1:repeats) {
    print(i) # Print the current repeat
    
    if (sampling=="empirical") {
      # Sample a large test set
      D_test<-D_pop[sample(1:nrow(D_pop), 1000, replace=TRUE),]
      
      # Sample labeled points
      i_l<-strata(D_pop,classname,c(1,1),method="srswr")$ID_unit
      i_l<-c(i_l, sample(1:nrow(D_pop),n_l-2,replace=TRUE)) # Take a minimum of 1 object from each class
      D_l <- D_pop[i_l,]
      
      # Sample enough unlabeled points
      i_u<-sample(1:nrow(D_pop),max(sizes),replace=TRUE)
      D_u_all <- D_pop[i_u,]
      D_u_all[,classname]<-rep(NA, nrow(D_u_all)) # Remove class names
    }
    
    for (s in 1:length(sizes)) {
      D_u <- D_u_all[1:sizes[s],]
      D_train<-rbind(D_l,D_u)
      
      for (c in 1:length(classifiers)) {
        trained_classifier<-do.call(classifiers[[c]],list(modelform, D_train))
        
        measurements<-c(function(classifier){1-mean(D_test[,classname]==predict(classifier,D_test))},
                        function(classifier){loss(classifier,D_test)},
                        function(classifier){loss(classifier,D_l)}
        )
        
        results[nrow(results)+1,]<-c(c,sizes[s],i,sapply(measurements,do.call,list(trained_classifier)))
      }
      
    }
  }
  return(results)
}

results<-lapply(names(datasets),function(dname){print(dname); LearningCurve(datasets[[dname]],modelforms[[dname]])})
names(results)<-names(datasets)

# Write Results to disk
save.image(file=paste(outputdir,"LearningCurves - ",current.time,".RData",sep=""))

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

}

do.call(grid.arrange,  plots1)
do.call(grid.arrange,  plots2)