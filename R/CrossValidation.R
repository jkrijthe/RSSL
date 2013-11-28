# Error curve class:
# What parameter is changed
# What classifiers
# Names of classifiers
# Plot function
# Different measurements on the data
ErrorCurve<-function(X, y, classifiers, sizes=10:10:nrow(X),n_test=1000,repeats=100,verbose=TRUE) {

  results<-array(NA, dim=c(repeats, length(sizes), length(classifiers), 3))
  dimnames(results)<-list(1:repeats,sizes,lapply(classifiers, function(c) {as.character(body(c))[[2]]}),c("Error", "Loss Test", "Loss Train"))
  sample.test<-sample(1:nrow(X), n_test, replace=TRUE)
  X_test<-X[sample.test,,drop=FALSE]
  y_test<-y[sample.test]
  
  if (verbose) cat("Number of features: ",ncol(X),"\n")
  if (verbose) cat("Number of objects:  ",nrow(X),"\n")
  if (verbose) pb<-txtProgressBar(0,repeats) # Display a text progress bar
      
  for (i in 1:repeats) {
    if (verbose) setTxtProgressBar(pb, i) # Print the current repeat
    
    sample.labeled<-strata(data.frame(strata=y),"strata",c(1,1),method="srswr")$ID_unit
    sample.labeled<-c(sample.labeled, sample(1:nrow(X),max(sizes)-2,replace=TRUE))

    X_l<-X[sample.labeled,,drop=FALSE]
    y_l<-y[sample.labeled]
    
    for (s in 1:length(sizes)) {
      X_l_s <- X_l[1:sizes[s],,drop=FALSE]
      y_l_s <- y_l[1:sizes[s]]
      for (c in 1:length(classifiers)) {
        try({
          trained_classifier<-do.call(classifiers[[c]],list(X_l_s, y_l_s, X_u=NULL,y_u=NULL))        
          results[i,s,c,1] <- 1-mean(y_test==predict(trained_classifier,X_test))
          results[i,s,c,2] <- loss(trained_classifier, X_test, y_test)
          results[i,s,c,3] <- loss(trained_classifier, X_l_s, y_l_s)
        })
        }
    }
  }
  if (verbose) cat("\n")
  return(list(call="Not known",results=results,n_test=n_test,independent="Number of training objects"))
}

ErrorCurveParameters<-function(X,y,s=10^(-5:5)) {

}

ErrorCurveSSL<-function(X, y, classifiers, n_l, sizes=2^(1:8), n_test=1000,repeats=100, verbose=FALSE) {

  results<-array(NA, dim=c(repeats, length(sizes), length(classifiers), 3))
  dimnames(results)<-list(1:repeats,sizes,lapply(classifiers, function(c) {as.character(body(c))[[2]]}),c("Error", "Loss Test", "Loss Train"))
  sample.test<-sample(1:nrow(X), n_test, replace=TRUE)
  X_test<-X[sample.test,,drop=FALSE]
  y_test<-y[sample.test]
  
  if (verbose) cat("Number of features: ",ncol(X),"\n")
  if (verbose) cat("Number of objects:  ",nrow(X),"\n")
  if (verbose) pb<-txtProgressBar(0,repeats) # Display a text progress bar
      
  for (i in 1:repeats) {
    if (verbose) setTxtProgressBar(pb, i) # Print the current repeat
    
    sample.labeled<-strata(data.frame(strata=y),"strata",c(1,1),method="srswr")$ID_unit
    sample.labeled<-c(sample.labeled, sample(1:nrow(X),n_l-2,replace=TRUE))
    X_l<-X[sample.labeled,,drop=FALSE]
    y_l<-y[sample.labeled]
    
    sample.unlabeled<-sample(1:nrow(X),max(sizes),replace=TRUE)
    X_u<-X[sample.unlabeled,,drop=FALSE]
    y_u<-y[sample.unlabeled]
    
    for (s in 1:length(sizes)) {
      X_u_s <- X_u[1:sizes[s],,drop=FALSE]
      y_u_s <- y_u[1:sizes[s]]
      for (c in 1:length(classifiers)) {
        try({
          trained_classifier<-do.call(classifiers[[c]],list(X_l, y_l, X_u=X_u_s,y_u=y_u_s))        
          results[i,s,c,1] <- 1-mean(y_test==predict(trained_classifier,X_test))
          results[i,s,c,2] <- loss(trained_classifier, X_test, y_test)
          results[i,s,c,3] <- loss(trained_classifier, X_l, y_l)
        })
        }
    }
  }
  if (verbose) cat("\n")
  return(list(call="Not known",n_l=n_l,results=results,n_test=n_test,independent="Number of unlabeled objects"))
}

ErrorCurveSSL2<-function(X, y, classifiers, n_l, sizes=2^(1:8), n_test=1000,repeats=100, verbose=FALSE) {

  results<-array(NA, dim=c(repeats, length(sizes), length(classifiers), 3))
  dimnames(results)<-list(1:repeats,sizes,lapply(classifiers, function(c) {as.character(body(c))[[2]]}),c("Error", "Avg. Loss Test", "Avg. Loss Train"))
  
  if (verbose) cat("Number of features: ",ncol(X),"\n")
  if (verbose) cat("Number of objects:  ",nrow(X),"\n")
  if (verbose) pb<-txtProgressBar(0,repeats) # Display a text progress bar
      
  for (i in 1:repeats) {
    if (verbose) setTxtProgressBar(pb, i) # Print the current repeat
    
    sample.labeled<-strata(data.frame(strata=y),"strata",c(1,1),method="srswr")$ID_unit
    sample.labeled<-c(sample.labeled, sample((1:nrow(X))[-sample.labeled],n_l-2,replace=FALSE))

    X_l<-X[sample.labeled,,drop=FALSE]
    y_l<-y[sample.labeled]
    
    sample.unlabeled<-sample((1:nrow(X))[-sample.labeled])
    X_u<-X[sample.unlabeled,,drop=FALSE]
    y_u<-y[sample.unlabeled]
    
    for (s in 1:length(sizes)) {
      if (sizes[s]>nrow(X_u)) {break}

      X_u_s <- X_u[1:sizes[s],,drop=FALSE]
      y_u_s <- y_u[1:sizes[s]]
      X_test <- X_u[-(1:sizes[s]),,drop=FALSE]
      y_test <- y_u[-(1:sizes[s])]

      prX_l<-X_l
      prX_u_s<-X_u_s
      prX_test<-X_test
      #For all preprocessing
#       pca<-prcomp(X_l)
#       # print(rankMatrix(X_l))
#       prX_l<-(predict(pca, X_l))[,1:(rankMatrix(X_l)-1)]
#       prX_u_s<-(predict(pca, X_u_s))[,1:(rankMatrix(X_l)-1)]
#       prX_test<-(predict(pca, X_test))[,1:(rankMatrix(X_l)-1)]
      

      # cat(nrow(X_test),nrow(X_u_s),nrow(X_l),"\n")
      for (c in 1:length(classifiers)) {

        try({
          trained_classifier<-do.call(classifiers[[c]],list(prX_l, y_l, X_u=prX_u_s,y_u=y_u_s))        
          results[i,s,c,1] <- 1-mean(y_test==predict(trained_classifier,prX_test))
          results[i,s,c,2] <- loss(trained_classifier, prX_test, y_test)/nrow(X_test)
          results[i,s,c,3] <- loss(trained_classifier, prX_l, y_l)/nrow(X_l)
        })
        }
    }
  }
  if (verbose) cat("\n")
  return(list(call="Not known",n_l=n_l,results=results,n_test=n_test,independent="Number of unlabeled objects"))
}

plot.ErrorCurve<-function(data,measurement=1,legendsetting="right",datasetname="Unknown Dataset") {
  ## Visualize
  m<-measurement
  results<-data$results[,,,]
  classifiers.names<-dimnames(data$results)[[3]]
  sizes<-dimnames(data$results)[[2]]
    
  results.mean<-apply(results[,,,],c(2,3,4),mean,na.rm=T)[,,m]
  results.stderror<-apply(results[,,,],c(2,3,4),stderror)[,,m]
  
  results.merged<-merge(melt(results.mean),melt(results.stderror),by=c("X1","X2"))
  names(results.merged)<-c("Size","Classifier","Mean","Std.Error")
  results.merged<-data.frame(results.merged)
  
  # results.merged$Classifier<-factor(results.merged$Classifier,labels=classifiers.names)
  h <- ggplot(results.merged, aes(x=factor(Size),y=Mean,group=Classifier,color=Classifier))
  # cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  # h <- h+ scale_colour_manual(values=cbPalette)

  h <- h + geom_line(aes(y=Mean,group=Classifier,color=Classifier))
  h <- h + geom_point(aes(y=Mean,group=Classifier,color=Classifier))
  h <- h + geom_ribbon(aes(ymin=Mean-Std.Error, ymax=Mean+Std.Error,group=Classifier,color=Classifier,fill=Classifier),colour=NA,alpha=0.5)
  h <- h + scale_x_discrete(breaks = sizes, labels=sizes) 
  h <- h + xlab(data$independent)
  h <- h + ggtitle(datasetname)
  
  # tmp <- ggplot_gtable(ggplot_build(h))
  # leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  # legend <- tmp$grobs[[leg]]
  

  h <- h + theme_bw() #eliminates baground, gridlines, and chart border  
  h <- h + theme( 
            plot.background = element_blank() ,
            panel.grid.major = element_blank() ,
            panel.grid.minor = element_blank() ,
            panel.border = element_blank() ,
            panel.background = element_blank(),
            legend.position = legendsetting,
            legend.text = element_text(size=5),
            axis.ticks = element_line(colour = "black"),
            axis.title.x = element_text(size = 10, vjust = 0.5),
            axis.title.y = element_text(size = 10, angle = 90, vjust = 0.5),
            axis.text.x = element_text(size = 7, lineheight = 0.9, colour = "black", vjust = 1),
            axis.text.y = element_text(size = 7, lineheight = 0.9, colour = "black", hjust = 1)
           )

  h <- h + theme(axis.line = element_line(color = 'black'))
  
  h <- h + ylab(dimnames(data$results)[[4]][m])
  return(h)

  # pdf(paste(outputdir,"LearningCurves - ",n_l," - ",measurements.names[m]," - ",current.time,".pdf",sep=""),height=44,width=34)
  # do.call(grid.arrange, c(plots,list(legend),list(ncol=2)))
  # dev.off()
  # do.call(grid.arrange, c(plots,list(legend),list(ncol=2)))
}

createlegend.ErrorCurve<-function(data,measurement=1,legendsetting="right",datasetname="Unknown Dataset") {
  ## Visualize
  m<-measurement
  results<-data$results[,,,]
  classifiers.names<-dimnames(data$results)[[3]]
  sizes<-dimnames(data$results)[[2]]
    
  results.mean<-apply(results[,,,],c(2,3,4),mean)[,,m]
  results.stderror<-apply(results[,,,],c(2,3,4),stderror)[,,m]
  
  results.merged<-merge(melt(results.mean),melt(results.stderror),by=c("X1","X2"))
  names(results.merged)<-c("Size","Classifier","Mean","Std.Error")
  results.merged<-data.frame(results.merged)
  
  # results.merged$Classifier<-factor(results.merged$Classifier,labels=classifiers.names)
  h <- ggplot(results.merged, aes(x=factor(Size),y=Mean,group=Classifier,color=Classifier))
  # cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  # h <- h+ scale_colour_manual(values=cbPalette)

  h <- h + geom_line(aes(y=Mean,group=Classifier,color=Classifier))
  h <- h + geom_point(aes(y=Mean,group=Classifier,color=Classifier))
  h <- h + geom_ribbon(aes(ymin=Mean-Std.Error, ymax=Mean+Std.Error,group=Classifier,color=Classifier,fill=Classifier),colour=NA,alpha=0.5)
  h <- h + scale_x_discrete(breaks = sizes, labels=sizes) 
  h <- h + xlab(data$independent)
  h <- h + ggtitle(datasetname)
  
  tmp <- ggplot_gtable(ggplot_build(h))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  
  h <- h + theme_bw() #eliminates baground, gridlines, and chart border  
  h <- h + theme( 
            plot.background = element_blank() ,
            panel.grid.major = element_blank() ,
            panel.grid.minor = element_blank() ,
            panel.border = element_blank() ,
            panel.background = element_blank(),
            legend.position = legendsetting,
            legend.text = element_text(size=5),
            axis.ticks = element_line(colour = "black"),
            axis.title.x = element_text(size = 10, vjust = 0.5),
            axis.title.y = element_text(size = 10, angle = 90, vjust = 0.5),
            axis.text.x = element_text(size = 7, lineheight = 0.9, colour = "black", vjust = 1),
            axis.text.y = element_text(size = 7, lineheight = 0.9, colour = "black", hjust = 1)
           )

  h <- h + theme(axis.line = element_line(color = 'black'))
  
  h <- h + ylab(dimnames(data$results)[[4]][m])

  return(legend)
}

CrossValidation<-function(X,y,classifiers,measures=c("predict","losstest"),groups=NULL,k=10,repeats=1,verbose=FALSE) {
  N<-nrow(X)
  results<-array(NA, dim=c(repeats, length(classifiers), 4))

  ## Repeats
  for (i in 1:repeats) {
    if (verbose) cat("\nRepeat: ",i,"/",repeats," ") # Print the current repeat
    sample.classguarantee<-strata(data.frame(strata=y),"strata",c(k,k),method="srswor")$ID_unit
    sample.random <- sample((1:N)[-sample.classguarantee])
    
    N_fold<-ceiling(N/k) # Number of objects per fold

    ##Folds
    predictions<-rep(list(rep(NA,N)),length(classifiers))
    for (f in 1:k) {
      if (N_fold<length(sample.random)) { # Check whether we are in the last fold
        idx_train<-c(sample.classguarantee[c(f,f+k)], sample.random[1:N_fold])
        sample.random<-sample.random[-(1:N_fold)]
      } else {
        idx_train<-c(sample.classguarantee[c(f,f+k)], sample.random)
      }
      idx_test<-(1:N)[-idx_train]
    
      X_train<-X[idx_train,]
      y_train<-y[idx_train]
      X_test<-X[-idx_train,]
      y_test<-y[-idx_train]

      for (c in 1:length(classifiers)) {
          try({     
            trained_classifier<-do.call(classifiers[[c]],list(X_train, y_train))
            predictions[[c]][idx_test]<-predict(trained_classifier,X_test)            
          })
        }
    }

    ## Evaluation
    for (c in 1:length(classifiers)) {
      results[i,c,1] <- mean(predictions[[c]]==y) # Classification Accuracy
      results[i,c,2] <- 0 # Loss on test set
      results[i,c,3] <- 0 # Loss on training set
      results[i,c,4] <- 0 #Time
    }
  }

  return(list(call="Not known",k=k,results=results))
}

CrossValidationTransductive<-function(X,y,classifiers,measures=c("predict","losstest"),groups=NULL,k=10,repeats=1,verbose=FALSE, dataset_name="Unknown Dataset") {
  N<-nrow(X)
  results<-array(NA, dim=c(repeats, length(classifiers), 4))

  ## Repeats
  for (i in 1:repeats) {
    if (verbose) cat("\nRepeat: ",i,"/",repeats," ") # Print the current repeat
    sample.classguarantee<-strata(data.frame(strata=y),"strata",c(k,k),method="srswor")$ID_unit
    sample.random <- sample((1:N)[-sample.classguarantee])
    
    N_fold<-ceiling(N/k) # Number of objects per fold

    ##Folds
    predictions<-rep(list(rep(NA,N)),length(classifiers))
    if (verbose) pb<-txtProgressBar(0,k)
    for (f in 1:k) {
      if (verbose) setTxtProgressBar(pb, f)

      if (N_fold<length(sample.random)) { # Check whether we are in the last fold
        idx_train<-c(sample.classguarantee[c(f,f+k)], sample.random[1:N_fold])
        sample.random<-sample.random[-(1:N_fold)]
      } else {
        idx_train<-c(sample.classguarantee[c(f,f+k)], sample.random)
      }
      idx_test<-(1:N)[-idx_train]
    
      X_train<-X[idx_train,]
      y_train<-y[idx_train]
      X_test<-X[-idx_train,]
      y_test<-y[-idx_train]

      for (c in 1:length(classifiers)) {
          try({     
            trained_classifier<-do.call(classifiers[[c]],list(X_train, y_train, X_u=X_test,y_u=y_test))
            predictions[[c]][idx_test]<-predict(trained_classifier,X_test)          
          })
        }
    }

    ## Evaluation for repeat
    for (c in 1:length(classifiers)) {
      results[i,c,1] <- mean(predictions[[c]]==y) # Classification Accuracy
      results[i,c,2] <- 0 # Loss on test set
      results[i,c,3] <- 0 # Loss on training set
      results[i,c,4] <- 0 #Time
    }
  }

  return(list(call="Not known",k=k,results=results))
}

CrossValidationSSL<-function(X,y,classifiers,n_labeled=100,measures=c("predict","losstest"),groups=NULL,k=2,repeats=1,verbose=FALSE,prop_unlabeled=0.5) {

  N<-nrow(X)

  # Check whether there is enough data for the n_labeled and k
  if (k*n_labeled>N) stop("Too many folds or too many labeled samples selected.")

  results<-array(NA, dim=c(repeats*k,length(classifiers), 4))

  ## Repeats
  if (verbose) pb<-txtProgressBar(0,repeats*k)
  for (i in 1:repeats) {

    sample.classguarantee<-strata(data.frame(strata=y),"strata",c(k,k),method="srswor")$ID_unit
    sample.random <- sample((1:N)[-sample.classguarantee])    

    ##Folds
    predictions<-rep(list(rep(NA,N)),length(classifiers))
    
    for (f in 1:k) {
      if (verbose) setTxtProgressBar(pb, (i-1)*k+f)
      
      idx_train<-c(sample.classguarantee[c(f,f+k)], sample.random[(1+(f-1)*(n_labeled-2)):(f*(n_labeled-2))])

      idx_rest<-(1:N)[-idx_train]
      idx2_unlabeled<-sample(length(idx_rest),ceiling(length(idx_rest)*prop_unlabeled))
      idx_unlabeled<-idx_rest[idx2_unlabeled]
      idx_test<-idx_rest[-idx2_unlabeled]
    
      X_train<-X[idx_train,]
      y_train<-y[idx_train]
      X_unlabeled<-X[idx_unlabeled,]
      y_unlabeled<-y[idx_unlabeled]
      X_test<-X[idx_test,]
      y_test<-y[idx_test]

      for (c in 1:length(classifiers)) {
          try({     
            trained_classifier<-do.call(classifiers[[c]],list(X_train, y_train, X_u=X_unlabeled,y_u=y_unlabeled))
            results[(f*(i-1))+f,c,1] <- mean(predict(trained_classifier,X_test)==y_test) # Classification Accuracy
            results[(f*(i-1))+f,c,2] <- loss(trained_classifier, X_test, y_test)/nrow(X_test) # Average Loss on test set
            results[(f*(i-1))+f,c,3] <- loss(trained_classifier, X_train, y_train)/nrow(X_test) # Average Loss on training set
            results[(f*(i-1))+f,c,4] <- 0 #Time          
          })
      }

    }

  }
  cat("\n")
  return(list(call="Not known",k=k,results=results))
}

CrossValidationSSL2<-function(X,y,classifiers,n_labeled=100,groups=NULL,k=2,repeats=1,verbose=FALSE,dataset_name="Unknown Dataset") {
  N<-nrow(X)
  results<-array(NA, dim=c(repeats, length(classifiers), 4))
  dimnames(results)<-list(1:repeats,lapply(classifiers, function(c) {as.character(body(c))[[2]]}),c("Error", "Avg. Loss Test", "Avg. Loss Train","Time"))

  if (verbose) cat(dataset_name,"\n")
  if (verbose) cat("Number of features:", ncol(X), "\n")
  if (verbose) cat("Number of objects:", nrow(X), "\n")
  if (verbose) pb<-txtProgressBar(0,repeats*k)
  ## Repeats
  for (i in 1:repeats) {
    results[i,,1] <- 0
    results[i,,2] <- 0 # Loss on test set
    results[i,,3] <- 0 # Loss on training set
    sample.classguarantee<-strata(data.frame(strata=y),"strata",c(k,k),method="srswor")$ID_unit
    sample.random <- sample((1:N)[-sample.classguarantee])
    
    N_fold<-floor(N/k) # Number of objects per fold

    ##Folds
    predictions<-rep(list(rep(NA,N)),length(classifiers))
    for (f in 1:k) {
      if (verbose) setTxtProgressBar(pb, (i-1)*k+f)

      if (f<k) { # Check whether we are in the last fold
        idx_test<-c(sample.classguarantee[c(f,f+k)], sample.random[1:(N_fold-2)])
        sample.random<-sample.random[-(1:(N_fold-2))]
      } else {
        idx_test<-c(sample.classguarantee[c(f,f+k)], sample.random)
      }
      idx_train<-(1:N)[-idx_test]
    
      X_train<-X[idx_train,]
      y_train<-y[idx_train]

      sample.labeled.classguarantee<-strata(data.frame(strata=y_train),"strata",c(1,1),method="srswor")$ID_unit
      sample.labeled.random <- sample((1:nrow(X_train))[-sample.labeled.classguarantee],n_labeled-2)
      idx_train_labeled<-c(sample.labeled.classguarantee, sample.labeled.random)

      X_train_labeled<-X_train[idx_train_labeled,]
      y_train_labeled<-y_train[idx_train_labeled]

      X_train_unlabeled<-X_train[-idx_train_labeled,]
      y_train_unlabeled<-y_train[-idx_train_labeled]

      X_test<-X[-idx_train,]
      y_test<-y[-idx_train]
      
      for (c in 1:length(classifiers)) {
          try({     
            trained_classifier<-do.call(classifiers[[c]],list(X_train_labeled, y_train_labeled, X_u=X_train_unlabeled,y_u=y_train_unlabeled))
            results[i,c,1]<-results[i,c,1] + sum(predict(trained_classifier,X_test)==y_test)/nrow(X)
          
            results[i,c,2] <- results[i,c,2]+loss(trained_classifier, X_test, y_test)/nrow(X_test) # Average Loss on test set
            results[i,c,3] <- results[i,c,3]+loss(trained_classifier, X_train_labeled, y_train_labeled)/nrow(X_train_labeled) # Average Loss on training set          
          })
        }
    }
    
  }
  cat("\n")
  return(list(dataset_name=dataset_name, k=k, n_labeled=n_labeled, results=results))
}

Resampling<-function(X,y,classifiers,prop_train=0.5,repeats=100,groups=NULL) {
  N<-nrow(X)
  results<-array(NA, dim=c(repeats, length(classifiers), 4))
  for (i in 1:repeats) {
    results[i,c,2] <- 0 # Loss on test set
    results[i,c,3] <- 0 # Loss on training set
    if (verbose) cat("\nRepeat: ",i,"/",repeats," ") # Print the current repeat
    sample.classguarantee<-strata(data,classname,c(k,k),method="srswor")$ID_unit
    sample.random <- sample((1:N)[-sample.classguarantee])

    X_train<-X[idx_train,]
    y_train<-y[idx_train]
    X_test<-X[-idx_train,]
    y_test<-y[-idx_train]

    for (c in 1:length(classifiers)) {
      try({     
        trained_classifier<-do.call(classifiers[[c]],list(X_train, y_train))
        predictions[[c]][idx_test]<-predict(trained_classifier,X_test)
        results[i,c,2]           
      })
    }
    for (c in 1:length(classifiers)) {
      results[i,c,1] <- mean(predictions[[c]]==y[idx_test]) # Classification Accuracy
      results[i,c,4] <- 0 #Time
    }
  }
}

table.CrossValidation<-function(object,caption="",classifier_names=NULL) {
  # overfolds<-apply(object$results,c(1,3:4),mean,na.rm=T)
  if (is.list(object)) {
    if ("results" %in% names(object)) {
      object<-list(object)
    }
  } else {
    stop("Supplied object is not a cross-validation results object")
  }

  if (is.null(classifier_names)) {
    classifier_names<-dimnames(object[[1]]$results)[[2]]
  }

  cat("\\begin{table}\n")
  cat("\\begin{tabular}{l|",paste(rep("l",dim(object[[1]]$results)[2]),collapse=""),"}\n",sep="")
  cat("\\hline\n")
  cat("Dataset &",paste(classifier_names,collapse=" & "),"\\\\ \n")
  sapply(1:length(datasets), function(n) { 
    cat(object[[n]]$dataset_name,"")
    overfolds<-object[[n]]$results
    means<-apply(overfolds,c(2:3),mean,na.rm=T)
    sds<-apply(overfolds,2:3,sd)
    options(digits=2)
  for (c in 1:dim(means)[1]) {
    cat("& $",ifelse(all(means[c]>=means[1])&(c!=5),"\\mathbf{",""),means[c,1]," \\pm ",sds[c,1],ifelse(all(means[c]>=means[1])&(c!=5),"} $","$"),sep="")
  }
  cat("\\\\ \n")
  })
  cat("\\end{tabular}\n")
  cat("\\caption{",caption,"}\n",sep="")
  cat("\\end{table}\n")
}

CVSelection<-function(X,y,classifier,parameters,...) {
  classifiers<-lapply(parameters,function(s){throwaway<<-s; return(function(X,y,X_u,y_u) {classifier(X,y,s=s)}) })
  cv_results<-CrossValidation(X,y,classifiers,k=2,...)
  parameters[which.max(apply(cv_results$results[,,1,drop=FALSE],2,function(x){mean(x,na.rm=T)}))]
}
