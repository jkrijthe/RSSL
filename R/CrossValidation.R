#' Regular cross-validation on a (set of) classifier(s)
#' 
#' @param X design matrix of the labeled objects
#' @param y vector with labels
#' @param classifiers list; Classifiers to crossvalidate
#' @param measures character; c("predict","losstest") giving the measures to be used
#' @param groups vector; Group assigments to be taken into account in defining the folds
#' @param k integer; Number of folds in the cross-validation
#' @param repeats integer; Number of repeated assignments to folds
#' @param verbose logical; Controls the verbosity of the output
#' 
#' @export
CrossValidation<-function(X,y,classifiers,measures=c("predict","losstest"),groups=NULL,k=10,repeats=1,verbose=FALSE) {
  N<-nrow(X)
  results<-array(NA, dim=c(repeats, length(classifiers), 4))

  ## Repeats
  for (i in 1:repeats) {
    if (verbose) cat("\nRepeat: ",i,"/",repeats," ") # Print the current repeat
    sample.classguarantee <- sample_k_per_level(y,k)
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

#' @title Cross-validation in the transductive setting
#' 
#' @param X design matrix of the labeled objects
#' @param y vector with labels
#' @param classifiers list; Classifiers to crossvalidate
#' @param measures character; c("predict","losstest") giving the measures to be used
#' @param groups vector; Group assigments to be taken into account in defining the folds
#' @param k integer; Number of folds in the cross-validation
#' @param repeats integer; Number of repeated assignments to folds
#' @param verbose logical; Controls the verbosity of the output
#' @param dataset_name character; Name of the dataset
#' 
#' @export
CrossValidationTransductive<-function(X,y,classifiers,measures=c("predict","losstest"),groups=NULL,k=10,repeats=1,verbose=FALSE, dataset_name="Unknown Dataset") {
  N<-nrow(X)
  results<-array(NA, dim=c(repeats, length(classifiers), 4))

  ## Repeats
  for (i in 1:repeats) {
    if (verbose) cat("\nRepeat: ",i,"/",repeats," ") # Print the current repeat
    sample.classguarantee <- sample_k_per_level(y,k)
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

#' @title Semi-supervised cross-validation
#' @details Labeled samples form the fold
#' 
#' @param X Design matrix
#' @param y label vector
#' @param classifiers list with classification functions
#' @param n_labeled number of labeled objects to use
#' @param measures which measures to use
#' @param groups TODO: description or drop
#' @param k number of folds
#' @param repeats number of repeats
#' @param verbose Boolean: verbose output or not
#' @param prop_unlabeled proportion of the non-labeled objects to be used as unlabeled objects vs. test objects
#' 
#' @export
CrossValidationSSL<-function(X,y,classifiers,n_labeled=100,measures=c("predict","losstest"),groups=NULL,k=2,repeats=1,verbose=FALSE,prop_unlabeled=0.5) {

  N<-nrow(X)

  # Check whether there is enough data for the n_labeled and k
  if (k*n_labeled>N) stop("Too many folds or too many labeled samples selected.")

  results<-array(NA, dim=c(repeats*k,length(classifiers), 4))

  ## Repeats
  if (verbose) pb<-txtProgressBar(0,repeats*k)
  for (i in 1:repeats) {

    sample.classguarantee<-sample_k_per_level(y,k)
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
            results[(f*(i-1))+f,c,2] <- mean(loss(trained_classifier, X_test, y_test)) # Average Loss on test set
            results[(f*(i-1))+f,c,3] <- mean(loss(trained_classifier, X_train, y_train)) # Average Loss on training set
            results[(f*(i-1))+f,c,4] <- 0 #Time          
          })
      }

    }

  }
  cat("\n")
  return(list(call="Not known",k=k,results=results))
}

#' Crossvalidation with unlabeled objects
#'
#' The folds are taken over the test sets, not the labeled sets.
#' TODO: include inside the other method
#' 
#' @param X Design matrix
#' @param y label vector
#' @param classifiers list with classification functions
#' @param n_labeled number of labeled objects to use
#' @param groups TODO: unused?
#' @param k number of folds
#' @param repeats number of repeats
#' @param n_min Minimum number of objects per class
#' @param verbose Boolean: verbose output or not
#' @param dataset_name Name of the dataset
#' 
#' @export
CrossValidationSSL2<-function(X,y,classifiers,n_labeled=100,groups=NULL,k=2,repeats=1,n_min=1,verbose=FALSE,dataset_name="Unknown Dataset") {
  N<-nrow(X)
  results<-array(NA, dim=c(repeats, length(classifiers), 4))
  dimnames(results)<-list(1:repeats,names(classifiers),c("Error", "Avg. Loss Test", "Avg. Loss Train","Avg. Loss Trans"))

  if (verbose) cat(dataset_name,"\n")
  if (verbose) cat("Number of features:", ncol(X), "\n")
  if (verbose) cat("Number of objects:", nrow(X), "\n")
  if (verbose) pb<-txtProgressBar(0,repeats*k)
  ## Repeats
  for (i in 1:repeats) {
    results[i,,1] <- 0
    results[i,,2] <- 0 # Loss on test set
    results[i,,3] <- 0 # Loss on training set
    results[i,,4] <- 0 # Loss on training set
    sample.classguarantee<-sample_k_per_level(y, n_min*k)
    sample.random <- sample((1:N)[-sample.classguarantee])
    
    N_fold<-floor(N/k) # Number of objects per fold

    ##Folds
    predictions<-rep(list(rep(NA,N)),length(classifiers))
    for (f in 1:k) {
      if (verbose) setTxtProgressBar(pb, (i-1)*k+f)

      if (f<k) { # Check whether we are in the last fold
        st<-((f-1)*n_min)
        idx_test<-c(sample.classguarantee[c((st+1):(st+n_min),((st+n_min*k)+1):((st+n_min*k)+n_min))], sample.random[1:(N_fold-2*n_min)])
        sample.random<-sample.random[-(1:(N_fold-2*n_min))]
      } else {
        idx_test<-c(sample.classguarantee[c((st+1):(st+n_min),((st+n_min*k)+1):((st+n_min*k)+n_min))], sample.random)
        
      }
      
      idx_train<-(1:N)[-idx_test]
    
      X_train<-X[idx_train,]
      y_train<-y[idx_train]

      sample.labeled.classguarantee<-sample_k_per_level(y_train,n_min)
      sample.labeled.random <- sample((1:nrow(X_train))[-sample.labeled.classguarantee],n_labeled-2*n_min)
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
          
            results[i,c,2] <- results[i,c,2]+sum(loss(trained_classifier, X_test, y_test))/nrow(X) # Average Loss on test set
            results[i,c,3] <- results[i,c,3]+sum(loss(trained_classifier, X_train_labeled, y_train_labeled))/(nrow(X_train_labeled)*k) # Average Loss on training set
            results[i,c,4] <- results[i,c,4]+sum(loss(trained_classifier, X_train, y_train))/(nrow(X_train)*k) # Average Loss in transductive sense  
          })
        }
    }
    
  }
  cat("\n")
  return(list(dataset_name=dataset_name, k=k, n_labeled=n_labeled, results=results))
}

# # Classifier evaluation through resampling
# # 
# # TODO: check whether this function works
# # 
# # @param X matrix; design matrix
# # @param y factor; label vector
# # @param classifiers list; list of classifiers to be evaluated
# # @param prop_train numeric; proportion of objects to be used as training set vs. test set
# # @param repeats integer; number of repeated samplings
# # @param groups factor; vector indicating group membership. Whole groups will be assigned to either the test set of the training set
# # 
# # 
# Resampling<-function(X,y,classifiers,prop_train=0.5,repeats=100,groups=NULL) {
#   N<-nrow(X)
#   results<-array(NA, dim=c(repeats, length(classifiers), 4))
#   for (i in 1:repeats) {
#     results[i,c,2] <- 0 # Loss on test set
#     results[i,c,3] <- 0 # Loss on training set
#     if (verbose) cat("\nRepeat: ",i,"/",repeats," ") # Print the current repeat
#     sample.classguarantee<-strata(data,classname,c(k,k),method="srswor")$ID_unit
#     sample.random <- sample((1:N)[-sample.classguarantee])
# 
#     X_train<-X[idx_train,]
#     y_train<-y[idx_train]
#     X_test<-X[-idx_train,]
#     y_test<-y[-idx_train]
# 
#     for (c in 1:length(classifiers)) {
#       try({     
#         trained_classifier<-do.call(classifiers[[c]],list(X_train, y_train))
#         predictions[[c]][idx_test]<-predict(trained_classifier,X_test)
#         results[i,c,2]           
#       })
#     }
#     for (c in 1:length(classifiers)) {
#       results[i,c,1] <- mean(predictions[[c]]==y[idx_test]) # Classification Accuracy
#       results[i,c,4] <- 0 #Time
#     }
#   }
# }

#' Generate Latex table of cross-validation results
#' 
#' @param object cross-validation results object
#' @param caption character; Caption to be placed over the Latex table
#' @param classifier_names character vector; (Shortened) Names of the classifiers
#' 
#' @export
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
  sapply(1:length(object), function(n) { 
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

# #' Select classifier parameters using cross-validation
# #' 
# #' TODO: evaluate the usefulness of this function
# #' 
# #' @param X Design matrix
# #' @param y label vector
# #' @param classifier Classification function
# #' @param parameters Vector of parameters to try
# #' @param ... parameters to be passed to the classification function
# #' 
# #' @export
# CVSelection<-function(X,y,classifier,parameters,...) {
#   classifiers<-lapply(parameters,function(s){throwaway<<-s; return(function(X,y,X_u,y_u) {classifier(X,y,s=s)}) })
#   cv_results<-CrossValidation(X,y,classifiers,k=2,...)
#   parameters[which.max(apply(cv_results$results[,,1,drop=FALSE],2,function(x){mean(x,na.rm=T)}))]
# }
