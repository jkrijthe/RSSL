#' Summary of Crossvalidation results
#'
#' @param object CrossValidation object
#' @param measure Measure of interest
#' @param ... Not used
#' @method summary CrossValidation
#' @export
summary.CrossValidation <- function(object, measure=NULL,...) {
  results <- object$results
  if (!is.null(measure)) {
    results <- results %>% filter_(quote(Measure) %in% measure) 
  }
  if ("Dataset" %in% names(object$results)) {
    results %>% 
      group_by_("Measure","Classifier","Dataset") %>% 
      summarize(Value=quote(mean(value))) %>% 
      ungroup %>%
      group_by_("Measure")
  } else {
    results %>% 
      group_by_("Measure","Classifier") %>% 
      summarize(Value=quote(mean(value))) %>% 
      ungroup %>% 
      tidyr::spread_("Measure","Value")
  }
}

#' Print CrossValidation object
#' 
#' @param x CrossValidation object
#' @param ... Not used
#' @method print CrossValidation
#' @export
print.CrossValidation <- function(x,...) {
  cat("CrossValidation object\n\n")
  cat("Classifiers:\n", paste("\t",levels(x$results$Classifier),collapse="\n"), "\n")
  cat("Measures:\n",  paste("\t",levels(x$results$Measure),collapse="\n"), "\n")
  cat(length(unique(x$results$fold)), " folds\n")
  cat(length(unique(x$results$repeats)), " repeats\n")
  cat(sum(is.na(x$results)), " missing\n")
}

#' Merge result of cross-validation runs on single datasets into a the same object
#' @param ... Named arguments for the different objects, where the name reflects the dataset name
#' @export
c.CrossValidation <- function(...) {
  obs <- eval(substitute(alist(...)))
  results <- dplyr::bind_rows(lapply(names(obs), function(x) {dplyr::mutate(obs[[x]]$results,Dataset=x)}))
  object<-list(results=results)
  class(object) <- "CrossValidation"
  return(object)
}

#' Cross-validation in semi-supervised setting
#' 
#' Cross-validation for semi-supervised learning, in which the dataset is split in three parts: labeled training object, unlabeled training object and validation objects. This can be used to evaluate different approaches to semi-supervised classification under the assumption the labels are missing at random. Different cross-validation schemes are implemented. See below for details.
#' 
#' @details
#' The input to this function can be either: a dataset in the form of a feature matrix and factor containing the labels, a dataset in the form of a formula and data.frame or a named list of these two options.
#' There are two main modes in which the cross-validation can be carried out, controlled by the \code{leaveout} parameter. 
#' When leaveout is "labeled", the folds are formed by non-overlapping labeled training sets of a user specified size. 
#' Each of these folds is used as a labeled set, while the rest of the objects are split into the an unlabeled and the test set, controlled by \code{prop_unlabeled} parameter. Note that objects can be used multiple times for testing, when training on a different fold, while other objects may never used for testing.
#' 
#' The "test" option of \code{leaveout}, on the other hand, uses the folds as the test sets. This means every object will be used as a test object exactly once. The remaining objects in each training iteration are split randomly into a labeled and an unlabeled part, where the number of the labeled objects is controlled by the user through the n_labeled parameter.
#' @param X design matrix of the labeled objects
#' @param y vector with labels
#' @param PU logical; Wether is a possitive unlabeled problem or not
#' @param positive_case Character that says which is the positive case
#' @param mc.cores integer; Number of cores to be used
#' @param ... arguments passed to underlying functions
#' 
#' @examples
#' X <- model.matrix(Species~.-1,data=iris)
#' y <- iris$Species
#' 
#' classifiers <- list("LS"=function(X,y,X_u,y_u) {
#'   LeastSquaresClassifier(X,y,lambda=0)}, 
#'   "EM"=function(X,y,X_u,y_u) {
#'     SelfLearning(X,y,X_u,
#'                  method=LeastSquaresClassifier)}
#' )
#' 
#' measures <- list("Accuracy" =  measure_accuracy,
#'                  "Loss" = measure_losstest,
#'                  "Loss labeled" = measure_losslab,
#'                  "Loss Lab+Unlab" = measure_losstrain
#' )
#' 
#' # Cross-validation making sure test folds are non-overlapping
#' cvresults1 <- CrossValidationSSL(X,y, 
#'                                  classifiers=classifiers, 
#'                                  measures=measures,
#'                                  leaveout="test", k=10,
#'                                  repeats = 2,n_labeled = 10)
#' print(cvresults1)
#' plot(cvresults1)
#' 
#' # Cross-validation making sure labeled sets are non-overlapping
#' cvresults2 <- CrossValidationSSL(X,y, 
#'                                  classifiers=classifiers, 
#'                                  measures=measures,
#'                                  leaveout="labeled", k=10,
#'                                  repeats = 2,n_labeled = 10,
#'                                  prop_unlabeled=0.5)
#' print(cvresults2)
#' plot(cvresults2)
#' 
#' @export
CrossValidationSSL <- function(X, y,...) {
  UseMethod("CrossValidationSSL")
}

#' @rdname CrossValidationSSL
#' @export
CrossValidationSSL.list <- function(X,y, ...,verbose=FALSE, PU=FALSE, positive_case=NULL, mc.cores=1) {
  if (is.matrix(X[[1]]) & is.factor(y[[1]])) {
    curves <- clapply(names(X),function(dname){
      if (verbose) cat(dname,"\n");
      
      Xd <- X[[dname]]
      Xd <- Xd[,colnames(Xd)!="(Intercept)"]
      Xd <- Xd[,apply(Xd, 2, var, na.rm=TRUE) != 0] # Remove constant columns
      yd <- y[[dname]]
      if (PU & is.null(positive_case)) positive_case_d <- positive_case[[dname]]
      
      CrossValidationSSL(X=Xd,y=yd,...,verbose=verbose)
    }, mc.cores=mc.cores)
  } else if (is(X[[1]],"formula") & is.data.frame(y[[1]])) { 
    curves <- clapply(names(X),function(dname){
      if (verbose) cat(dname,"\n");
      data <- data.frame(y[[dname]]) 
      classname <- all.vars(X[[dname]])[1]
      
      Xd <- model.matrix(X[[dname]],y[[dname]])
      Xd <- Xd[,colnames(Xd)!="(Intercept)"]
      Xd <- Xd[,apply(Xd, 2, var, na.rm=TRUE) != 0] # Remove constant columns
      yd <- data[,classname]
      if (PU & is.null(positive_case)) positive_case_d <- positive_case[[dname]]
      
      CrossValidationSSL(X=Xd,y=yd,...,verbose=verbose,positive_case=positive_case_d)
    },mc.cores=mc.cores)
  } else {
    stop("Unknown input. Should be either a named list of matrices and label vectors or a named list of formulae and data frames.")
  }
  names(curves) <- names(X)
  return(do.call(c,curves))
}

#' @rdname CrossValidationSSL
#' @param classifiers list; Classifiers to crossvalidate
#' @param measures named list of functions giving the measures to be used
#' @param k integer; Number of folds in the cross-validation
#' @param repeats integer; Number of repeated assignments to folds
#' @param verbose logical; Controls the verbosity of the output
#' @param leaveout either "labeled" or "test", see details
#' @param n_labeled Number of labeled examples, used in both leavout modes
#' @param pre_scale logical; Whether the features should be scaled before the dataset is used
#' @param pre_pca logical; Whether the features should be preprocessed using a PCA step
#' @param n_min integer; Minimum number of labeled objects per class
#' @param prop_unlabeled numeric; proportion of unlabeled objects
#' @param time logical; Whether execution time should be saved.
#' @param low_level_cores integer; Number of cores to use compute repeats of the learning curve
#' @param PU logical; Wether is a possitive unlabeled problem or not
#' @param positive_case Character that says which is the positive case
#' @export
CrossValidationSSL.matrix <- function(X, y, classifiers, measures=list("Error"=measure_error), k=10, repeats=1, verbose=FALSE, leaveout="test", n_labeled=10, prop_unlabeled=0.5,time=TRUE,pre_scale=FALSE,pre_pca=FALSE,n_min=1,low_level_cores=1,PU=FALSE, positive_case=NULL,...) {
  N<-nrow(X)
  
  if (!is.factor(y)) { stop("Labels are not a factor.") }
  if (nrow(X)!=length(y)) { stop("Number of objects in X and y do not match.") }
  K <- length(levels(y))
  
  # Pre-processing
  if (pre_scale) X <- scale(X) # Pre-scale data
  
  if (pre_pca) {
    t_pca <- princomp(X)
    n_comp <- sum(cumsum(t_pca$sdev^2)/sum(t_pca$sdev^2)<0.99)
    n_comp <- n_comp+1 #min(c(n_comp,floor(n_labeled/2)))
    X <- t_pca$scores[,1:n_comp,drop=FALSE]
  }
  
  if (n_labeled=="enough") { n_labeled <- max(ncol(X)+5,20) }
  else if (n_labeled=="d") { n_labeled <- ncol(X)+1 }
  else if (n_labeled=="2d") { n_labeled <- ncol(X)*2 }
  else {n_labeled<-n_labeled}
  
  # Set variables
  results<-array(NA, dim=c(repeats, k, length(classifiers), length(measures)+time))
  if (is.null(names(classifiers))) {
    classifier_names <- lapply(classifiers, function(c) {as.character(body(c))[[2]]})
  } else {
    classifier_names <- names(classifiers) 
  }
  
  if (is.null(names(measures))) {
    measure_names <- lapply(measures, function(c) {as.character(body(c))[[2]]})
  } else {
    measure_names <- names(measures) 
  }
  
  if (time) { measure_names<-c(measure_names,"Time")}
  name_list <- list("repeats"=1:repeats,
                    "fold"=1:k,
                    "Classifier"=classifier_names,
                    "Measure"=measure_names
  )
  dimnames(results)<- name_list
  
  # Check whether there is enough data for the n_labeled and k
  if (leaveout=="labeled" & k*n_labeled>N) stop("Too many folds or too many labeled samples selected.")
  
  
  ## Repeats
  if (verbose) pb<-txtProgressBar(0,repeats*k)
  results <- clapply(1:repeats,function(i) {
    results <- results[1,,,,drop=FALSE]
    
    if (!PU) {
        sample.classguarantee <- sample_k_per_level(y,k)
      } else{
        sample.classguarantee <- sample_k_positive(y,K*k,positive_case)
      }

    sample.random <- sample((1:N)[-sample.classguarantee])    
    
    
    ##Folds
    N_fold <- floor(N/k)
    
    for (f in 1:k) {
      if (verbose) setTxtProgressBar(pb, (i-1)*k+f)
      if (leaveout=="labeled") {
        idx_train_labeled<-c(sample.classguarantee[c(f,f+k)], 
                             sample.random[(1+(f-1)*(n_labeled-2)):(f*(n_labeled-2))])
        
        idx_rest<-(1:N)[-idx_train_labeled]
        idx2_unlabeled<-sample(length(idx_rest),
                               ceiling(length(idx_rest)*prop_unlabeled))
        idx_train_unlabeled<-idx_rest[idx2_unlabeled]
        idx_test<-idx_rest[-idx2_unlabeled]
        
        X_labeled<-X[idx_train_labeled,]
        y_labeled<-y[idx_train_labeled]
        X_unlabeled<-X[idx_train_unlabeled,]
        y_unlabeled<-y[idx_train_unlabeled]
        X_test<-X[idx_test,]
        y_test<-y[idx_test]
      }
      else if (leaveout=="test") {
          if (f<k) { # Check whether we are in the last fold
            st<-((f-1)*n_min)
            idx_test<-c(sample.classguarantee[c((st+1):(st+n_min),((st+n_min*k)+1):((st+n_min*k)+n_min))], 
                        sample.random[1:(N_fold-2*n_min)])
            sample.random<-sample.random[-(1:(N_fold-2*n_min))]
          } else {
            idx_test<-c(sample.classguarantee[c((st+1):(st+n_min),((st+n_min*k)+1):((st+n_min*k)+n_min))], 
                        sample.random)
          }
          idx_train<-(1:N)[-idx_test]
          
          X_train<-X[idx_train,,drop=FALSE]
          y_train<-y[idx_train]
          
          sample.labeled.classguarantee<-sample_k_per_level(y_train,n_min)
          sample.labeled.random <- sample((1:nrow(X_train))[-sample.labeled.classguarantee],n_labeled-2*n_min)
          idx_train_labeled<-c(sample.labeled.classguarantee, sample.labeled.random)
          
          X_labeled <- X_train[idx_train_labeled,,drop=FALSE]
          y_labeled <- y_train[idx_train_labeled]
          
          X_unlabeled <- X_train[-idx_train_labeled,,drop=FALSE]
          y_unlabeled <- y_train[-idx_train_labeled]
          
          X_test<-X[-idx_train,,drop=FALSE]
          y_test<-y[-idx_train]
      }
      
      
      # Train classifiers
      for (c in 1:length(classifiers)) {
        if (time) timed <- proc.time()
        trained_classifier<-do.call(classifiers[[c]],
                                    list(X=X_labeled, y=y_labeled, X_u=X_unlabeled, y_u=y_unlabeled))
        if (time) {
          timed <- proc.time()-timed
          results[1,f,c,length(measures)+1] <- timed[[3]]  
        }
        for (m in 1:length(measures)) {
          results[1,f,c,m] <- do.call(measures[[m]],
                                      list(trained_classifier=trained_classifier,
                                           X=X_labeled, 
                                           y=y_labeled, 
                                           X_u=X_unlabeled, 
                                           y_u=y_unlabeled,
                                           X_test=X_test,
                                           y_test=y_test))
        }
      }
  }
    dimnames(results)$repeats <- i
    return(reshape2::melt(results))
  }, mc.cores=low_level_cores)
  results <- dplyr::bind_rows(results)
  object <- list()
  object$results <- results
  class(object) <- "CrossValidation"
  return(object)
}

#' Plot CrossValidation object
#' 
#' @param x CrossValidation object
#' @param y Not used
#' @param ... Not used
#' @method plot CrossValidation
#' @export
plot.CrossValidation <-function(x,y,...) {
  if ("Dataset" %in% names(x$results)) {
    x$results %>% 
      ggplot(aes_string(x="Classifier",y="value",color="Classifier")) + 
      geom_boxplot() +
      facet_wrap(~Measure+Dataset,scales="free") +
      theme(legend.position="bottom") +
      scale_color_discrete(name="Repeat")
  } else {
    x$results %>% 
    ggplot(aes_string(x="Classifier",y="value",color="factor(repeats)")) + 
      geom_boxplot() +
      facet_wrap(~Measure,scales="free") +
      theme(legend.position="bottom") +
      scale_color_discrete(name="Repeat")
  }
}
