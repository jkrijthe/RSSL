#' Print LearningCurve object
#' 
#' @param x LearningCurve object
#' @param ... Not used
#' @method print LearningCurve
#' @export
print.LearningCurve <- function(x,...) {
  cat("Learning Curve object\n\n")
  cat(names(x$results)[[2]],": ",paste(unique(x$results[[2]]),collapse=", "), "\n")
  cat("Classifiers:\n", paste("\t",levels(x$results$Classifier),collapse="\n"), "\n")
  cat("Measures:\n",  paste("\t",levels(x$results$Measure),collapse="\n"), "\n")
  cat(length(unique(x$results$repeats)), " repeats\n")
  cat(sum(is.na(x$results)), " missing\n")
}

#' Compute Semi-Supervised Learning Curve
#' 
#' Evaluate semi-supervised classifiers for different amounts of unlabeled training examples or different fractions of unlabeled vs. labeled examples.
#' 
#' \code{classifiers} is a named list of classifiers, where each classifier should be a function that accepts 4 arguments: a numeric design matrix of the labeled objects, a factor of labels, a numeric design  matrix of unlabeled objects and a factor of labels for the unlabeled objects.
#' 
#' \code{measures} is a named list of performance measures. These are functions that accept seven arguments: a trained classifier, a numeric design matrix of the labeled objects, a factor of labels, a numeric design  matrix of unlabeled objects and a factor of labels for the unlabeled objects, a numeric design matrix of the test objects and a factor of labels of the test objects. See \code{\link{measure_accuracy}} for an example.
#' 
#' This function allows for two different types of learning curves to be generated. If \code{type="unlabeled"}, the number of labeled objects remains fixed at the value of \code{n_l}, where \code{sizes} controls the number of unlabeled objects. \code{n_test} controls the number of objects used for the test set, while all remaining objects are used if \code{with_replacement=FALSE} in which case objects are drawn without replacement from the input dataset. We make sure each class is represented by at least \code{n_min} labeled objects of each class. For \code{n_l}, additional options include: "enough" which takes the max of the number of features and 20, max(ncol(X)+5,20), "d" which takes the number of features or "2d" which takes 2 times the number of features.
#' 
#'  If \code{type="fraction"} the total number of objects remains fixed, while the fraction of labeled objects is changed. \code{frac} sets the fractions of labeled objects that should be considered, while \code{test_fraction} determines the fraction of the total number of objects left out to serve as the test set.
#'  
#' @family RSSL utilities
#' 
#' @param X design matrix
#' @param y vector of labels
#' @param ... arguments passed to underlying function
#' @param classifiers list; Classifiers to crossvalidate
#' @param n_l Number of labeled objects to be used in the experiments (see details)
#' @param with_replacement Indicated whether the subsampling is done with replacement or not (default: FALSE)
#' @param sizes vector with number of unlabeled objects for which to evaluate performance
#' @param n_test Number of test points if with_replacement is TRUE
#' @param repeats Number of learning curves to draw
#' @param n_min Minimum number of labeled objects per class in
#' @param verbose Print progressbar during execution (default: FALSE)
#' @param dataset_name character; Name of the dataset
#' @param type Type of learning curve, either "unlabeled" or "fraction"
#' @param fracs list; fractions of labeled data to use
#' @param test_fraction numeric; If not NULL a fraction of the object will be left out to serve as the test set
#' @param pre_scale logical; Whether the features should be scaled before the dataset is used
#' @param pre_pca logical; Whether the features should be preprocessed using a PCA step
#' @param measures named list of functions giving the measures to be used
#' @param time logical; Whether execution time should be saved.
#' @param low_level_cores integer; Number of cores to use compute repeats of the learning curve
#' @return LearningCurve object
#' 
#' @examples
#' set.seed(1)
#' df <- generate2ClassGaussian(2000,d=2,var=0.6)
#' 
#' classifiers <- list("LS"=function(X,y,X_u,y_u) {
#'  LeastSquaresClassifier(X,y,lambda=0)}, 
#'   "Self"=function(X,y,X_u,y_u) {
#'     SelfLearning(X,y,X_u,LeastSquaresClassifier)}
#' )
#' 
#' measures <- list("Accuracy" =  measure_accuracy,
#'                  "Loss Test" = measure_losstest,
#'                  "Loss labeled" = measure_losslab,
#'                  "Loss Lab+Unlab" = measure_losstrain
#' )
#' 
#' # These take a couple of seconds to run
#' \dontrun{
#' # Increase the number of unlabeled objects
#' lc1 <- LearningCurveSSL(as.matrix(df[,1:2]),df$Class,
#'                         classifiers=classifiers,
#'                         measures=measures, n_test=1800,
#'                         n_l=10,repeats=3)
#' 
#' plot(lc1)
#' 
#' # Increase the fraction of labeled objects, example with 2 datasets
#' lc2 <- LearningCurveSSL(X=list("Dataset 1"=as.matrix(df[,1:2]),
#'                                "Dataset 2"=as.matrix(df[,1:2])),
#'                         y=list("Dataset 1"=df$Class,
#'                                "Dataset 2"=df$Class),
#'                         classifiers=classifiers,
#'                         measures=measures,
#'                         type = "fraction",repeats=3,
#'                         test_fraction=0.9)
#' 
#' plot(lc2)
#' }
#' @export
LearningCurveSSL<-function(X, y, ...) {
  UseMethod("LearningCurveSSL")
}

#' @export
LearningCurveSSL.list<-function(X, y, ..., verbose=FALSE, mc.cores=1) {

  if (is.matrix(X[[1]]) & is.factor(y[[1]])) {
    curves <- clapply(names(X),function(dname){
      if (verbose) cat(dname,"\n");
      
      Xd <- X[[dname]]
      Xd <- Xd[,colnames(Xd)!="(Intercept)"]
      Xd <- Xd[,apply(Xd, 2, var, na.rm=TRUE) != 0] # Remove constant columns
      yd <- y[[dname]]
      
      LearningCurveSSL(Xd,yd,...,verbose=verbose)
    },mc.cores=mc.cores)
  } else if (is(X[[1]],"formula") & is.data.frame(y[[1]])) { 
    curves <- clapply(names(X),function(dname){
      if (verbose) cat(dname,"\n");
      data <- data.frame(y[[dname]]) 
      classname <- all.vars(X[[dname]])[1]
      
      Xd <- model.matrix(X[[dname]],y[[dname]])
      Xd <- Xd[,colnames(Xd)!="(Intercept)"]
      Xd <- Xd[,apply(Xd, 2, var, na.rm=TRUE) != 0] # Remove constant columns
      yd <- data[,classname]
      
      LearningCurveSSL(Xd,yd,...,verbose=verbose)
    },mc.cores=mc.cores)
  } else {
    stop("Unknown input. Should be either a list of matrices and label vectors or formulae and data frames.")
  }
  names(curves) <- names(X)
  results <- dplyr::bind_rows(lapply(names(curves),function(x) {dplyr::mutate(curves[[x]]$results,Dataset=x)}))
  object<-list(n_l=curves[[1]]$n_l,
               results=results,
               n_test=curves[[1]]$n_test)
  class(object)<-"LearningCurve"
  return(object)
}

#' @rdname LearningCurveSSL
#' @export
LearningCurveSSL.matrix<-function(X, y, classifiers, measures=list("Accuracy"=measure_accuracy), type="unlabeled", n_l=NULL, with_replacement=FALSE, sizes=2^(1:8), n_test=1000,repeats=100, verbose=FALSE,n_min=1,dataset_name=NULL,test_fraction=NULL,fracs=seq(0.1,0.9,0.1),time=TRUE,pre_scale=FALSE, pre_pca=FALSE,low_level_cores=1,...) {
  
  if (!is.factor(y)) { stop("Labels are not a factor.") }
  if (nrow(X)!=length(y)) { stop("Number of objects in X and y do not match.") }
  K <- length(levels(y))
  
  # Pre-processing
  if (pre_scale) X <- scale(X) # Pre-scale data
  
  if (pre_pca) {
    t_pca <- princomp(X)
    n_comp <- sum(cumsum(t_pca$sdev^2)/sum(t_pca$sdev^2)<0.99)+1
    n_comp <- n_comp #min(c(n_comp,floor(n_labeled/2)))
    X <- t_pca$scores[,1:n_comp,drop=FALSE]
  }
  
  if (type=="unlabeled") {
    if (is.null(n_l)) stop("Set the number of labeled objects n_l")
    
    if (n_l=="enough") { n_l <- max(ncol(X)+5,20) }
    else if (n_l=="d") { n_l <- ncol(X)+1 }
    else if (n_l=="2d") { n_l <- max(ncol(X)*2,10) }
    else if (n_l=="half") { n_l <- ceiling(ncol(X)/2) }
    else {n_l <- n_l}
  }
  
  # Set variables
  
  if (type=="fraction") { sizes <- fracs }
  
  results<-array(NA, dim=c(repeats, length(sizes), length(classifiers), length(measures)+time))
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
                    "independent"=sizes,
                    "Classifier"=classifier_names,
                    "Measure"=measure_names
  )
  if (type=="fraction") {
    names(name_list)[[2]] <- "Fraction of labeled objects"
  } else {
    names(name_list)[[2]] <- "Number of unlabeled objects"
  }
  dimnames(results)<- name_list
  
  if (verbose) cat("Number of features: ", ncol(X),"\n")
  if (verbose) cat("Number of objects:  ", nrow(X),"\n")
  if (verbose) pb <- txtProgressBar(0,repeats) # Display a text progress bar
  
  if (type=="unlabeled") {
    results <- clapply(1:repeats,function(i) {
      results <- results[1,,,,drop=FALSE]
      
      if (verbose) setTxtProgressBar(pb, i) # Print the current repeat
      
      sample.labeled <- sample_k_per_level(y,n_min)
      sample.labeled <- c(sample.labeled, sample((1:nrow(X))[-sample.labeled],n_l-(K*n_min),replace=FALSE))
    
      X_l <- X[sample.labeled,,drop=FALSE]
      y_l <- y[sample.labeled]
      
      if (!with_replacement) {
        sample.unlabeled <- sample((1:nrow(X))[-sample.labeled])
      } else {
        sample.unlabeled <- sample(1:nrow(X),max(sizes)+n_test,replace=TRUE)
      }
      X_u <- X[sample.unlabeled,,drop=FALSE]
      y_u <- y[sample.unlabeled]
      
      for (s in 1:length(sizes)) {
        if (sizes[s]>nrow(X_u)) {break}
        
        X_u_s <- X_u[1:sizes[s],,drop=FALSE]
        y_u_s <- y_u[1:sizes[s]]
        if (!with_replacement) {
          X_test <- X_u[-(1:sizes[s]),,drop=FALSE]
          y_test <- y_u[-(1:sizes[s])]
        } else {
          X_test <- X_u[-(1:max(sizes)),,drop=FALSE]
          y_test <- y_u[-(1:max(sizes))]
        }
        
        for (c in 1:length(classifiers)) {
          if (time) timed <- proc.time()
            trained_classifier<-do.call(classifiers[[c]],
                                      list(X=X_l, y=y_l, X_u=X_u_s, y_u=y_u_s))
          if (time) {
            timed <- proc.time()-timed
            results[1,s,c,length(measures)+1] <- timed[[3]]  
          }
          for (m in 1:length(measures)) {
            results[1,s,c,m] <- do.call(measures[[m]],
                                        list(trained_classifier=trained_classifier,
                                             X=X_l, 
                                             y=y_l, 
                                             X_u=X_u_s, 
                                             y_u=y_u_s,
                                             X_test=X_test,
                                             y_test=y_test))
          }
        }
      }
      dimnames(results)$repeats <- i
      return(reshape2::melt(results))
    }, mc.cores=low_level_cores)
    results <- dplyr::bind_rows(results)
  } else if (type=="fraction") {
    results <- clapply(1:repeats,function(i) {
      results <- results[1,,,,drop=FALSE]
      
    sample.guaranteed <- sample_k_per_level(y,n_min)
    if (!is.null(test_fraction)) { 
      idx_test <- sample((1:nrow(X))[-sample.guaranteed], size=ceiling(nrow(X)*test_fraction))
      sampleorder <- c(sample.guaranteed,sample((1:nrow(X))[-c(sample.guaranteed,idx_test)]))
    } else {
      sampleorder <- c(sample.guaranteed,sample((1:nrow(X))[-c(sample.guaranteed)]))
    }
    
    for (s in 1:length(fracs)) {
      idx_lab <- sampleorder[1:ceiling(length(sampleorder)*fracs[s])]
      
      X_l <- X[idx_lab,,drop=FALSE]
      y_l <- y[idx_lab]
      if (!is.null(test_fraction)) {
        # Separate test set
        X_u <- X[-c(idx_lab,idx_test),,drop=FALSE]
        y_u <- y[-c(idx_lab,idx_test)]
        
        X_test <- X[idx_test,,drop=FALSE]
        y_test <- y[idx_test]
      } else {
        # Test on unlabeled data
        X_u <- X[-c(idx_lab),,drop=FALSE]
        y_u <- y[-c(idx_lab)]
        
        X_test <- X_u
        y_test <- y_u
      }
      
      for (c in 1:length(classifiers)) {
        if (time) timed <- proc.time()
        trained_classifier<-do.call(classifiers[[c]],
                                    list(X=X_l, y=y_l, X_u=X_u, y_u=y_u))
        if (time) {
          timed <- proc.time()-timed
          results[1,s,c,length(measures)+1] <- timed[[3]]  
        }
        
        for (m in 1:length(measures)) {
          results[1,s,c,m] <- do.call(measures[[m]],
                                      list(trained_classifier=trained_classifier,
                                           X=X_l, y=y_l, 
                                           X_u=X_u, y_u=y_u,
                                           X_test=X_test,y_test=y_test))
        }
      }
    }
    dimnames(results)$repeats <- i
    return(reshape2::melt(results))
    }, mc.cores=low_level_cores)
    results <- dplyr::bind_rows(results)
  } else {
    stop("Unknown value for argument 'type'")
  }
  if (verbose) cat("\n")
  
  object<-list(n_l=n_l,
               results=results,
               n_test=n_test)
  class(object)<-"LearningCurve"
  return(object)
}

#' Plot LearningCurve object
#' 
#' @param x LearningCurve object
#' @param y Not used
#' @param ... Not used
#' @method plot LearningCurve
#' @export
plot.LearningCurve <- function(x, y, ...) {
  
  data <- x
  
  # Check for input object
  if (class(data)=="LearningCurve") { 
    data <- list(data)
  } else if (!(is.list(data) & all(lapply(data,class)=="LearningCurve"))) {
    stop("Input object should be LearningCurve of list of LearningCurve objects.")
  }
  
  # Extract metadata from the first experiment
  #m<-measurement
  x_label <- names(x$results)[[2]]
  #y_label <- dimnames(data[[1]]$results)[[4]][m]
  
  # Generate the dataset for plotting
  if ("Dataset" %in% names(x$results)) {
    
    plot_frame <-  x$results %>% 
      dplyr::group_by(!!dplyr::sym(x_label), .data$Classifier, .data$Measure, .data$Dataset) %>%
      summarize(Mean=mean(.data$value,na.rm=TRUE),SE=stderror(.data$value)) %>%
      ungroup()
    facet_used <- facet_wrap(~ Dataset + Measure,scales="free",ncol=length(unique(plot_frame$Measure)))
  } else {
    plot_frame <-  x$results %>% 
      dplyr::group_by(!!dplyr::sym(x_label), .data$Classifier, .data$Measure) %>%
      dplyr::summarize(Mean=mean(.data$value,na.rm=TRUE),SE=stderror(.data$value)) %>% 
      dplyr::ungroup()
    facet_used <- facet_wrap(~Measure,scales="free")
  }
  
  p <- plot_frame %>% 
    ggplot(aes_string(x=paste0("`",x_label,"`"),y="Mean",color="Classifier",shape="Classifier")) +
    geom_point(size=1, na.rm=TRUE) +
    geom_line(aes_string(linetype="Classifier"), na.rm=TRUE) +
    geom_ribbon(aes_string(ymax="Mean+1*SE",ymin="Mean-1*SE",fill="Classifier"),size=0,alpha=0.3, na.rm=TRUE) +
    #geom_errorbar(aes(ymax=Mean+2*SE,ymin=Mean-2*SE,fill=Classifier),width=0.1) +
    theme_classic() +
    facet_used +
    ylab("") +
    theme(legend.position="bottom",
          strip.background=element_rect(size = 0),
          axis.title.y=element_text(angle = 0,size=rel(0.8)),
          axis.title.x=element_text(size=rel(0.8)),
          axis.text.y=element_text(size=rel(0.8)),
          axis.text.x=element_text(size=rel(0.8)))
  if (x_label=="`Fraction of labeled objects`") {
    p <- p + scale_x_continuous()
  } else {
    p <- p + scale_x_continuous(trans = scales::log2_trans())
  }
  
  return(p)
}

#' Sample k indices per levels from a factor 
#' @param y factor; factor with levels
#' @param k integer; number of indices to sample per level
#' @return vector with indices for sample
#' @export
sample_k_per_level <- function(y,k) {
  stopifnot(is.factor(y))
  stopifnot(k>0)
  
  all_idx <- 1:length(y)
  sample_idx <- c()
  for (i in levels(y)) {
    sample_idx <- c(sample_idx,sample(all_idx[y==i],k))
  }
  return(sample_idx)
}
