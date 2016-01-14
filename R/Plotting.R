coefficients_after_scaling <- function(w0,w,scaling) {
  if (is.null(scaling)) {
    list(intercept = as.numeric(-w0/w[2]), 
         slope = as.numeric(-w[1]/w[2]))
  } else {
    m <- scaling@mean
    s <- scaling@scale
    
    # If either mean of scaling is missing
    if (is.null(s)) {
      s <- rep(1,length(m))
    }
    if (is.null(m)) {
      m <- rep(0,length(s))
    }
    
    list(intercept = as.numeric(-w0*s[2]/w[2]+m[2]+m[1]*(s[2]*w[1])/(s[1]*w[2])), 
         slope = as.numeric(-(s[2]*w[1])/(s[1]*w[2])))
  }
}

#' Plot RSSL classifier boundary
#' @param ... List of trained classifiers
#' @param show_guide logical (default: TRUE); Show legend
#' @export
geom_classifier <- function(...,show_guide=TRUE) {
  classifiers <- list(...)
  alt_names <-  eval(substitute(alist(...)))
  if (is.null(names(classifiers))) names(classifiers) <- alt_names
  boundaries <- bind_rows(lapply(1:length(classifiers), 
         function(i) { 
          data.frame(line_coefficients(classifiers[[i]])
                      ) 
  }))
  boundaries$Classifier <- factor(names(classifiers),levels=names(classifiers),ordered=TRUE)
  geom_abline(aes_string(intercept="intercept",slope="slope",linetype="Classifier"),
              data=boundaries,show.legend = show_guide)
}

#' Plot RSSL responsibilities
#' @param classifier Classifier; Classifier to plot the responsibilities for
#' @param data data.frame; Data corresponding to the coordinates of the unlabeled objects
#' @export
geom_responsibilities <- function(classifier,data) {
  data <- data.frame(data,Responsibility=classifier@responsibilities)
  geom_point(aes_string(fill="Responsibility"),shape=21,size=4,color="white", data=data) 
}