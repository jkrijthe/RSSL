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

#' Plot linear RSSL classifier boundary
#' @param ... List of trained classifiers
#' @param show_guide logical (default: TRUE); Show legend
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' df <- generate2ClassGaussian(100,d=2,var=0.2) %>% 
#'  add_missinglabels_mar(Class~., 0.8)
#'
#' df %>% 
#'  ggplot(aes(x=X1,y=X2,color=Class)) +
#'  geom_point() +
#'  geom_linearclassifier("Supervised"=LinearDiscriminantClassifier(Class~.,df),
#'                        "EM"=EMLinearDiscriminantClassifier(Class~.,df))
#' @export
geom_linearclassifier <- function(...,show_guide=TRUE) {
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


StatClassifier <- 
  ggproto("StatClassifier", Stat, 
          required_aes = c("x","y"),
          
          setup_params = function(data, params) {
            params
          },
          
          compute_group = function(data, scales, classifiers, breaks, precision, brute_force) {
            if (all(data$group>1)) return(NULL)
            out <- lapply(classifiers, function(x) {
              if (hasMethod(line_coefficients,class(x)) && !brute_force) {
                
                coef <- line_coefficients(x)
                y_at_limits <- coef$intercept + coef$slope * scales$x$get_limits()
                x_at_limits <- (scales$y$get_limits()-coef$intercept)/coef$slope
                
                select_y <- (y_at_limits>=scales$y$get_limits()[1] & y_at_limits<=scales$y$get_limits()[2])
                select_x <- (x_at_limits>=scales$x$get_limits()[1] & x_at_limits<=scales$x$get_limits()[2])
                x_vals <- c(scales$x$get_limits()[select_y],x_at_limits[select_x])
                y_vals <- c(y_at_limits[select_y], scales$y$get_limits()[select_x])
                data.frame(x = x_vals,
                           y = y_vals,
                           piece=rep(1,length(x_vals)), group=rep("1",length(x_vals)), stringsAsFactors = FALSE)
              } else {
                
                df_contour <- expand.grid(x=seq(scales$x$get_limits()[1],
                                                scales$x$get_limits()[2],
                                                length.out = precision),
                                          y=seq(scales$y$get_limits()[1],
                                                scales$y$get_limits()[2],
                                                length.out = precision))
                
                df_contour_mat <- df_contour
                if (!is.null(x@modelform)) {
                  colnames(df_contour) <- attr(terms(x@modelform),
                                               "term.labels")
                } else {
                  df_contour <- as.matrix(df_contour)
                }
                
                df_contour_mat$z <- decisionvalues(x,df_contour) - 
                  ifelse(.hasSlot(x,"threshold"),x@threshold,0)
                colnames(df_contour_mat) <- c("x","y","z")
                out <- ggplot2:::contour_lines(df_contour_mat,breaks=0,complete=TRUE) %>% 
                  mutate(group=as.character(data$group[1]))
                out$level <- NULL
                out
              }
            }) %>% 
              bind_rows(.id="classifier")  %>% 
              mutate(piece=paste(classifier,piece,sep="-")) %>% 
              mutate(group=as.integer(factor(paste(classifier,group,sep="-")))) %>% 
              mutate(group=piece)
            
            out
          }  
  )

#' Plot RSSL classifier boundaries
#' 
#' @examples
#' library(RSSL)
#' library(ggplot2)
#' library(dplyr)
#' 
#' df <- generateCrescentMoon(200)
#' 
#' # This takes a couple of seconds to run
#' \dontrun{
#' g_svm <- SVM(Class~.,df,kernel = kernlab::rbfdot(sigma = 1))
#' g_ls <- LeastSquaresClassifier(Class~.,df)
#' g_nm <- NearestMeanClassifier(Class~.,df)
#' 
#' 
#' df %>% 
#'   ggplot(aes(x=X1,y=X2,color=Class,shape=Class)) +
#'   geom_point(size=3) +
#'   coord_equal() +
#'   scale_x_continuous(limits=c(-20,20), expand=c(0,0)) +
#'   scale_y_continuous(limits=c(-20,20), expand=c(0,0)) +
#'   stat_classifier(aes(linetype=..classifier..),
#'                   color="black", precision=50,
#'                   classifiers=list("SVM"=g_svm,"NM"=g_nm,"LS"=g_ls)
#'   )
#' }   
#' @param mapping aes; aesthetic mapping
#' @param data data.frame; data to be displayed
#' @param inherit.aes logical; If FALSE, overrides the default aesthetics
#' @param breaks double; decision value for which to plot the boundary
#' @param precision integer; grid size to sketch classification boundary
#' @param brute_force logical; If TRUE, uses numerical estimation even for linear classifiers
#' @param classifiers List of Classifier objects to plot
#' @param show.legend logical; Whether this layer should be included in the legend
#' @param ... Additional parameters passed to geom
#' @export
stat_classifier <- function(mapping = NULL, data = NULL, show.legend = NA,
                            inherit.aes = TRUE, breaks=0, precision=50, 
                            brute_force=FALSE, classifiers=classifiers,
                            ...) {
  
  if (is.null(names(classifiers))) names(classifiers) <- lapply(classifiers,function(c){c@name})
  
  layer(
    stat = StatClassifier, data = data, mapping = mapping, geom = GeomContour, 
    position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(classifiers=classifiers, na.rm = TRUE, breaks=breaks,precision=precision, brute_force=brute_force,...)
  ) 
}

#' Plot RSSL classifier boundary (deprecated)
#' 
#' Deprecated: Use geom_linearclassifier or stat_classifier to plot classification boundaries
#' 
#' @param ... List of trained classifiers
#' @param show_guide logical (default: TRUE); Show legend
#' @export
geom_classifier <- function(...,show_guide=TRUE) {
  classifiers <- list(...)
  alt_names <-  eval(substitute(alist(...)))
  if (is.null(names(classifiers))) names(classifiers) <- alt_names
  stat_classifier(aes_string(linetype="..classifier.."), classifiers=classifiers,color="black")
}
