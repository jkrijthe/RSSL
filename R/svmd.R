# This code is a slightly adapted version of the libsvm interface of the Bioconductor Kebabs package.

svmd <-
function (x, ...)
    UseMethod ("svmd")

svmd.formula <-
function (formula, data = NULL, ..., subset, na.action = na.omit, scale = TRUE)
{
    call <- match.call()
    addArgs <- list(...)
    if (!inherits(formula, "formula"))
        stop("method is only for formula objects")
    if ("kernel" %in% names(addArgs) && addArgs$kernel == "precomputed")
        stop("formula input not supported for precomputed kernel")
    m <- match.call(expand.dots = FALSE)
    if (identical(class(eval.parent(m$data)), "matrix"))
        m$data <- as.data.frame(eval.parent(m$data))
    m$... <- NULL
    m$scale <- NULL
    m[[1]] <- as.name("model.frame")
    m$na.action <- na.action
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    attr(Terms, "intercept") <- 0
    x <- model.matrix(Terms, m)
    y <- model.extract(m, "response")
    attr(x, "na.action") <- attr(y, "na.action") <- attr(m, "na.action")
    if (length(scale) == 1)
        scale <- rep(scale, ncol(x))
    if (any(scale)) {
        remove <- unique(c(which(labels(Terms) %in%
                                 names(attr(x, "contrasts"))),
                           which(!scale)
                           )
                         )
        scale <- !attr(x, "assign") %in% remove
    }
    ret <- svmd.default (x, y, scale = scale, ..., na.action = na.action)
    ret$call <- call
    ret$call[[1]] <- as.name("svmd")
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action")))
        ret$na.action <- attr(m, "na.action")
    class(ret) <- c("svm.formula", class(ret))
    return (ret)
}

#' @export
svmd.default <-
function (x,
          y           = NULL,
          scale       = TRUE,
          type        = NULL,
          kernel      = "radial",
          degree      = 3,
          gamma       = if (is.vector(x)) 1 else 1 / ncol(x),
          coef0       = 0,
          cost        = 1,
          nu          = 0.5,
          class.weights = NULL,
          cachesize   = 40,
          tolerance   = 0.001,
          epsilon     = 0.1,
          shrinking   = TRUE,
          cross       = 0,
          probability = FALSE,
          fitted      = TRUE,
          ...,
          subset,
          na.action = na.omit,
          upbound     = NULL)
{
    if (is(x, "KernelMatrix") || is(x, "kernelMatrix"))
        kernel <- "precomputed"
    
    sparse <- FALSE
    
    ## dense precomputed kernel matrix or dense or sparse data matrix
    if (is(kernel, "character") && kernel == "precomputed")
    {
        if (!missing(scale) && any(scale))
            stop("please scale data before computing the kernel matrix")
        scale <- FALSE
        if (inherits(x, "Matrix"))
            x <- as.matrix(x)
        if (inherits(x, "matrix.csr")) {
            if (!requireNamespace("SparseM", quietly=TRUE))
                stop("package SparseM could not be loaded\n")
            x <- as.matrix(x)
        }
        if (!is.matrix(x))
            stop(sQuote("x"), "is not a precomputed kernel matrix")
        if (nrow(x) != ncol(x))
            stop("precomputed kernel matrix must be symmetric")
    }
    else
    {
        if(inherits(x, "Matrix")) {
            if (!requireNamespace("SparseM", quietly=TRUE))
                stop("package SparseM could not be loaded\n")
            x <- as(x, "matrix.csr")
        }
        if(inherits(x, "simple_triplet_matrix")) {
            if (!requireNamespace("SparseM", quietly=TRUE))
                stop("package SparseM could not be loaded\n")
            ind <- order(x$i, x$j)
            x <- new("matrix.csr",
            ra = x$v[ind],
            ja = x$j[ind],
            ia = as.integer(cumsum(c(1, tabulate(x$i[ind])))),
            dimension = c(x$nrow, x$ncol))
        }
        if (sparse <- inherits(x, "matrix.csr"))
        {
            if (!requireNamespace("SparseM", quietly=TRUE))
                stop("package SparseM could not be loaded\n")
        }
        else
            x <- as.matrix(x)
    }

    xhold   <- if (fitted) x else NA
    x.scale <- y.scale <- NULL
    formula <- inherits(x, "svm.formula")

    ## determine model type
    if (is.null(type)) type <-
        if (is.null(y)) "one-classification"
        else if (is.factor(y)) "C-classification"
        else "eps-regression"

    type <- pmatch(type, c("C-classification",
                           "nu-classification",
                           "one-classification",
                           "eps-regression",
                           "nu-regression"), 99) - 1

    if (type > 10) stop("wrong type specification!")

    kernel <- pmatch(kernel, c("linear",
                               "polynomial",
                               "radial",
                               "sigmoid",
                               "precomputed"), 99) - 1

    if (kernel > 10) stop("wrong kernel specification!")
    
    nac <- attr(x, "na.action")

    if (missing(subset)) subset <- NULL

    if (sparse) {
        scale <- rep(FALSE, ncol(x))
        if(!is.null(y)) na.fail(y)
            x <- SparseM::t(SparseM::t(x)) ## make shure that col-indices
                                           ## are sorted
    } else {
        ## subsetting and na-handling for matrices
        ## determine rows with missing entries
        missed <- attr(na.action(x), "na.action")
        if (!is.null(y))
            missed <- union(missed, attr(na.action(y), "na.action"))

        if (!is.null(missed)) {
            if (is.null(subset))
                subset <- setdiff(1:nrow(x), missed)
            else
                subset <- setdiff(subset, missed)
        }

        if (!is.null(subset)) {
            if (kernel == 4) {
                x <- x[subset, subset] ## matrix must be quadratic
                if (!is.null(y)) y <- y[subset]
            } else {
                x <- x[subset, ]
                if (!is.null(y)) y <- y[subset]
            }
        }

        ## scaling
        if (length(scale) == 1)
            scale <- rep(scale, ncol(x))
        if (any(scale)) {
            co <- !apply(x[,scale, drop = FALSE], 2, var)
            if (any(co)) {
                warning(paste("Variable(s)",
                              paste(sQuote(colnames(x[,scale,
                                                      drop = FALSE])[co]),
                                    sep="", collapse=" and "),
                              "constant. Cannot scale data.")
                        )
                scale <- rep(FALSE, ncol(x))
            } else {
                xtmp <- scale(x[,scale])
                x[,scale] <- xtmp
                x.scale <- attributes(xtmp)[c("scaled:center","scaled:scale")]
                if (is.numeric(y) && (type > 2)) {
                    y <- scale(y)
                    y.scale <- attributes(y)[c("scaled:center","scaled:scale")]
                    y <- as.vector(y)
                }
            }
        }
    }

    ## further parameter checks
    nr <- nrow(x)
    if (cross > nr)
        stop(sQuote("cross"), " cannot exceed the number of observations!")

    if (!is.vector(y) && !is.factor (y) && type != 2)
        stop("y must be a vector or a factor.")
    if (type != 2 && length(y) != nr)
        stop("x and y don't match.")

    if (cachesize < 0.1)
        cachesize <- 0.1

    if (type > 2 && !is.numeric(y))
        stop("Need numeric dependent variable for regression.")

    lev <- NULL
    weightlabels <- NULL

    ## in case of classification: transform factors into integers
    if (type == 2) # one class classification --> set dummy
        y <- rep(1, nr)
    else
        if (is.factor(y)) {
            lev <- levels(y)
            y <- as.integer(y)
            if (!is.null(class.weights)) {
                if (is.null(names(class.weights)))
                    stop("Weights have to be specified along with their",
                         "according level names !")
                weightlabels <- match (names(class.weights), lev)
                if (any(is.na(weightlabels)))
                    stop("At least one level name is missing or misspelled.")
            }
        } else {
            if (type < 3) {
                if(any(as.integer(y) != y))
                    stop("dependent variable has to be of factor or integer",
                         "type for classification mode.")
                y <- as.factor(y)
                lev <- levels(y)
                y <- as.integer(y)
            } else lev <- unique(y)
        }

    nclass <- 2
    if (type < 2) nclass <- length(lev)

    if (type > 1 && length(class.weights) > 0) {
        class.weights <- NULL
        warning(sQuote("class.weights"), " are set to NULL for regression",
                "mode. For classification, use a _factor_ for ", sQuote("y"),
                ", or specify the correct ", sQuote("type"), " argument.")
    }

    err <- empty_string <- paste(rep(" ", 255), collapse = "")
  
    if (is.null(upbound)) {
      ubound <- 0
      upbound <- 0.0
    } else {
      if (length(upbound)!=length(y)) { stop("incorrect number of upper bounds.")}
      ubound <- 1
    }
    
    if (is.null(type)) stop("type argument must not be NULL!")
    if (is.null(kernel)) stop("kernel argument must not be NULL!")
    if (is.null(degree)) stop("degree argument must not be NULL!")
    if (is.null(gamma)) stop("gamma argument must not be NULL!")
    if (is.null(coef0)) stop("coef0 seed argument must not be NULL!")
    if (is.null(cost)) stop("cost argument must not be NULL!")
    if (is.null(nu)) stop("nu argument must not be NULL!")
    if (is.null(cachesize)) stop("cachesize argument must not be NULL!")
    if (is.null(tolerance)) stop("tolerance argument must not be NULL!")
    if (is.null(epsilon)) stop("epsilon argument must not be NULL!")
    if (is.null(shrinking)) stop("shrinking argument must not be NULL!")
    if (is.null(cross)) stop("cross argument must not be NULL!")
    if (is.null(sparse)) stop("sparse argument must not be NULL!")
    if (is.null(probability)) stop("probability argument must not be NULL!")

    cret <- .C("svmtraind",
                ## data
                as.double  (if (sparse) x@ra else t(x)),
                as.integer (nr), as.integer(ncol(x)),
                as.double  (y),
                ## sparse index info
                as.integer (if (sparse) x@ia else 0),
                as.integer (if (sparse) x@ja else 0),

                ## parameters
                as.integer (type),
                as.integer (kernel),
                as.integer (degree),
                as.double  (gamma),
                as.double  (coef0),
                as.double  (cost),
                as.double  (nu),
                as.integer (weightlabels),
                as.double  (class.weights),
                as.integer (length (class.weights)),
                as.double  (cachesize),
                as.double  (tolerance),
                as.double  (epsilon),
                as.integer (shrinking),
                as.integer (cross),
                as.integer (sparse),
                as.integer (probability),
                as.integer (ubound),
                as.double  (upbound),

                ## results
                nclasses = integer  (1),
                nr       = integer  (1), # nr of support vectors
                index    = integer  (nr),
                labels   = integer  (nclass),
                nSV      = integer  (nclass),
                rho      = double   (nclass * (nclass - 1) / 2),
                coefs    = double   (nr * (nclass - 1)),
                sigma    = double   (1),
                probA    = double   (nclass * (nclass - 1) / 2),
                probB    = double   (nclass * (nclass - 1) / 2),

                cresults = double   (cross),
                ctotal1  = double   (1),
                ctotal2  = double   (1),
                error    = err,
                obj      = double   (1))

    if (cret$error != empty_string)
        stop(paste(cret$error, "!", sep=""))

    cret$index <- cret$index[1:cret$nr]

    ret <- list (
                 call     = match.call(),
                 type     = type,
                 kernel   = kernel,
                 cost     = cost,
                 degree   = degree,
                 gamma    = gamma,
                 coef0    = coef0,
                 nu       = nu,
                 epsilon  = epsilon,
                 sparse   = sparse,
                 scaled   = scale,
                 x.scale  = x.scale,
                 y.scale  = y.scale,
                 selected = subset, ## indices of samples used [UB]
                                    ## for prediction (after NA-handling
                                    ## and subsetting)
                 nclasses = cret$nclasses, #number of classes
                 levels   = lev,
                 tot.nSV  = cret$nr, #total number of sv
                 #number of SV in diff. classes
                 nSV      = cret$nSV[1:cret$nclasses],
                 labels   = cret$label[1:cret$nclasses], #labels of the SVs.
                 SV       = if (kernel == 4) NULL
                 else if (sparse) SparseM::t(SparseM::t(x[cret$index,]))
                 else t(t(x[cret$index,])), #copy of SV
                 index    = cret$index,  #indexes of sv in x
                 ##constants in decision functions
                 rho      = cret$rho[1:(cret$nclasses *
                                     (cret$nclasses - 1) / 2)],
                 ##probabilites
                 compprob = probability,
                 probA    = if (!probability) NULL else
                 cret$probA[1:(cret$nclasses * (cret$nclasses - 1) / 2)],
                 probB    = if (!probability) NULL else
                 cret$probB[1:(cret$nclasses * (cret$nclasses - 1) / 2)],
                 sigma    = if (probability) cret$sigma else NULL,
                 ##coefficients of sv
                 coefs    = if (cret$nr == 0) NULL else
                 t(matrix(cret$coefs[1:((cret$nclasses - 1) * cret$nr)],
                          nrow = cret$nclasses - 1,
                          byrow = TRUE)),
                 na.action = nac,
                 obj=cret$obj
                 )

    ## cross-validation-results
    if (cross > 0)
        if (type > 2) {
            scale.factor     <- if (any(scale))
                                    crossprod(y.scale$"scaled:scale") else 1;
            ret$MSE          <- cret$cresults * scale.factor;
            ret$tot.MSE      <- cret$ctotal1  * scale.factor;
            ret$scorrcoeff   <- cret$ctotal2;
        } else {
            ret$accuracies   <- cret$cresults;
            ret$tot.accuracy <- cret$ctotal1;
        }

    class (ret) <- "svm"

    if (fitted) {
        ret$fitted <- na.action(predict.svmd(ret, xhold,
                                             decision.values = TRUE))
        ret$decision.values <- attr(ret$fitted, "decision.values")
        attr(ret$fitted, "decision.values") <- NULL
        if (type > 1) ret$residuals <- y - ret$fitted
    }

    ret
}

predict.svmd <-
function (object, newdata,
          decision.values = FALSE,
          probability = FALSE,
          ...,
          na.action = na.omit)
{
    if (missing(newdata))
        return(fitted(object))

    if (object$tot.nSV < 1)
        stop("Model is empty!")

    if (is.vector(newdata) && is.atomic(newdata))
        newdata <- t(t(newdata))

    if (object$kernel == 4)
    {
        if (inherits(newdata, "Matrix"))
            newdata <- as.matrix(newdata)
        if (inherits(newdata, "matrix.csr")) {
            if (!requireNamespace("SparseM", quietly=TRUE))
                stop("package SparseM could not be loaded\n")
            newdata <- as.matrix(newdata)
        }
        if (!is.matrix(newdata))
            stop(sQuote("newdata"), "is not a precomputed kernel matrix")
        sparse <- FALSE
        SVs <- max(if (is.null(object$selected))
                       object$index
                   else
                       object$selected[object$index]
                   )
        if (ncol(newdata) < max(SVs))
            stop ("test data does not match model !")
    } else {
        if(inherits(newdata, "Matrix")) {
            if (!requireNamespace("SparseM", quietly=TRUE))
                stop("package SparseM could not be loaded\n")
            newdata <- as(newdata, "matrix.csr")
        }
        if(inherits(newdata, "simple_triplet_matrix")) {
            if (!requireNamespace("SparseM", quietly=TRUE))
                stop("package SparseM could not be loaded\n")
            ind <- order(newdata$i, newdata$j)
            newdata <- new("matrix.csr",
                ra = newdata$v[ind],
                ja = newdata$j[ind],
                ia = as.integer(cumsum(c(1, tabulate(newdata$i[ind])))),
                dimension = c(newdata$nrow, newdata$ncol))
        }
        if (sparse <- inherits(newdata, "matrix.csr"))
            newdata <- SparseM::t(SparseM::t(newdata))
        else
            newdata <- as.matrix(newdata)
    }
    
    if (object$sparse || sparse)
    {
        if (!requireNamespace("SparseM", quietly=TRUE))
            stop("package SparseM could not be loaded\n")
    }

    act <- NULL
    preprocessed <- !is.null(attr(newdata, "na.action"))
    rowns <- if (!is.null(rownames(newdata)))
        rownames(newdata)
    else
        1:nrow(newdata)
    if (!object$sparse) {
        if (inherits(object, "svm.formula")) {
            if(is.null(colnames(newdata)))
            colnames(newdata) <- colnames(object$SV)
            newdata <- na.action(newdata)
            act <- attr(newdata, "na.action")
            newdata <- model.matrix(delete.response(terms(object)),
            as.data.frame(newdata))
        } else {
            newdata <- na.action(as.matrix(newdata))
            act <- attr(newdata, "na.action")
        }
    }
    
    if (!is.null(act) && !preprocessed)
        rowns <- rowns[-act]

    if (any(object$scaled))
        newdata[,object$scaled] <-
            scale(newdata[,object$scaled, drop = FALSE],
                  center = object$x.scale$"scaled:center",
                  scale  = object$x.scale$"scaled:scale"
                  )
    
    ret <- .C("svmpredictd",
               as.integer (decision.values),
               as.integer (probability),

               ## model
               as.double  (if (object$kernel == 4) {
                               if (is.null(object$selected))
                                   object$index
                               else
                                   object$selected[object$index]
                           } else if (object$sparse) object$SV@ra
                                  else t(object$SV)),
               as.integer (if (object$kernel == 4) object$tot.nSV
                           else nrow(object$SV)),
               as.integer (if (object$kernel == 4) ncol(newdata)
                           else ncol(object$SV)),
               as.integer (if (object$sparse) object$SV@ia else 0),
               as.integer (if (object$sparse) object$SV@ja else 0),
               as.double  (as.vector(object$coefs)),
               as.double  (object$rho),
               as.integer (object$compprob),
               as.double  (if (object$compprob) object$probA else 0),
               as.double  (if (object$compprob) object$probB else 0),
               as.integer (object$nclasses),
               as.integer (object$tot.nSV),
               as.integer (object$labels),
               as.integer (object$nSV),
               as.integer (object$sparse),

               ## parameter
               as.integer (object$type),
               as.integer (object$kernel),
               as.integer (object$degree),
               as.double  (object$gamma),
               as.double  (object$coef0),

               ## test matrix
               as.double  (if (sparse) newdata@ra else t(newdata)),
               as.integer (nrow(newdata)),
               as.integer (if (sparse) newdata@ia else 0),
               as.integer (if (sparse) newdata@ja else 0),
               as.integer (sparse),

               ## decision-values
               ret = double(nrow(newdata)),
               dec = double(nrow(newdata) * object$nclasses *
                            (object$nclasses - 1) / 2),
               prob = double(nrow(newdata) * object$nclasses))
  
    ret2 <- if (is.character(object$levels)) # classification: return factors
        factor (object$levels[ret$ret], levels = object$levels)
    else if (object$type == 2) # one-class-classification: return TRUE/FALSE
        ret$ret == 1
    else if (any(object$scaled) && !is.null(object$y.scale))
        # return raw values, possibly scaled back
        ret$ret * object$y.scale$"scaled:scale" + object$y.scale$"scaled:center"
    else
        ret$ret

    names(ret2) <- rowns
    ret2 <- napredict(act, ret2)

    if (decision.values) {
        colns = c()
        for (i in 1:(object$nclasses - 1))
            for (j in (i + 1):object$nclasses)
                colns <- c(colns,
                           paste(object$levels[object$labels[i]],
                                 "/", object$levels[object$labels[j]],
                                 sep = ""))
        attr(ret2, "decision.values") <-
            napredict(act,
                      matrix(ret$dec, nrow = nrow(newdata), byrow = TRUE,
                             dimnames = list(rowns, colns)
                             )
                      )
    }

    if (probability && object$type < 2) {
        if (!object$compprob)
            warning("SVM has not been trained using `probability = TRUE`, probabilities not available for predictions.")
        else
            attr(ret2, "probabilities") <-
                napredict(act,
                          matrix(ret$prob, nrow = nrow(newdata), byrow = TRUE,
                                 dimnames = list(rowns, object$levels[object$labels])
                                 )
                          )
    }

    ret2
}
