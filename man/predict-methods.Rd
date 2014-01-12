\docType{methods}
\name{predict}
\alias{predict}
\alias{predict,LeastSquaresClassifier-method}
\alias{predict,LinearSVM-method}
\alias{predict,LogisticLossClassifier-method}
\alias{predict,LogisticRegression-method}
\alias{predict,NormalBasedClassifier-method}
\alias{predict,SGDSVM-method}
\alias{predict,SelfLearning-method}
\title{Predictions of a classifier or regression function}
\usage{
predict(object, ...)

predict(object, X, probs=FALSE)

predict(object, X, probs=FALSE)

\S4method{predict}{LogisticRegression}(object, newdata, probs = FALSE)

predict(object, newdata)

predict(object, newdata)

\S4method{predict}{LinearSVM}(object, newdata)

\S4method{predict}{SelfLearning}(object, newdata, ...)
}
\arguments{
  \item{object}{Object of class LeastSquaresClassifier}

  \item{X}{Design matrix of the test data, intercept term
  is added within the function}

  \item{newdata}{data.frame object with test data}

  \item{probs}{whether class probabilities should be
  returned}

  \item{object}{Object of class LogisticLossClassifier}

  \item{X}{Design matrix of the test data, intercept term
  is added within the function}

  \item{newdata}{data.frame object with test data}

  \item{probs}{whether class probabilities should be
  returned}

  \item{object}{object of class NormalBasedClassifier}

  \item{newdata}{a matrix of data.frame, depending on what
  was used to train the classifier, with new objects to be
  classified}

  \item{object}{object of class SGDSVM}

  \item{newdata}{a matrix of data.frame, depending on what
  was used to train the classifier, with new objects to be
  classified}
}
\value{
factor of predicted classes

factor of predicted classes

a vector with predicted classes

a vector with predicted classes
}
\description{
Predictions of a classifier or regression function

Predict classes of new data based on trained least squares
classifier

Predict classes of new data based on trained least squares
classifier

Log likelihood on the training set Predict

Predict the class of new objects for a normal based
classifier

Predict the class of new objects for a normal based
classifier

Predict class of new observations using a LinearSVM

Predict method for SelfLearning Classifier The Predict
Method delegates prediction to the specific model object
}

