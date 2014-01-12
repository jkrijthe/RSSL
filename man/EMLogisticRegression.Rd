\name{EMLogisticRegression}
\alias{EMLogisticRegression}
\title{Expectation Maximization Logistic Regression}
\usage{
EMLogisticRegression(X,y,X_u,intercept=TRUE,scale=FALSE)
}
\arguments{
  \item{X}{Design matrix of the labeled observations}

  \item{y}{Factor or vector of labels}

  \item{X_u}{Design matrix of the unlabeled observations}

  \item{intercept}{TRUE if an intercept should be added to
  the model}

  \item{scale}{If TRUE, apply a z-transform to the design
  matrix X before running the regression}
}
\value{
S4 object of class EMLogisticRegression with the following
slots: \item{theta}{weight vector} \item{classnames}{the
names of the classes}
}
\description{
Semi-supervised Logistic regression using expectation
maximization, a way of soft self-learning
}

