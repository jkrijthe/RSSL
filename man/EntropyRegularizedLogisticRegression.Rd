\name{EntropyRegularizedLogisticRegression}
\alias{EntropyRegularizedLogisticRegression}
\title{Entropy Regularized Logistic Regression}
\usage{
  EntropyRegularizedLogisticRegression(X, y,
    X_u=NULL,lambda_entropy=1.0,lambda=0.0,intercept=TRUE,
    init=NA)
}
\arguments{
  \item{X}{Design matrix, intercept term is added within
  the function}

  \item{y}{Vector or factor with class assignments}

  \item{X_u}{Design matrix of the unlabeled data, intercept
  term is added within the function}

  \item{lambda_entropy}{Weight of the labeled observations
  compared to the unlabeled observations}

  \item{lambda}{l2 Regularization}

  \item{intercept}{Whether an intercept should be added to
  the model}

  \item{init}{Initial parameters for the gradient descent}
}
\value{
  S4 object of class EntropyRegularizedLogisticRegression
  with the following slots: \item{theta}{weight vector}
  \item{classnames}{the names of the classes}
}
\description{
  Entropy regularized logistic regression as introduced by
  \cite{Grandvalet2005}.
}

