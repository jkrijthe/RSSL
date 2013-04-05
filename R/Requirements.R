library(class)
library(sampling)
library(caret)
library(MASS)
library(e1071)
library(foreign)
#Used Somewhere
library(pls)
library(compiler)

enableJIT(3)

source('Definitions.R')

source('NormalBasedClassifier.R')

source('NearestMeanClassifier.R')
source('LinearDiscriminantClassifier.R')
source('QuadraticDiscriminantClassifier.R')
source('MCNearestMeanClassifier.R')
source('MCLinearDiscriminantClassifier.R')

source('LeastSquaresClassifier.R')
source('ICLeastSquaresClassifier.R')

source('LogisticRegression.R')
source('EntropyRegularizedLogisticRegression.R')

source('SelfLearning.R')

source('TransductiveSVM.R')