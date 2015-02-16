library(devtools)
library(RSSL)
library(createdatasets)
library("randomForest")
library("sampling")
library("MASS")
library("magrittr")
library("reshape2")

setdatadir("~/Data")

#data <- model.frame(Class~.,Generate2ClassGaussian(n=350,d=34,var = 0.8),na.action=NULL)
#data <- model.frame(Return~.,createIonosphere())
 data <- model.frame(formula(Diagnosis ~ .),createSPECTF())
X <- data[,-1]
y <- data[[1]]
X<-model.matrix(~.-1,X)
#X <- X[,colnames(X)!="(Intercept)"]

#X<-X[,apply(X, 2, var, na.rm=TRUE) != 0] # Remove constant columns
#X<-scale(X) # Pre-scale data



classifiers<-list(
  "LSC"=function(X,y,X_u,y_u) { LeastSquaresClassifier(X,y,x_center=FALSE,scale=FALSE) },
  "SL"=function(X,y,X_u,y_u) { SelfLearning(X,y,X_u,x_center=FALSE,scale=FALSE,method=LeastSquaresClassifier)},
  "USMscale"=function(X,y,X_u,y_u) { USMLeastSquaresClassifier(X,y,X_u,x_center=TRUE,scale=TRUE) },
  "USMnew"=function(X,y,X_u,y_u) { USMLeastSquaresClassifier(X,y,X_u,x_center=TRUE,scale=TRUE,y_scale=TRUE) },
  "ICLS"=function(X,y,X_u,y_u) { ICLeastSquaresClassifier(X,y,X_u,x_center=FALSE,scale=FALSE) },
  "ICLS scale"=function(X,y,X_u,y_u) { ICLeastSquaresClassifier(X,y,X_u,x_center=TRUE,scale=TRUE,y_scale=TRUE,intercept=FALSE) },
  "Proj"=function(X,y,X_u,y_u) { ICLeastSquaresClassifier(X,y,X_u,x_center=FALSE,scale=FALSE,projection="semisupervised") },
  "Proj scale"=function(X,y,X_u,y_u) { ICLeastSquaresClassifier(X,y,X_u,x_center=TRUE,scale=TRUE,projection="semisupervised",y_scale=TRUE,intercept=FALSE) }
)

# classifiers<-list(
#   "nm"=function(X,y,X_u,y_u) { NearestMeanClassifier(X,y,x_center=FALSE,scale=FALSE) },
#   "slnm"=function(X,y,X_u,y_u) { SelfLearning(X,y,X_u,x_center=FALSE,scale=FALSE,method=NearestMeanClassifier)},
#   "mcnm"=function(X,y,X_u,y_u) { MCNearestMeanClassifier(X,y,X_u,x_center=FALSE,scale=FALSE) },
#   "emnm"=function(X,y,X_u,y_u) { EMNearestMeanClassifier(X,y,X_u,x_center=FALSE,scale=FALSE) },
#   "icnm"=function(X,y,X_u,y_u) { ICNearestMeanClassifier(X,y,X_u,x_center=FALSE,scale=FALSE) }
# )
# # 
# classifiers <- list(
#   "lda"=function(X,y,X_u,y_u) { LinearDiscriminantClassifier(X,y,x_center=FALSE,scale=FALSE) },
#   "oracle"=function(X,y,X_u,y_u) { LinearDiscriminantClassifier(rbind(X,X_u),unlist(list(y,y_u)),x_center=FALSE,scale=FALSE) },
#   "sllda"=function(X,y,X_u,y_u) { SelfLearning(X,y,X_u,x_center=FALSE,scale=FALSE,method=LinearDiscriminantClassifier)},
#   "mclda"=function(X,y,X_u,y_u) { MCLinearDiscriminantClassifier(X,y,X_u,x_center=FALSE,scale=FALSE) }
#   "emlda"=function(X,y,X_u,y_u) { EMLinearDiscriminantClassifier(X,y,X_u,x_center=FALSE,scale=FALSE) },
#   "iclda"=function(X,y,X_u,y_u) { ICLinearDiscriminantClassifier(X,y,X_u,x_center=FALSE,scale=FALSE) }
# )

# classifiers <- list(
#  # "oracle"=function(X,y,X_u,y_u) { GRFClassifier(rbind(X,X_u),unlist(list(y,y_u)),rbind(X,X_u),x_center=FALSE,scale=FALSE) },
#   "grf 1"=function(X,y,X_u,y_u) { GRFClassifier(X,y,X_u,x_center=FALSE,scale=FALSE,sigma=1)},
#   "grf 0.5"=function(X,y,X_u,y_u) { GRFClassifier(X,y,X_u,x_center=FALSE,scale=FALSE,sigma=0.5)},
#   "grf 0.2"=function(X,y,X_u,y_u) { GRFClassifier(X,y,X_u,x_center=FALSE,scale=FALSE,sigma=0.2)}
# )
# 
# classifiers <- list(
#   # "oracle"=function(X,y,X_u,y_u) { GRFClassifier(rbind(X,X_u),unlist(list(y,y_u)),rbind(X,X_u),x_center=FALSE,scale=FALSE) },
#   # "usvm"=function(X,y,X_u,y_u) { TSVMuniversvm(X,y,X_u,binary_path="~/Dropbox/Code/SemiSupervisedComparison/universvm1.22/",temp_path = "~/Downloads/",x_center=FALSE,scale=FALSE)},
#   "self ls"=function(X,y,X_u,y_u) { SelfLearning(X,y,X_u,x_center=FALSE,scale=FALSE,method=LeastSquaresClassifier,lambda=1)}
# )
  # function(X,y,X_u,y_u) { 
#   ICLeastSquaresClassifier(X,y,X_u, lambda1 = 0.1, intercept=TRUE,x_center=TRUE,scale=TRUE,projection = "semisupervised") },
#                function(X,y,X_u,y_u) { ICLeastSquaresClassifier(X,y,X_u, lambda1 = 0.1, intercept=TRUE,x_center=TRUE,scale=TRUE) },
#                function(X,y,X_u,y_u) { ICLeastSquaresClassifier(X,y,X_u, lambda2 = 0.1, intercept=TRUE,x_center=TRUE,scale=TRUE,projection = "semisupervised") },
#                function(X,y,X_u,y_u) { ICLeastSquaresClassifier(X,y,X_u, lambda2 = 0.1, intercept=TRUE,x_center=TRUE,scale=TRUE) },
#                function(X,y,X_u,y_u) { ICLeastSquaresClassifier(X,y,X_u, lambda1=0.1, lambda2 = 0.1, intercept=TRUE,x_center=TRUE,scale=TRUE,projection = "semisupervised") },
#                function(X,y,X_u,y_u) { ICLeastSquaresClassifier(X,y,X_u, lambda1=0.1, lambda2 = 0.1, intercept=TRUE,x_center=TRUE,scale=TRUE) },
#                function(X,y,X_u,y_u) { LeastSquaresClassifier(X,y,lambda = 0.1, intercept=TRUE,x_center=TRUE,scale=TRUE) }
# 
# )


verbose <- TRUE
n_labeled <- "enough"
repeats <- 10

if (n_labeled=="enough") { 
  n_l<-max(ncol(X)+5,20) 
} else if (n_labeled=="d") { 
  n_l<-ncol(X)+1 
} else { n_l<-n_labeled }

errorcurve <- ErrorCurveSSL(X,y,classifiers,n_l=n_l,s=2^c(0:10),repeats=repeats,verbose=verbose,with_replacement = TRUE,n_test = 1000)

#errorcurve <- ErrorCurveTransductive(X,y,classifiers,n_l=n_l,s=2^c(4:9),repeats=repeats,verbose=verbose,with_replacement = FALSE)

## Visualize
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)

df_res <- errorcurve$results %>% melt
colnames(df_res) <- c("repeat","Objects","Classifier","Measure","Value")
#levels(df_res$Classifier)
p <- df_res %>%
  group_by(Objects,Classifier,Measure) %>%
  summarize(Mean=mean(Value),SE=sd(Value)/sqrt(n())) %>% 
  ungroup %>%
  #filter(Classifier!=levels(df_res$Classifier)[5]) %>% 
  ggplot(aes(x=Objects,y=Mean,color=Classifier,shape=Classifier)) +
  geom_point(size=4) +
  geom_line(aes(linetype=Classifier)) +
  geom_ribbon(aes(ymax=Mean+SE,ymin=Mean-SE,fill=Classifier),size=0,alpha=0.2) +
  #geom_errorbar(aes(ymax=Mean+2*SE,ymin=Mean-2*SE,fill=Classifier),width=0.1) +
  scale_x_continuous(trans = log2_trans()) +
  theme_classic() +
  facet_wrap(~ Measure,scales="free") +
  xlab("Unlabeled objects")+
  ylab("")

print(p)
