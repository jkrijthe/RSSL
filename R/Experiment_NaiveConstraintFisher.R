library(mlbench)
library(sampling)
library(caret)
library(foreign)
library(parallel)

# Settings
repeats<-10
sizes<-c(2,4,8,16,32,64,128,256)
measurements<-6
n_l<-10

results<-array(dim=c(length(sizes),repeats,6),dimnames=c("Sizes","Repeats","Measurements"))

distance<-2


#Datasets
R<-list()
datasets<-list()
modelforms<-list()


## Evalutation Sets
# datasets[["Kr-vs-Kp"]] <- read.csv("~/Data/kr-vs-kp.data")
# modelforms[["Kr-vs-Kp"]] <- formula("factor(won)~.")


#datasets[["Haberman"]] <- read.csv("~/Data/haberman.data")
#modelforms[["Haberman"]] <- formula("factor(X1.1)~.")

 
#datasets[["Ionosphere"]] <- read.arff("~/Data/UCI/ionosphere.arff")[,-c(2)]
#modelforms[["Ionosphere"]] <- formula("factor(class)~.")
# 
# datasets[["Parkinsons"]] <- read.csv("~/Data/parkinsons.data")[,-c(1:4)]
# modelforms[["Parkinsons"]]<-formula("factor(status)~.")
# 
datasets[["Pima"]] <- read.arff("~/Data/UCI/diabetes.arff")
modelforms[["Pima"]] <- formula("factor(class)~.")
# 
# datasets[["Sonar"]] <- read.arff("~/Data/UCI/sonar.arff")
# modelforms[["Sonar"]]  <- formula("factor(Class)~.")
# 
# datasets[["SPECT"]] <- data.frame(rbind(data.matrix(read.csv("~/Data/SPECT.train")),data.matrix(read.csv("~/Data/SPECT.test"))))
# modelforms[["SPECT"]] <- formula("factor(X1)~.")
# 
# datasets[["SPECTF"]] <- data.frame(rbind(data.matrix(read.csv("~/Data/SPECTF.train")),data.matrix(read.csv("~/Data/SPECTF.test"))))
# modelforms[["SPECTF"]]  <- formula("factor(X1)~.")
# 
# datasets[["Transfusion"]] <- read.csv("~/Data/transfusion.data")[,-c(3)] # Monetary reward is a linear combination of the number of visits
# modelforms[["Transfusion"]]  <- formula("factor(whether.he.she.donated.blood.in.March.2007)~.")
# 
#  datasets[["WDBC"]] <- read.csv("~/Data/wdbc.data")[,-c(3)]
#  modelforms[["WDBC"]]  <- formula("factor(M)~.")




#D_pop<-read.arff("~/Data/UCI/heart-statlog.arff")
#modelform<-formula("factor(class)~.")

#D_pop<-read.arff("~/Data/UCI/breast-cancer.arff")
#modelform<-formula("factor(Class)~.")

#D_pop<-data.frame(mlbench.2dnormals(10000,2,distance))
#D_test<-data.frame(mlbench.2dnormals(10000,2,distance))

# Experiment Loop


exprun<-function(i) {
  classname<-all.vars(modelform)[1]
  
  exp_results<-array(dim=c(length(sizes),6),dimnames=c("Sizes","Measurements"))
  print(i)
  
  #n_l<-ncol(D_pop)-1
  
    D_test<-D_pop[sample(1:nrow(D_pop), 1000, replace=TRUE),]
    i_l<-strata(D_pop,classname,c(1,1),method="srswr")$ID_unit
    i_l<-c(i_l, sample(1:nrow(D_pop),n_l-2,replace=TRUE))
  
  #i_u<-strata(D_pop,classname,c(max(sizes),max(sizes)),method="srswr")$ID_unit
    i_u<-sample(1:nrow(D_pop),max(sizes),replace=TRUE)
    D_l <- D_pop[i_l,]
    D_u_all <- D_pop[i_u,]
    D_u_all[,classname]<-rep(NA, nrow(D_u_all))
    
    # Sample Enough unlabeled points
    
    for (s in 1:length(sizes)) {
      D_u <- D_u_all[1:sizes[s],]
      D_train<-rbind(D_l,D_u)
  
      # Train methods
      #g_constrained <- SemiSupervisedFisherClassifier(modelform, D_train,scale=TRUE,lambda=0.2,lambda1=0.0)
      #g_unconstrained <- FisherClassifier(modelform, D_l,scale=TRUE,lambda=0.2)
      
      g_constrained <- NearestMeanClassifier(modelform, D_train)
      g_unconstrained <- MCNearestMeanClassifier(modelform, D_train)
  
  # Results: Classification errors on test set
  exp_results[s,1]<-1-mean(D_test[,classname]==predict(g_constrained,D_test))
  exp_results[s,2]<-1-mean(D_test[,classname]==predict(g_unconstrained,D_test))
  
  
  # Results: Log likelihood on test set
  exp_results[s,3]<-loss(g_constrained,D_test)
  exp_results[s,4]<-loss(g_unconstrained,D_test)
  
  # Results: Log likelihood on labelled training set
  exp_results[s,5]<-loss(g_constrained,D_l)
  exp_results[s,6]<-loss(g_unconstrained,D_l)
  
  # Visualize Results
  #gr<-qplot(x.1, x.2, colour = classes, data = D_test, asp=1)
  #gr + geom_abline(intercept=-theta[1]/theta[3], slope=-theta[2]/theta[3])
}
  return(exp_results)
}

## Single dataset
# Multicore:
#R<-mclapply(1:repeats,exprun,mc.cores=3)
#R<-lapply(1:10,exprun)

## Multiple datasets
system.time(
for (dname in names(datasets)) {
  print(dname)
  D_pop<-datasets[[dname]]
  modelform<-modelforms[[dname]]
  classname<-all.vars(modelform)[1]
  R[[dname]]<-lapply(as.list(1:repeats),exprun)
  #R[[dname]]<-mclapply(as.list(1:repeats),exprun,mc.cores=3)
})

#save.image(file="ExperimentPeaks.RData")
