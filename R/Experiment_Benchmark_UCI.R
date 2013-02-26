repeats<-10
exp_results_uci<-array(dim=c(1,length(datasets),6,repeats))

for (d in 1:length(names(datasets))) {
  print(names(datasets)[d])
  
  modelform<-modelforms[[d]]
  D_pop<-datasets[[d]]
  classname<-all.vars(modelform)[1]
  nsplits<-10
  
  n_l<-ncol(D_pop)+10
  
  print(n_l)
  n<-1
  
  for (f in 1:repeats) {
    print(f)
    #D_test<-D_pop[sample(1:nrow(D_pop), 1000, replace=TRUE),]
    i_l<-strata(D_pop,classname,c(1,1),method="srswr")$ID_unit
    i_l<-c(i_l, sample(1:nrow(D_pop),n_l-2,replace=TRUE))
    
    #i_u<-sample(1:nrow(D_pop),max(sizes),replace=TRUE)
    D_l <- D_pop[i_l,]
    #D_u <- D_pop[-i_l,]
    D_pop_left<-D_pop[-i_l,]
    i_u<-sample(1:nrow(D_pop_left),1000,replace=TRUE)
    D_u<-D_pop_left[i_u,]
    D_u[,classname] <- rep(NA, nrow(D_u))
    D_test <-D_pop_left[-i_u,]
    D_train<-rbind(D_l,D_u)
    
    if (FALSE) {
      X <- model.matrix(modelform, D_l)
      y <- as.factor(data.matrix(D_l[,classname]))
      
      leps<-1.0
      bestl<-1.0
      for (l in c(1,0.5,0.1,0.05,0.01,0.0)) { 
        eps<-1-mean(D_l[,classname]==factor(bootstrap::crossval(X, y, function(x,y) {FisherClassifier.xy(x,y,modelform,lambda=l,scale=TRUE)}, predictxy, ngroup=4)$cv.fit,levels=1:2,labels=levels(y)))
        #eps<-1-mean(CrossValidation(function(D){FisherClassifier(modelform,D,lambda=l)},D_l, k=nrow(D_l))==D_l[,classname])
        #print(eps)
        if (eps<leps) {
          bestl<-l
          leps<-eps
        }
      }
      print(bestl)
    }  
    
    
    if (FALSE) {
      X <- model.matrix(modelform, D_l)
      D_u_tm<-D_u
      D_u_tm[,classname] <- 1
      X_u <- model.matrix(modelform, D_u_tm)
      y <- as.factor(data.matrix(D_l[,classname]))
      leps<-1.0
      bestl<-1.0
      for (l in c(1,0.5,0.1,0.05,0.01,0.0)) {
        eps<-1-mean(D_l[,classname]==factor(bootstrap::crossval(X, y, function(x,y) {SemiSupervisedFisherClassifier.xy(x,y,X_u,modelform,lambda=l,scale=TRUE)}, predictxy, ngroup=4)$cv.fit,levels=1:2,labels=levels(y)))
        #eps<-1-mean(CrossValidation(function(D){FisherClassifier(modelform,D,lambda=l)},D_l, k=nrow(D_l))==D_l[,classname])
        #print(eps)
        if (eps<leps) {
          bestl<-l
          leps<-eps
        }
      }
      print(bestl)
    }  
    
    #g_constrained <- SemiSupervisedFisherClassifier(modelform, D_train,lambda=bestl,scale=TRUE)
    
    g_unconstrained <- FisherClassifier(modelform, D_l,scale=TRUE)
    g_constrained <- SemiSupervisedFisherClassifier(modelform, D_train,scale=TRUE)
    g_svm <- svm(modelform, D_l)
    g_lr <- LogisticRegression(modelform,D_l)
    #g_tsvm <- TransductiveSVM(modelform,D_train)
    g_erlr <- EntropyRegularizedLogisticRegression(modelform,D_train)
    
    # Results: Classification errors on test set
    exp_results_uci[n,d,1,f]<-1-mean(D_test[,classname]==predict(g_constrained,D_test))
    exp_results_uci[n,d,2,f]<-1-mean(D_test[,classname]==predict(g_unconstrained,D_test))
    exp_results_uci[n,d,3,f]<-1-mean(D_test[,classname]==predict(g_svm,D_test))
    exp_results_uci[n,d,4,f]<-1-mean(D_test[,classname]==predict(g_lr,D_test))
    #exp_results_uci[n,d,5,f]<-1-(predict(g_tsvm,D_test)/100)
      exp_results_uci[n,d,6,f]<-1-mean(D_test[,classname]==predict(g_erlr,D_test))
  }
  
}

for (n in 1:dim(exp_results_uci)[1]) {
  res_table<-matrix(nrow=dim(exp_results_uci)[2],ncol=dim(exp_results_uci)[3],dimnames=list(names(datasets),c("SLLS","LS","SVM","LR","TSVM","ERLR")))
  for (d in 1:dim(exp_results_uci)[2]) {
    res_table[d,]<-rowMeans(exp_results_uci[n,d,,])
  }
  #Print Latex table
  print(xtable(res_table))
}