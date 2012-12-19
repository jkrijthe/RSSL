alllabelings<-function(options,k) {
  if (k==1){ A<-options }
    else {
        A<-c()
        for (i in 1:length(options)) {
            A<-cbind(A, rbind(rep(options[i],length(options)^(k-1)), alllabelings(options,k-1)))
        }
    }
  return(A)
}

constrainedlr<-function(modelform, D) {
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  # Construct all possible labelings
  labelings<-alllabelings(levels(D[,classname]),nrow(D_u))
  #print(labelings)
    
  # Determine which labelling gives highest Log-Likelihood on the -supervised- data and return as model
  ll<--Inf
  for (i in 1:ncol(labelings)) {
    D_u_i<-D_u
    D_u_i[,classname]<-labelings[,i]
    D_i<-rbind(D_l,D_u_i)
    h_i <- glm(modelform, data=D_i, family=binomial("logit"))
    ll_i<-logLik(h_i)
    
    #print(ll_i)
    #y<-data.matrix(as.numeric(D_i[,classname])-1)
    #X<-data.matrix(cbind(rep(1,nrow(D_i)),D_i[,!(colnames(D_i) %in% c(classname)), drop=FALSE]))
    #print(loglikelihood_logisticregression(h_i$coefficients,X,y))
    
    
    
    y<-as.numeric(data.matrix(D_l[,classname]))-1
    X<-data.matrix(cbind(rep(1,nrow(D_l)),D_l[,!(colnames(D_l) %in% c(classname)), drop=FALSE]))

    ll_i<-loglikelihood_logisticregression(h_i$coefficients,X,y)
    
    if (!is.na(ll_i) && ll_i>ll) { 
      ll<-ll_i
      h_trained<-h_i
      print(ll)
    }
  }
  print(ll)
  print(logLik(glm(modelform, data=D_l, family=binomial("logit"))))
  print("Done")
  if (ll==0) print(D_l)
  return(h_trained)
}

loglikelihood_logisticregression<-function(theta,X,y) {  
  #q<-1/(1+exp(X %*% theta))
  #q[q==0.0]<-q[q==0.0]+1.0e-10
  #q[q==1.0]<-q[q==1.0]-1.0e-10
  #ll<-sum(y*log(1-q)+(1-y)*log(q))
  
  sum(y*(X %*% theta) - log(1+exp(X%*%theta)))
}

gaconstrainedlr<-function(modelform, D) {
  library("genalg")
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_l$classes<-as.integer(D_l$classes)-1
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  n_u<-nrow(D_u)
  
  objective <- function(w) {
    print(w)
    D_u_i<-D_u
    #D_u_i[,classname]<-as.integer(w>0.5)
    D_u_i[,classname]<-w
    D_i<-rbind(D_l,D_u_i)
    #print(D_i)
    h_trained<-glm(modelform, data=D_i, family=binomial("logit"))
    print(h_trained$coefficients)
    
    # Log likelihood on all (real and inferred) data
    y<-data.matrix(as.numeric(D_i[,classname]))
    X<-data.matrix(cbind(rep(1,nrow(D_i)),D_i[,!(colnames(D_i) %in% c(classname)), drop=FALSE]))
    print(logLik(h_trained))
    print(loglikelihood_logisticregression(h_trained$coefficients,X,y))
    
      #return the log likelihood of the supervised data only
    y<-data.matrix(as.numeric(D_l[,classname]))
    X<-data.matrix(cbind(rep(1,nrow(D_l)),D_l[,!(colnames(D_l) %in% c(classname)), drop=FALSE]))
    
    print(loglikelihood_logisticregression(h_trained$coefficients,X,y))
    loglikelihood_logisticregression(h_trained$coefficients,X,y)
  }
  
  rbga.results = rbga(rep(0,n_u), rep(1,n_u), evalFunc=objective, verbose=TRUE, mutationChance=0.01)
  #plot(rbga.results)
  #plot(rbga.results, type="hist")
  #plot(rbga.results, type="vars")
  
  return(h_trained)
}


lr<-function(modelform,D) {
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  D_l <- D[!is.na(D[,classname]),] # labeled data
  
  # Return the Logistic regression model based only on the labeled data
  return(glm(modelform, data=D_l, family=binomial("logit")))
}

