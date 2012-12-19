# Formal class definition
setClass("ImplicitlyConstrainedNearestMeanClassifier",
         representation(parameters.allowed="matrix"),
         prototype(name="Implicitly Constrained Nearest Mean Classifier"),
         contains="NearestMeanClassifier")

# Constructor Method
ImplicitlyConstrainedNearestMeanClassifier <- function(modelform, D) {
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  nm_solution<-NearestMeanClassifier(modelform,D_l)
  featurenames<-nm_solution@featurenames

  labelings<-alllabelings(levels(D[,classname]),nrow(D_u))

  parameters.allowed<-matrix(nrow=ncol(labelings),ncol=length(featurenames)*2+1)
  # Determine which labelling gives highest Log-Likelihood on the -supervised- data and return as model
  ll<--Inf
  for (i in 1:ncol(labelings)) {
    D_u_i<-D_u
    D_u_i[,classname]<-labelings[,i]
    D_i<-rbind(D_l,D_u_i)
    h_i <- NearestMeanClassifier(modelform,D_i)
   
    ll_i<-logLik(h_i,D_l)
    parameters.allowed[i,]<-c(as.vector(h_i@means),ll_i)
    
    if (!is.na(ll_i) && ll_i>ll) { 
      ll<-ll_i
      h_trained<-h_i
    }
  }
  new("ImplicitlyConstrainedNearestMeanClassifier",means=h_trained@means,classname=classname,classnames=nm_solution@classnames,featurenames=featurenames,parameters.allowed=parameters.allowed,modelform=modelform)
}

#setMethod("ImplicitlyConstrainedNearestMeanClassifier", signature(modelform="formula"), ImplicitlyConstrainedNearestMeanClassifier)

# Plot function
setMethod("plot", signature(x="ImplicitlyConstrainedNearestMeanClassifier"), function(x) {
  object<-x
  pl<-qplot(object@parameters.allowed[,1],object@parameters.allowed[,2],color=object@parameters.allowed[,3])
  pl+geom_point(x=object@means[1,], y=object@means[2,], shape=1)
})