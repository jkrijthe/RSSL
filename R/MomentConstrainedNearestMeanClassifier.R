# Nearest Mean using moment constraints. See Loog (2012)

# Formal class definition

setClass("MomentConstrainedNearestMeanClassifier",
         representation(),
         prototype(name="Moment Constrained Nearest Mean Classifier through ad hoc mean shifting"),
         contains="NearestMeanClassifier")

# Constructor method

MomentConstrainedNearestMeanClassifier <- function(modelform, D) {
  D_labelled <- D[!is.na(D[,all.vars(modelform)[1]]),]
  nm_solution <- NearestMeanClassifier(modelform, D_labelled)
  featurenames<-nm_solution@featurenames
  
  m<-nm_solution@means
  m_t<-sapply(D[,featurenames,drop=FALSE],mean)
  m_t_l<-sapply(D_labelled[,featurenames,drop=FALSE],mean)
  
  for (i in 1:nrow(nm_solution@means)) {
    m[i,]<-nm_solution@means[i,]-m_t_l+m_t
  }
  
  new("MomentConstrainedNearestMeanClassifier",
      D=D, modelform=modelform,means=m, classnames=nm_solution@classnames, featurenames=featurenames)
}
