#Test Allabelings
alllabelings(c(0,1),10)

g_ga<-gaconstrainedlr(modelform, D_train)

#Test logLik function
#TODO: still doesn't work for small samples when the data is separable
#TODO: still doesn't work if there is just one class
library(sampling)
D_pop<-data.frame(mlbench.2dnormals(10000,2,distance))
for (j in 1:100) {
i<-strata(D_pop,classname,c(2,2),method="srswor")$ID_unit
D_l<-D_pop[i,]

modelform<-formula("factor(classes)~.")
classname<-all.vars(modelform)[1]
m<-glm(modelform, data=D_l, family=binomial("logit"))


y<-as.numeric(data.matrix(D_l[,classname]))-1
X<-data.matrix(cbind(rep(1,nrow(D_l)),D_l[,!(colnames(D_l) %in% c(classname)), drop=FALSE]))

if (abs( loglikelihood_logisticregression(m$coefficients,X,y)-logLik(m) )>1.0e-4) {
  print(loglikelihood_logisticregression(m$coefficients,X,y))
  print(logLik(m))
}
}