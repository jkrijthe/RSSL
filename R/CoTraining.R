# CoTraining<-function(X,y,first_variables,method1,method2,u=75,p=1,n=3,K=100,argsmethod1=list(),argsmethod2=list()) {
# 
# 	X1<-X[,first_variables]
# 	X2<-X[,-first_variables]
# 	X1_u<-X_u[,first_variables]
# 	X2_u<-X_u[,-first_variables]
# 
# 	unlabeled_pool<-sample(1:nrow(X_u),75)
# 
# 	for (k in 1:K) {
# 		#Train classifiers
# 		h1<-method1(X1,y)
# 		h2<-method2(X2,y)
# 
# 		# Choose 2n positive and 2p negative examples to add to the labeled set
# 		# Choose using h1
# 		predict(h1,X1_u[unlabeled_pool,],prob)
# 
# 		# Remove from pool
# 
# 		# Choose using h2
# 		predict(h2,X2_u[unlabeled_pool,],prob)
# 		#Replenish pool
# 		# unlabeled_pool<-c(unlabeled_pool,sample(1:nrow(X_u),2*n+2*p)
# 	}
# 
# 
# 
# 
# }
