

#   if (sampling=="empirical") {
#     # Sample a large test set
#     D_test<-D_pop[sample(1:nrow(D_pop), 1000, replace=TRUE),]
#     # Sample labeled points
#     i_l<-strata(D_pop,classname,c(1,1),method="srswr")$ID_unit
#     i_l<-c(i_l, sample(1:nrow(D_pop),n_l-2,replace=TRUE)) # Take a minimum of 1 object from each class
#     D_l <- D_pop[i_l,]
#   
#     # Sample enough unlabeled points
#     i_u<-sample(1:nrow(D_pop),max(sizes),replace=TRUE)
#     D_u_all <- D_pop[i_u,]
#     D_u_all[,classname]<-rep(NA, nrow(D_u_all)) # Remove class names
#   }