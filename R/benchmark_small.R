# Benchmark, small

i_removed<-sample(150,120,replace=FALSE)
D<-iris
D$Species[i_removed]<-rep(NA, length(i_removed))
D_unlabelled<-iris[i_removed,]
modelform<-formula("Species~.")

# Linear Discriminant

m<-lda(modelform,D[-i_removed,])
mean(predict(m,D_unlabelled)$class==D_unlabelled$Species)

m<-selflearning(modelform,D)
mean(predict(m,D_unlabelled)$class==D_unlabelled$Species)

# Nearest Neighbor

m<-nearestmean(modelform,D[-i_removed,])
mean(predict(m,D_unlabelled)==D_unlabelled$Species)

m<-mcnm(modelform,D)
mean(predict(m,D_unlabelled)==D_unlabelled$Species)
