library(reshape)
library(ggplot2)
library(plotrix)
library(gridExtra)

# Visualize Results
plots1<-list()
plots2<-list()



for (dname in names(datasets)) {

Res<-merge(cast(melt(R[[dname]]),"X1~X2",fun.aggregate=mean),cast(melt(R[[dname]]),"X1~X2",fun.aggregate=std.error),by="X1")
names(Res)<-c("Sizes",
            "SSLerror","SLerror",
            "SSLlossTest","SLlossTest",
            "SSLlossTrain","SLlossTrain",
            "SSLerrorSTD","SLerrorSTD",
            "SSLlossTestSTD","SLlossTestSTD",
            "SSLlossTrainSTD","SLlossTrainSTD")
  
h <- ggplot(Res, aes(x=Sizes,y=SSLerror))
h <- h + geom_ribbon(aes(ymin=SLerror-SLerrorSTD, ymax=SLerror+SLerrorSTD),alpha=0.5,fill="red") + geom_line(aes(y=SLerror),color="red")
h <- h + geom_ribbon(aes(ymin=SSLerror-SSLerrorSTD, ymax=SSLerror+SSLerrorSTD),alpha=0.5,fill="green") + geom_line(aes(y=SSLerror),color="green")
h <- h + scale_x_discrete(breaks = 1:length(sizes), labels=sizes) 
h <- h + xlab("Number of unlabeled objects")
h <- h + ggtitle(dname)
h <- h + theme( # remove the vertical grid lines
  panel.grid.major.x = element_blank() ,
  # explicitly set the horizontal lines (or they will disappear too)
  panel.grid.major.y = element_blank(),
  panel.background = element_blank()
)
  h<-h + ylab("Classification error")

  plots1[[dname]] <-h

  
  
h <- ggplot(Res, aes(x=Sizes))
h <- h + geom_ribbon(aes(ymin=SLlossTest-SLlossTestSTD, ymax=SLlossTest+SLlossTestSTD),alpha=0.5,fill="red") + geom_line(aes(y=SLlossTest),color="red")
h <- h + geom_ribbon(aes(ymin=SSLlossTest-SSLlossTestSTD, ymax=SSLlossTest+SSLlossTestSTD),alpha=0.5,fill="green") + geom_line(aes(y=SSLlossTest),color="green")
h <- h + scale_x_discrete(breaks = 1:length(sizes), labels=sizes)
h <- h + xlab("Number of unlabeled objects")
h <- h + ggtitle(dname)
h <- h + theme( # remove the vertical grid lines
  panel.grid.major.x = element_blank() ,
  # explicitly set the horizontal lines (or they will disappear too)
  panel.grid.major.y = element_blank(),
  panel.background = element_blank()
)
h<-h + ylab("Loss")
  plots2[[dname]] <-h
}

do.call(grid.arrange,  plots1)
do.call(grid.arrange,  plots2)


# R<-data.frame(cbind(sizes, t(apply(results,1,colMeans))[,1:2], t(apply(results,1,function(x){apply(x,2,std.error)}))[,1:2]))
# names(R)<-c("Sizes","SSL","SL","SSLsd","SLsd")
# 
# R<-cast(melt(results),"X1+X2~X3")
# names(R)<-c("Sizes","Repeat","SSLerror","SLerror","SSLlossTest","SLlossTest","SSLlossTrain","SLlossTrain","NA1","NA2")
# 
# library(lattice)
# bwplot(formula("SSLlossTest+SLlossTest~Sizes"),
#        type="b",
#        data=R,
#        main="Pima n=10",
#        xlab="n_u",
#        auto.key =list(lines = TRUE)
# )
# R<-dataset(results)
# ggplot(data=,aes(Sizes,SL)) + geom_line() +geom_line(aes(y=))
# 
# 
# 
# plot(NA, ylim = c(0, max(t(apply(results,1,colMeans))[,1:2])))
# lines(t(apply(results,1,colMeans))[,1])
# lines(t(apply(results,1,colMeans))[,2])
# 
# for (i in seq_along(dis)){
#   subdat <- subset(dat, d == dis[i])
#   with(subdat, lines(F,T, col = linecols[i]))
# }
# legend("bottomright", legend=dis, fill=linecols)
# 
# #This should output zero:
# table(list(logl=results[,6]<results[,5],error=results[,4]<results[,3]))
# 
# 
# print("Percentage of datasets where classification error of constrained approach is lower:")
# print(mean(results[!apply(is.infinite(results), 1, any),1]<results[!apply(is.infinite(results), 1,any),2]))
# print("Percentage of datasets where likelihood on the test set constrained approach is better (lower:)")
# print(mean(results[!apply(is.infinite(results), 1, any),3]<results[!apply(is.infinite(results), 1,any),4]))