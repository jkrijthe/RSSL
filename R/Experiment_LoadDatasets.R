# Experiments_LoadDatasets
library(mlbench)
# UCI Benchmarks

datasets<-list()
modelforms<-list()
#datasets[["Kr-vs-Kp"]] <- read.csv("~/Data/UCI Originals/kr-vs-kp.data")
#modelforms[["Kr-vs-Kp"]] <- formula("factor(won)~.")

datasets[["Normals"]]<-as.data.frame(mlbench.2dnormals(1000,cl=2,r=1))
modelforms[["Normals"]] <- formula("factor(classes)~.")

datasets[["Haberman"]] <- read.csv("~/Data/UCI Originals/haberman.data")
modelforms[["Haberman"]] <- formula("factor(X1.1)~.")
# 
datasets[["Ionosphere"]] <- read.arff("~/Data/UCI Arff/UCI/ionosphere.arff")[,-c(1,2)]
modelforms[["Ionosphere"]] <- formula("factor(class)~.")

datasets[["Parkinsons"]] <- read.csv("~/Data/UCI Originals/parkinsons.data")[,-c(1:4)]
modelforms[["Parkinsons"]]<-formula("factor(status)~.")

datasets[["Pima"]] <- read.arff("~/Data/UCI Arff/UCI/diabetes.arff")
modelforms[["Pima"]] <- formula("factor(class)~.")

datasets[["Sonar"]] <- read.arff("~/Data/UCI Arff/UCI/sonar.arff")
modelforms[["Sonar"]]  <- formula("factor(Class)~.")

datasets[["SPECT"]] <- data.frame(rbind(data.matrix(read.csv("~/Data/UCI Originals/SPECT.train")),data.matrix(read.csv("~/Data/UCI Originals/SPECT.test"))))
modelforms[["SPECT"]] <- formula("factor(X1)~.")

datasets[["SPECTF"]] <- data.frame(rbind(data.matrix(read.csv("~/Data/UCI Originals/SPECTF.train")),data.matrix(read.csv("~/Data/UCI Originals/SPECTF.test"))))
modelforms[["SPECTF"]]  <- formula("factor(X1)~.")

datasets[["Transfusion"]] <- read.csv("~/Data/UCI Originals/transfusion.data")[,-c(3)] # Monetary reward is a linear combination of the number of visits
modelforms[["Transfusion"]]  <- formula("factor(whether.he.she.donated.blood.in.March.2007)~.")

datasets[["WDBC"]] <- read.csv("~/Data/UCI Originals/wdbc.data")[,-c(3)]
modelforms[["WDBC"]]  <- formula("factor(M)~.")

# Chapelle Bechmark datasets

dnames<-c("Digit1","USPS","COIL2","BCI","g241c","COIL","g241n")
for (d in c(1:5,7)) { 
  X<-read.table(paste("Data/SSL,set=",d,",X.tab",sep=""))
  y<-read.table(paste("Data/SSL,set=",d,",y.tab",sep=""))
  D<-data.frame(X,y=as.factor(y$V1))
  datasets[[dnames[d]]]<-D
  modelforms[[dnames[d]]]<-formula("y~.")
}