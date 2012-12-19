setClass("TransductiveSVM", representation(),prototype(name="Transductive Support Vector Machine"), contains="Classifier")

TransductiveSVM<-function() {
  
}

setMethod("predict","TransductiveSVM")