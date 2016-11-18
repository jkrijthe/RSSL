library(RSSL)
library(ggplot2)
library(dplyr)

set.seed(1)
df_orig <- generateSlicedCookie(200,expected=TRUE)
df <- df_orig %>% 
  add_missinglabels_mar(Class~.,0.98)

classifiers <- list("Well"=WellSVM(Class~.,df,C1 = 1, C2=0.1, 
                                   gamma = 0,x_center=TRUE,scale=TRUE),
                    "Sup"=SVM(Class~.,df,C=1,x_center=TRUE,scale=TRUE))

df %>% 
  ggplot(aes(x=X1,y=X2,color=Class)) +
  geom_point() +
  coord_equal() +
  stat_classifier(aes(color=..classifier..),
                  classifiers = classifiers)
