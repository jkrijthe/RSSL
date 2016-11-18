library(RSSL)
library(ggplot2)
library(dplyr)

set.seed(1)
df_circles <- generateTwoCircles(400,noise=0.1) %>% 
  add_missinglabels_mar(Class~.,0.99)

# Visualize the problem
df_circles %>% 
  ggplot(aes(x=X1,y=X2,color=Class)) +
  geom_point() + 
  coord_equal()

# Visualize the solution
class_grf <- GRFClassifier(Class~.,df_circles,
                           adjacency="heat",
                           adjacency_sigma = 0.1)
df_circles %>%
  filter(is.na(Class)) %>% 
  mutate(Responsibility=responsibilities(class_grf)[,1]) %>% 
  ggplot(aes(x=X1,y=X2,color=Responsibility)) +
  geom_point() + 
  coord_equal()

# Generate problem
df_para <- generateParallelPlanes()
df_para$Class <- NA
df_para$Class[1] <- "a"
df_para$Class[101] <- "b"
df_para$Class[201] <- "c"
df_para$Class <- factor(df_para$Class)

# Visualize problem
df_para %>% 
  ggplot(aes(x=x,y=y,color=Class)) +
  geom_point() + 
  coord_equal()

# Estimate GRF classifier with knn adjacency matrix (default)
class_grf <- GRFClassifier(Class~.,df_para)

df_para %>%
  filter(is.na(Class)) %>% 
  mutate(Assignment=factor(apply(responsibilities(class_grf),1,which.max))) %>% 
  ggplot(aes(x=x,y=y,color=Assignment)) +
  geom_point()
