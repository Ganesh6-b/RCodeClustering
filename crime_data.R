setwd("F://R//files")

crimedata <- read.csv("crime_data.csv")

View(crimedata)
table(crimedata$X)
#perform normalization
#hierarchial clustering
n_data <- scale(crimedata[,-1])

#distance_matrix

d <- dist(n_data, method = "euclidean")
d

#model building

model1 <- hclust(d, method = "single")
summary(model1)

#dendogram

 plot(model1, hang = -1)

#cuttree

groups <- cutree(model1, k = 5)
groups

rect.hclust(model1, k = 5, border = "red")
class(model1)

#convert to matrix

finmodel <- as.matrix(groups)
table(finmodel)

final <- data.frame(crimedata, finmodel)
View(final)

#move to first column

library(data.table)

setcolorder(final, c("finmodel"))
?setcolorder
View(final)


#Kmeans clustering

wss = (nrow(n_data)-1)*sum(apply(n_data,2,var))
for(i in 2:4) wss[i] = sum(kmeans(n_data, centers = i)$withinss)
plot(1:4, wss, type='b')


km <- kmeans(d, 2)
summary(km)

install.packages("animation")
library(animation)

km <- kmeans.ani(d,2)

