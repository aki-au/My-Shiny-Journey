#install.packages("psych",repos="https://personality-project.org/r",type="source")
library(psych)
data("iris")
print(str(iris))
iris_filtered<- subset(iris, iris$Species!="virginica")
iris_filtered2<- subset(iris, iris$Species!="setosa")
iris_filtered3<- subset(iris, iris$Species!="versicolor")
iris_filtered$Species= iris_filtered$Species[,drop=TRUE]
iris_filtered2$Species= iris_filtered2$Species[,drop=TRUE]
iris_filtered3$Species= iris_filtered3$Species[,drop=TRUE]
boxplot(iris_filtered$Sepal.Width~iris_filtered$Species,data=iris_filtered, main="Sepal Width Data",
                xlab="Species Name", ylab="Width")
boxplot(iris_filtered2$Sepal.Width~iris_filtered2$Species,data=iris_filtered2, main="Sepal Width Data",
        xlab="Species Name", ylab="Width")
boxplot(iris_filtered3$Sepal.Width~iris_filtered3$Species,data=iris_filtered3, main="Sepal Width Data",
        xlab="Species Name", ylab="Width")
stripchart(iris_filtered$Sepal.Width~iris_filtered$Species,method="jitter",jitter=.05,vertical=T,add=T) 
stripchart(iris_filtered2$Sepal.Width~iris_filtered2$Species,method="jitter",jitter=.05,vertical=T,add=T) 
stripchart(iris_filtered3$Sepal.Width~iris_filtered3$Species,method="jitter",jitter=.05,vertical=T,add=T)
par(mfrow=c(1,2))
hist(iris_filtered$Sepal.Width[iris_filtered$Species=="setosa"], main = "Sepal Width Data", xlab = "Setosa", ylab = "Width" )
hist(iris_filtered$Sepal.Width[iris_filtered$Species=="versicolor"],main = "Sepal Width Data",xlab="Versicolor", ylab = "Width" )
par(mfrow=c(1,2))
hist(iris_filtered2$Sepal.Width[iris_filtered2$Species=="versicolor"], main = "Sepal Width Data", xlab = "Versicolor", ylab = "Width" )
hist(iris_filtered2$Sepal.Width[iris_filtered2$Species=="virginica"],main = "Sepal Width Data",xlab="Virginica", ylab = "Width" )
par(mfrow=c(1,2))
hist(iris_filtered3$Sepal.Width[iris_filtered3$Species=="setosa"], main = "Sepal Width Data", xlab = "Setosa", ylab = "Width" )
hist(iris_filtered3$Sepal.Width[iris_filtered3$Species=="virginica"],main = "Sepal Width Data",xlab="Virginica", ylab = "Width" )
res.ftest <- var.test(iris_filtered$Sepal.Width ~ iris_filtered$Species, data = iris_filtered)
res.ftest1 <- var.test(iris_filtered2$Sepal.Width ~ iris_filtered2$Species, data = iris_filtered2)
res.ftest2 <- var.test(iris_filtered3$Sepal.Width ~ iris_filtered3$Species, data = iris_filtered3)
