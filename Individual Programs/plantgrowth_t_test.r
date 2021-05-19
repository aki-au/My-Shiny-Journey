#install.packages("psych",repos="https://personality-project.org/r",type="source")
library(psych)
data("PlantGrowth")
print(str(PlantGrowth))
plant <- split(PlantGrowth, PlantGrowth$group)
ctrlplant<- plant[1]
trt1plant<-plant[2]
trt2plant<-plant[3]
plant.data<- data.frame(ctrlplant, trt1plant, trt2plant)
df = subset(plant.data, select = -c(ctrl.group,trt1.group,trt2.group) )
plantctrlvstrt1 = subset(df, select = -c(trt2.weight) )
plantctrlvstrt2 = subset(df, select = -c(trt1.weight) )
planttrt1vstrt2 =  subset(df, select = -c(ctrl.weight) )
boxplot(plantctrlvstrt1,main="Control vs Treatment 1",ylab="Weight of Dried Plant") 
stripchart(plantctrlvstrt1,method="jitter",jitter=.05,vertical=T,add=T) 
multi.hist(plantctrlvstrt1)
# hist(plantctrlvstrt1$ctrl.weight, main="Histogram of Control Weight", xlab="Control Weight")
# hist(plantctrlvstrt1$trt1.weight, main="Histogram of Treatment 1 Weight", xlab="Treatment 1 Weight")
boxplot(plantctrlvstrt2,main="Control vs Treatment 2",ylab="Weight of Dried Plant") 
stripchart(plantctrlvstrt2,method="jitter",jitter=.05,vertical=T,add=T) 
# hist(plantctrlvstrt2$ctrl.weight, main="Histogram of Control Weight", xlab="Control Weight")
# hist(plantctrlvstrt2$trt2.weight, main="Histogram of Treatment 2 Weight", xlab="Treatment 2 Weight")
multi.hist(plantctrlvstrt2)
with(plantctrlvstrt1, t.test(plantctrlvstrt1$ctrl.weight, plantctrlvstrt1$trt1.weight,equal.var=TRUE) ) 
with(plantctrlvstrt2, t.test(plantctrlvstrt2$ctrl.weight, plantctrlvstrt2$trt2.weight,equal.var=TRUE) ) 

boxplot(planttrt1vstrt2,main="Control vs Treatment 2",ylab="Weight of Dried Plant") 
stripchart(planttrt1vstrt2,method="jitter",jitter=.05,vertical=T,add=T) 
multi.hist(planttrt1vstrt2)
with(planttrt1vstrt2, t.test(planttrt1vstrt2$trt1.weight, planttrt1vstrt2$trt2.weight,equal.var=TRUE) ) 





