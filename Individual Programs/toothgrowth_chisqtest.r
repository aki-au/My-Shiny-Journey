library(vcd)
library(ggplot2)
ToothGrowth$size <- ifelse(ToothGrowth$len < median(ToothGrowth$len),"small", "big" )
print(str(ToothGrowth))
dosevssize=table(ToothGrowth$dose, ToothGrowth$size)
dosevssupp=table(ToothGrowth$dose, ToothGrowth$supp)
suppvssize=table(ToothGrowth$supp, ToothGrowth$size)
ggplot(ToothGrowth) +
aes(x = dose, fill = size) +
geom_bar()
ggplot(ToothGrowth) +
aes(x = dose, fill = supp) +
geom_bar()
ggplot(ToothGrowth) +
aes(x = supp, fill = size) +
geom_bar()
csqdosevssize=chisq.test(dosevssize)
mosaic(~ dose + size,
direction = c("v", "h"),
data = ToothGrowth,
shade = TRUE
 )
print(csqdosevssize$expected)
print(dosevssize)
print("We reject the null hypothesis ")
csqdosevssupp=chisq.test(dosevssupp)
mosaic(~ dose + supp,
       direction = c("v", "h"),
       data = ToothGrowth,
       shade = TRUE
)
print(csqdosevssupp$expected)
print(dosevssupp)
print("We accept the null hypothesis ")
csqsuppvssize=chisq.test(suppvssize)
mosaic(~ supp + size,
       direction = c("v", "h"),
       data = ToothGrowth,
       shade = TRUE
)
print(csqsuppvssize$expected)
print(suppvssize)
print("We reject the null hypothesis ")


