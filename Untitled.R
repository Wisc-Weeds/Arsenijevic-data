



library(tidyverse)
library(drc)


data <- read_csv("Canopeo.csv")
data$treatment <- as.factor(data$treatment)
data$row  <- as.factor(data$row)
glimpse(data)



###
model <- drm(cavg ~ doy, treatment, fct=l3(fixed = c(NA,NA,NA), names = c("slope", "upper", "ec50")), data=data)
summary(model)
plot(model)
dataset <- ED(model, c(50), type="absolute", interval = "delta")
dataset <- as.data.frame(dataset)

#write_csv(dataset, path="new.csv")
new <- read_csv("new.csv")
new$trt  <- as.factor(new$trt)

ggplot(new, aes(x=trt, y=Estimate, color=trt)) + geom_point() +
  geom_errorbar(aes(ymin=Lower, ymax=Upper))



row <- drm(cavg ~ doy, row, fct=l3(fixed = c(NA,NA,NA), names = c("slope", "upper", "ec50")), data=data)
summary(row)
ED(row, c(50,90))
EDcomp(row, c(50,50))
plot(row)



till<- drm(cavg ~ doy, tillage, fct=l3(fixed = c(NA,NA,NA), names = c("slope", "upper", "ec50")), data=data)
summary(till)
ED(till, c(50,90))
EDcomp(till, c(50,50))
plot(till)



Herb<- drm(cavg ~ doy, herbicide, fct=l3(fixed = c(NA,NA,NA), names = c("slope", "upper", "ec50")), data=data)
summary(Herb)
ED(Herb, c(50,90))
EDcomp(Herb, c(50,50))
plot(Herb)


Pl <- drm(cavg ~ doy, planting, fct=l3(fixed = c(NA,NA,NA), names = c("slope", "upper", "ec50")), data=data)
summary(Pl)
ED(Pl, c(50,90))
EDcomp(Pl, c(50,50))
plot(Pl)



Test <- drm(cavg ~ doy, test, fct=l3(fixed = c(NA,NA,NA), names = c("slope", "upper", "ec50")), data=data)
summary(Test)
ED(Test, c(50,90))
EDcomp(Test, c(90,90))
plot(Test)


