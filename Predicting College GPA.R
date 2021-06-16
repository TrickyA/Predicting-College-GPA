#Bringing in the data
dat <- read.csv("GPA2.csv", header = T)

#Checking out the data
head(dat)
dim(dat)

#Importing ggplot2 package for visualizations
library(ggplot2)

#Distribution, shape, summary and standard deviation of college GPA column 
#(Our y variable)
hist(dat$colgpa,main = "College GPA", col="blue",xlim=c(1,4), breaks=15, xlab = "GPA")
boxplot(dat$colgpa,main = "College GPA", col="red", xlab = "GPA")

summary(dat$colgpa)

sd(dat$colgpa)


#Correlation matrix of the other variables to see if any...
#relationships exist
Matrix <- dat[,c(1,2,4,5,6)]

cor(Matrix)
#Round results to 2 decimal places
round(cor(Matrix),2)

#Regression to see which variables are important in the model

reg1 <- lm(colgpa ~., data = dat)
summary(reg1)

reg2 <- lm(colgpa ~ athlete+female, data = dat)
summary (reg2)

#According to these results, 
#A student's SAT score, athlete status, rank in high school graduating class, high school percentile position
#and gender status are significant in this regression model. 
#All of these variables have small p-values (<0.05) which indicates that they are important for the model.


#Analyzing the interaction terms
#High school percentile broken down by athlete and by female gender
library(ggplot2)

ggplot(dat, aes(x = hsperc, y = colgpa, color = as.factor(athlete))) + 
  geom_point() + geom_smooth(method = "lm") + scale_color_manual(values = c ("1" = "red","0" = "blue"))

ggplot(dat, aes(x = hsperc, y = colgpa, color = as.factor(female))) + 
  geom_point() + geom_smooth(method = "lm")

#Looking at these two scatterplots, it appears that only the interaction term hsperc*athlete is worth exploring and placing in the model. 
#This is because the lines on that plot are not parallel, have different slopes, and they intersect. 


#New regression model to predict college GPA (y) using the important predictor variables
#SAT score, athlete status, rank in high school graduating class, high school percentile position, gender status, interaction term
Model_1 <- lm(colgpa ~ SAT+ athlete+ hsrank+hsperc+female+hsperc*athlete, data=dat)
summary(Model_1)

#Round results to 3 decimal places
round(Model_1$coeff,3)

#Least Squares Line:
#ColGPA = 1.156 + 0.002SAT - 0.02athlete – 0.001hsrank – 0.011hsperc + 0.152female + 0.006athlete*hsperc.

#Trying out a prediction
#Let's say a male student received an 1390 on his SAT
#Let's say he was ranked 8th in his high school class
#Let's say he's not an athlete
#And let's say this rank is in the 6th percentile.

newdat <- data.frame(SAT = 1390, female = 0, hsrank = 8, hsperc = 6, athlete = 0)
round(predict(Model_1, newdat, interval = "predict"),3)

#According to the model, this male student is predicted to have
#A college GPA of 3.358
