library(MASS)
library(ISLR)
### Simple linear regression
names(Boston)
?Boston
plot(medv~lstat,Boston)
fit1=lm(medv~lstat,data=Boston)
fit1
summary(fit1)
abline(fit1,col="red")
names(fit1)
confint(fit1)
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")

##############################
### Multiple linear regression
##############################
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)

fit3=lm(medv~.,Boston)
summary(fit3)
par(mfrow=c(2,2))# 2 by 2 layout
plot(fit3)

#1.Residual vs fitted - shows non -linear fit3 is a single vector(curve shows non-linearity)
#The first one is the residuals against the fitted values.
#The vector fitted values is just a single vector.
#So we can plot the residuals against that.
#And the reason we do that is we are looking for
#non-linearities.
#And we kind of know there's a non-linearity in this one.



#2.scale-location - absolute standardized residuals.
#One plots this to see, perhaps, if the variance is
#changing with the mean or the fit.
#In this case, it looks like there may be some

fit4=update(fit3,~.-age-indus)
summary(fit4)

##############################
### Nonlinear terms and Interactions
##############################

#Next we check interaction exists for the removed fields age an indus

#interaction between lstat and age.
#And that we do with a star, sort of like multiply.
#But in this formula language, it means an interaction.

#shows maineffect -  eventhoush age is not significant variable but : in below
#indicate it is interaction with lstat. usually rule is maineffect has 
#effect with the interaction field like age need to be included.



#main effects for each and the interaction.
#And the pure interaction is indicated by a colon.
#And while the main effect for age is not significant here,
#the interaction is somewhat significant.  




fit5=lm(medv~lstat*age,Boston)
summary(fit5)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 36.0885359  1.4698355  24.553  < 2e-16 ***
#  lstat       -1.3921168  0.1674555  -8.313 8.78e-16 ***
#  age         -0.0007209  0.0198792  -0.036   0.9711    
#lstat:age    0.0041560  0.0018518   2.244   0.0252 *  

#lstat:age is intercept indicate pure interaction ( relationship between lstat and age)



######################
#quadratic
######################

#The next thing we do here is we fit lstat.
#And we saw that there was a non-linear looking scatter
#plot between medv and lstat.
#And so here we explicitly put in a quadratic term.
#And there's two things going on here.
#The one is we've--
#  the quadratic we indicate by lstat power two.
#But power has a meaning in this formula language.
#And so if you want it to mean actually just raise lstat to
#the power of two, we protect it with
#this identity function I(lstat^2).
#So the formula language doesn't dig inside this
#identity function.
#So it literally puts in the square of lstat.



fit6=lm(medv~lstat +I(lstat^2),Boston); summary(fit6)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 42.862007   0.872084   49.15   <2e-16 ***
#  lstat       -2.332821   0.123803  -18.84   <2e-16 ***
#  I(lstat^2)   0.043547   0.003745   11.63   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#no surprise, both coefficients are strongly
#significant, the linear and the quadratic.

#Attach = attach Boston
#means that the named variables in Boston are
#available in our data space.


attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20)
# above quadratic fit from fit6
#And now we'll include the quadratic fit.
#Now, we can't use abline anymore, because that only
#works when you've got a straight line fit.
#Now, we need to somehow get the fitted values from our
# quadratic fit and include them in the plot.
# And so we do that with a points command.
# And the first argument is lstat itself.
# The second argument are the fitted values from fit6.
# That was our quadratic fit.
# So the fitted values are for each value of lstat, it's the
# fitted value from the model.
# So that's exactly what we want.
# And that will be plotted as a series of points.
# And we tell it the color should be red.
# And the pch, which is the plotting
# character, is to be 20.



fit7=lm(medv~poly(lstat,4))#fourth degree polynomial
#e.g For example, x3y1 degree 4. 3+1



points(lstat,fitted(fit7),col="blue",pch=20)


# It looks like the plotting character 20 is a
# little round ball.
# We'll have a little look in a moment and see a bit more
# about that.
# Before we do that, there's an easier way of fitting
# polynomials.
# And that's to use a poly function in R. So here we're
# going to fit medv as a polynomial of
# degree four in lstat.
# And we'll add that to the plot with color blue.
# And you can see that the fourth degree polynomial is
# getting a little bit too wiggly.

plot(1:20,1:20,pch=1:20,cex=2)
# It's starting to over-fit the data a little bit, especially,
# you can see in the right tail over here, it's maybe going
# after these rather isolated points here.
# So before we end, let's have a look at what plotting
# characters are available.
# So here's a simple way of seeing them all; plot one to
# 20 and plotting characters one to 20.
# We can see the whole lot.
# And there you see them.
# And so you have available a whole bunch of plotting
# characters, actually more than these.
# And number 20 was, sure enough, this one over here.
# And cx=2 means that we want to enlarge those plotting
# characters by 2.
# We want to double the size.
##############################
###Qualitative predictors
##############################
fix(Carseats) #So the command fix is a way of throwing up an editor in R.
names(Carseats)
summary(Carseats)
# And you can see, for quantitative variables, it
# gives you summaries like the mean, the median, and upper-
#   and lower-quantiles But for categorical or quantitative
# variables, it actually gives you a table of
# the distinct values.
# So ShelveLoc is a qualitative variable in this case.



fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)


##############################
###Writing R functions
##############################
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)
regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)

# when writing functions.
# Dot dot dot means these are unnamed arguments.
# But you're allowed to add extra arguments.
# And they'll get passed on exactly as they supplied
# wherever they are used inside the function.
# And here I have passed them on to the plot function.
# And what that does is it allows us to add extra
# arguments to the plot function via our regplot.
# And so now when we make the call, we call regplot price
# and sales, and we give it x lab and y lab.



