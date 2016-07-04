
#NOTE: 
#global rule is  for regression the distance between decision
#boundary and data(predictor) should be small for good prediction.
#For  Classification the  distance between decision boundary 
#and predictors should be large for good prediction.since more decision
#boundary more space for data to fit in classification correctly inside 
#boundary.
#for the classification the no. of decision boundary is no. of classification
#response expected -1 if we are to classify three classifications(good,bad,average)
#then no of decision boundary is 2 


require(ISLR)
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket,col=Smarket$Direction)

######################
# Logistic regression
######################

# logistic regression
# model instead of one of the many other models that can be
# fit to the GLM.
# So we execute that fit.
# That's two lines there.
# And then we do a summary, and the summary tells you some
# things about the fit.
# And in this case, it's giving you p values on each of the
# coefficients.
# It estimated coefficients, their standard errors, the
# z-score, and the p value.
# And it seems like none of the coefficients
# are significant here.
# Again, not a big surprise for these kinds of data.
# It doesn't necessarily mean it won't be able to make any kind
# of reasonable predictions.
# It just means that possibly these variables are very
# correlated.
# Actually, the plot doesn't suggest that.
# Anyway, none are significant.
# And it gives the null deviance, which is the
# deviance just for the mean.
# So that's the log likelihood if you just use the mean
# model, and then the deviance for the model with all the
# predictors in, that's the residual deviance.
# And there was a very modest change in deviance.
# It looks like four units on how many degrees of freedom.
# We got six degrees of freedom difference in degrees of
# freedom there.
# OK, so we can make predictions from the fitted model.
# And so we assign to glm.probs the predict of glm.fit, and we
# tell it type equals response.
# So this will make predictions on the training data that we
# use to fit the model.
# And it gives you a vector of fitted probabilities.
# We can look at the first five, and we see that they're very
# close to 50%, which is, again, not too surprising.
# We don't expect to get strong predictions in this case.
# So this is a prediction of whether the market's going to
# be up or down based on the lags and the other predictors.
# We can turn those probabilities into
# classifications by thresholding at 0.5.
# And so we do that by using the if/else command.
# So if/else takes effect, in this case, glm.probs, in fact,
# a vector of logicals.
# So glm.probs bigger than 0.5.
# So that'll be a vector of trues and falses.
# And then if/else says that, element by element, if it's
# true, you're going to call it up.
# Otherwise, you're going to call it down.
# And so that does that for us.
# And now we're going to look at our performance.
# And it's convenient to actually attach the data frame
# market so that the variables are available by
# name, which we do.
# And now we can make a table of glm.pred, which is our ups and
# downs from our prediction, against the true direction.
# So we do that.
# And we get a table, and we see there's lots of elements on
# the off diagonals.
# On the diagonals is where we do correct classification, and
# on the off diagonals is where we make mistakes.
# And we see there's quite a lot of those.
# And we can actually get our mean classification
# performance.
# So that's cases where glm.pred is equal to the direction.
# 

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial)


summary(glm.fit)
glm.probs=predict(glm.fit,type="response") 

##manually calculate z-score
#mean(Smarket$Lag3)##0.001716 
#variance<-var(Smarket$Lag3)# 1.296644
#sdresult<-sd(Smarket$Lag3)*sqrt((length(Smarket$Lag3)-1)/(length(Smarket$Lag3)))
 
#sd(Smarket$Lag3)##0.011085- 0.001716/1.138247
#z=(x-mean)/sd ==0.008

glm.probs[1:5]


glm.pred=ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred==Direction)#0.5216

#And we just take the mean of those, so it'll give you a
# proportion, in this case, 0.52.
# So on the training data, we do slightly better than chance.
# Well, we may have overfit on the training data.
# So what we're going to do now is divide our data up into a
# training and test set.
# So what we'll do is we'll make a vector of logicals.
# And what it is is train is equal to year less than 2005.
# For all those observations for which year is less than 2005,
# we'll get a true.
# Otherwise, we'll get a false.

############################
# Make training and test set
############################

# And now we refit our glm.fit, except we say
# subset equals train.
# And so it will use any those observations for
# which train is true.
# So now that means we fit just to the data in
# years less than 2005.

train = Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial, subset=train)

# And now, when we come to predict, we're going to
# predict on the remaining data, which is
# years 2005 or greater.
# And so we use the predict function again.
# And for the new data, we give it Smarket, but
# index by not trained.
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") 


# So that's going to be--
#   not trained is going to be true if year is 2005 or more.
# And we tell it type equals response, so we actually want
# to predict the probabilities in this case.
# And again, we make this if/else, make
# this up/down variable.
glm.pred=ifelse(glm.probs >0.5,"Up","Down")


# And let's make a subset, a new variable, direction.2005,
# again, for the test data, which is the response
# variable, direction, which is just for our test data.
# In other words, not trained.
# So we call it direction.2005.
Direction.2005=Smarket$Direction[!train]

# And now we make a table.
# So now this is on test data.
table(glm.pred,Direction.2005)

# A smaller subset of the data is test.
# And we compute our mean again, and it's 0.48.
mean(glm.pred==Direction.2005)# 0.4801587

# So now we actually get slightly less than 50%.
# So we're doing worse than the null rate, which is 50%.
# So what are we going to do?
# Well, we might be overfitting.

#####################
#Fit smaller model
#####################
# And that's why we're doing worse on the test data.
# So now we're going to fit a smaller model.
# So we're going to just use lag1 and lag2 and leave out
# all the other variables.










glm.fit=glm(Direction~Lag1+Lag2,
            data=Smarket,family=binomial, subset=train)


# And so here we do that.
# The rest of it calls the same.
# And then we run through our commands again, we compute our
# table, and here we get a correct classification of just
# about 56%, which is not too bad at all.
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)#0.5595238
106/(76+106)
# And so using the smaller model, it appears to have done
# better here.
# And if we do a summary of that guy, let's see if anything
# became significant by using the smaller model, given that
# it gave us better predictions.
# And of course, that's what happens when you try and do
# Well, nothing became significant, but at least the
# prediction of performance appeared to have increased.
# So that's fitting logistic regression models in R using
# the GLM function and family equals binomial.

###############################
## Linear Discriminant Analysis
###############################
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket, subset=Year<2005)
lda.fit
plot(lda.fit)
Smarket.2005=subset(Smarket,Year==2005)
lda.pred=predict(lda.fit,Smarket.2005)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)#0.5595238

#######################
## K-Nearest Neighbors
#######################
library(class)
?knn
attach(Smarket)
Xlag=cbind(Lag1,Lag2)
train=Year<2005
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])# 0.5
