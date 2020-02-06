### This code is written for R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"
### Platform: x86_64-apple-darwin15.6.0 (64-bit)

# first, load the data found in this respoitory
# the first two lines are skipped because they are for your information abour the data
butter <- read.table("butterflyfish_growth.txt", skip=2, header=T)

# next, plot the data to inspect it and insure there are no anomalies
plot(butter$ages, butter$lengths, xlab = 'Ages (days)', ylab = 'Length (mm)')

# Now let's fit the Von Bertalanffy growth curve: length = Linf*(1-exp(-k*(age-t0)))
# where, Linf is the asymptotic size
# k is the growth coefficient
# t0 is the number used to calculate length at age zero
# The easiest way to fit this data to our model is to use the base R nonlinear least squares function: nls()
# This function is fine for simple data
# There are functions in other packages that use other alogorithms if nls() gets stuck
# for this function, we are going to give it some starting guesses for the parameters we want nls() to estimate
growth.curve <- nls(lengths ~ Linf * (1 - exp(-k * (ages - t0))),data = butter, start = list(Linf = 90, k = .001,t0 = -30))

# We can get confidence intervals for the parameters
confint(growth.curve, trace=T, level = 0.95)
# For this example, the profile confidence intervals
# can't be computed, presumably because the data
# are inadequate

# alternatively, you can ask for each parameter individually to see the confidence intervals
confint(growth.curve, trace=T, level = 0.95, parm = 'Linf')
confint(growth.curve, trace=T, level = 0.95, parm = 'k')
confint(growth.curve, trace=T, level = 0.95, parm = 't0')

# Now look at the summary statistics
summary(growth.curve)

# Get a predicted length for each age
for.plotting <- predict(growth.curve)
# We are going to sort the predicted values so that the plot makes sense, try to plot without sorting to see what happens
for.plotting <- for.plotting[order(for.plotting)]
sorted.butter = butter[order(butter$ages),]


# Now plot the data and overlay the predicted model fit
plot(butter$ages, butter$lengths, xlab = 'Ages (days)', ylab = 'Length (mm)')
lines(sorted.butter$ages, for.plotting, lty=2, col=2)

### Congrats you have fit a growth curve to the data


### another more fun example
# this example fits the Von Bertalanffy growth curve to my 6 month-old daughter Zora
# presumably she is not the best animal to fit this model to, but, hey lets have some fun

# load data
z <- read.csv('z_growth.csv')

# create a dataframe to use in our nls() function
baby <- data.frame(ages = z$days, lengths = z$length..cm.)

# fit the model to the data
growth.curve <- nls(lengths ~ Linf * (1 - exp(-k * (ages - t0))), data = baby, start = list(Linf = 90, k= .001, to = -30))

# how good is the fit
summary(growth.curve)

# create some dummmy data to predict age at length from model fit
preds <- data.frame(ages = seq(0, 365*21, length.out = 1000))
# predict age at length
preds$lengths <- predict(growth.curve, newdata = preds)

# plot it out
plot(z$days,z$length..cm., xlab = 'Age (days)', ylab = 'Length (cm)')
lines(preds$ages, preds$lengths, lty=2)

# now lets have a little more fun
plot(z$days,z$length..cm.,xlim=c(0,1000),ylim=c(40,100), xlab = 'Age (days)', ylab = 'Length (cm)')
lines(preds$ages,preds$lengths,lty=2)
# plot out the asymptotic size, which is the first coefficient in our model
# the coefficients() function extracts the coefficients used to to fit the 
abline(h=coefficients(growth.curve)[1],lty=3,col=2)

# how old will my daughter be when she reaches her asymptotic size and stops growing?
preds$ages[which((preds$lengths-coefficients(growth.curve)[1])==0)[1]]/365 
# dividing by 365 will give us the answer in years since the originial data was in days

# Finally, how big will she be when she stops growing?
coefficients(growth.curve)[1]
# remember the answer will be in centimeters
