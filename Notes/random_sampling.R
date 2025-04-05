library(tidyverse)
library(mosaic)

# let's load our synthetic "population" of voters in synthetic_voters.csv

####
# random sampling
####

synthetic_voters = read.csv("/Users/pranavbelligundu/Documents/GitHub/SDS-315/Notes/synthetic_voters.csv")

# Here's our "population":
# table of presidential preference: counts...
xtabs(~pres, data=synthetic_voters)

# ...and proportions
xtabs(~pres, data=synthetic_voters) %>%
  prop.table

# a nice shortcut for the overall proportion for pres
# notice it omits Vance: that's because Shapiro is first alphabetically
prop(~pres, data=synthetic_voters)

# let's store that computation in a variable called theta_shapiro
theta_shapiro = prop(~pres, data=synthetic_voters)


###
# Simple random samples
###

# Now let's take a sample of size N=10 from our population
N = 10

# Here's a true random sample of 10 voters
my_poll = sample(synthetic_voters, size=N)
my_poll

# let's look at the proportion of Democrat vs. Republican voters in our sample
prop(~pres, data=my_poll)

# let's repeat the sampling process 5 times
# notice how every sample yields a different proportion
# that's sampling error!
do(5)*prop(~pres, data=sample(synthetic_voters, size=N))

# Let's now repeat this 1000 times and see the result
# as a histogram
sim1 = do(1000)*prop(~pres, data=sample(synthetic_voters, size=N))
head(sim1) # 1000 rows of a single column called prop_Shapiro

# let's make a histogram
# here we set the bin width to every 0.01
# and we zoom out the x limits to encompass everything from 0 to 1
# we also add a vertical line at the true population value
ggplot(sim1) + 
  geom_histogram(aes(x=prop_Shapiro), binwidth=0.01)+ 
  xlim(0,1) +
  geom_vline(xintercept = theta_shapiro, color='blue')

# This histogram is centered on the true value.
# no _systematic_ departures from the true, i.e. no sampling bias
# but it has some spread around that value (nonzero sampling variability).

# let's mess around with the sample size:
# try N = 50, 250, 1000

N = 1000
sim2 = do(10000)*prop(~pres, data=sample(synthetic_voters, size=N))

# the sampling distribution of our estimate
ggplot(sim2) + 
  geom_histogram(aes(x=prop_Shapiro), binwidth=0.01)+ 
  xlim(0,1) +
  geom_vline(xintercept = theta_shapiro, color='blue')

# Remember: "standard error" is a formal math term -- it always means
#   the standard deviation of the sampling distribution.
# Margin of error is a colloquial term with no fixed mathematical meaning.
# the "margin of error" often comes from going out 1 or 2 standard errors
# from the sample estimate.
sim2 %>%
  summarize(std_err = sd(prop_Shapiro))

# or, summary shortcut
sd(~prop_Shapiro, data=sim2)

# this standard error is a property of the sampling procedure,
# not a specific sample.
