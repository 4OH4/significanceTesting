# A/B Testing Examples 1
# Rupert Thomas, July 2016

## Simple 2-type A/B Test using Fisher's Exact test and Chi-square

# This is a very relevant reference:
# http://onlinelibrary.wiley.com/doi/10.1002/sim.3531/pdf
# Actually advises against using Fisher's Exact, if favour of 
# Chi-square or alternatives

# Number of impressions/observations
nA = 100
nB = 100
n = nA + nB

# Fraction of positive results (the 'Click Through Rate')
ctrA = 0.50
ctrB = 0.60

# Get total positive results into a 2x2 matrix
clickTotals = matrix(c(nA*ctrA, nB*ctrB, nA*(1-ctrA), nB*(1-ctrB)), 
										 nrow=2, byrow=T, 
										 dimnames=list(Clicked=c("Yes","No"),Shown=c("A","B")))

# Conduct Fisher Exact test to determine independence of rows and columns
results_Fisher = fisher.test(clickTotals, alternative="two.sided") # run with default two-tailed
print(results_Fisher)

# As the number of impressions is big enough, Chi-Square will give a very similar result
results_ChiSq = chisq.test(clickTotals, correct=T) 	# run with default: Yate's correction
print(results_ChiSq)
