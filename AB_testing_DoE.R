### A/B Testing Examples 2
# Rupert Thomas, July 2016

### Design of Experiements approach for A/B Testing
# Calculate the power (or other variable, such as required size) for an experiment
# to identify a change in a categorical variable, such as click through rate

## This is a very relevant reference:
# https://signalvnoise.com/posts/3004-ab-testing-tech-note-determining-sample-size
# as is:
# http://onlinelibrary.wiley.com/doi/10.1002/sim.3531/pdf


## Define key variables - set these to NULL to solve for that parameter

# Define sensitivity of the test (minimum amount of difference to be detected)
sensitivity = 0.1 	# percent

# Define baseline effect (the typical response observed )
p = 0.15

# Calculate the expected effect at the limit of sensitivitiy (how much difference
# in response between the two groups)
p1 = p
p2 = p * (1+sensitivity)

# Define the significance level, alpha, by which we say the result is statistically 
# significant
alpha = 0.05

# Define statistical power required (the probability of identifying a real effect)
power = 0.8

# Define the number of samples in each group
n = NULL

# Run the power proportions test
results = power.prop.test(p1=p1, p2=p2, sig.level = alpha,
								power = power,
								alternative = "two.sided",
								strict = FALSE)
results



## Explore the significance level solution space and plot a graph
# Define the size of the steps in the proportions (p1, p2)
step_size = 0.001
n_steps = 100 	# don't set too big, or will run out of space on the distribution

# Create a range of test points, either side of the results from above
p1_steps = seq(max(step_size,results$p1 - (step_size*n_steps/2)), min(1-step_size,results$p1 + (step_size*n_steps/2)), step_size)
p2_steps = seq(max(step_size,results$p2 - (step_size*n_steps/2)), min(1-step_size,results$p2 + (step_size*n_steps/2)), step_size)

# Set up arrays to store the results in
dummyArray = rep(NA,length(p1_steps)*length(p2_steps))
Significance.Level = dummyArray
P1 = dummyArray
P2 = dummyArray

i = 0

# Iterate through all the conditions, and get the significance level at each one
for (p1 in p1_steps){
	for (p2 in p2_steps){
		i = i+1
		
		output = power.prop.test(n=results$n, p1=p1, p2=p2, sig.level = NULL,
										power = results$power,
										alternative = "two.sided",
										strict = FALSE)
		
		Significance.Level[i] = output$sig.level
		P1[i] = p1
		P2[i] = p2
	}
}

# Find which points are below the significance level specified
Significant = Significance.Level < results$sig.level
output_df = data.frame(P1,P2,Significance.Level,Significant)

# Plot the results
library(ggplot2)
ggplot(data=output_df, aes(x=P1,y=P2,shape=Significant,colour=Significant)) + 
	geom_point(size=3)
