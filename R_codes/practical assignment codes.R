set.seed(20147866) # setting seed for replicability and debugging

#(1) Expected sample size estimation for BOP2 design with decision rules parameters (lamda and gamma) and samples sizes (n1, n2) under the null and alternative hypothesiS


BOP2_design <- function(N, lambda, gamma, n1, n2, theta) {
  # N is the Number of samples to be simulated
  Simulated_Samples <- rep(NA, N) # creating and empty vector to store simulated samples
  
  for (i in 1:N) {
    
    # Simulate theta from its prior, and then the stage 1 data conditional
    # on this theta.
    
    y1 <- rbinom(1, n1, theta) # stage 1 data conditioned on the null hypothesis
    
    # The posterior distribution parameters
    a1 <- 0.5 + y1
    b1 <- 0.5 + n1 - y1
    
    # Computing the probability of futility
    
    prob_futility <- pbeta(0.5, a1, b1)
    
    # Threshold to determine whether to proceed or not based on the decision rule parameters
    
    c1 <- 1 - lambda * (n1 / n2)^gamma
    
    # The final total sample size is stored in Simulated_Samples
    
    if (prob_futility > c1) {
      Simulated_Samples[i] <- n1
    } else {
      Simulated_Samples[i] <- n2
    }
  }
  return(mean(Simulated_Samples)) # return the expected sample size
}


##################################################################################

#Example: carrying out an example of the BOP2 design function

#Example (1) :  Estimating the sample size under the null hypothesis theta = 0.5
#N = 10000, lambda = 0.5, gamma = 0.8, n1 = 30, n2 = 70

BOP2_design(10^4, 0.5, 0.8, 30, 70, 0.5)


##################################################################################

#Example (2) : Estimating the sample size under the alternative hypothesis theta = 0.7
#N = 10000, lamda = 0.5, gamma = 0.8, n1 = 30, n2 = 70

BOP2_design(10^4, 0.5, 0.8, 30, 70, 0.7)


##################################################################################
#To calculate the time required to execute the code

library(tictoc)   #Library function required for timing the code

tic()	#To start timing the code

y <- replicate (20, BOP2_design(10^4, 0.5, 0.8, 30, 70, 0.7))    #To replicate the procedure 20 times to improve the accuracy 

toc()	# To stop timing the algorithm and return the time taken to execute the code



##################################################################################

