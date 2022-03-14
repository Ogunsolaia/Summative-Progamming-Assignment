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

#Finding the optimal value for lambda and gamma
# I commented this out because it takes time to execute, the improve/exact method was used
    #library(tictoc)   #Library function required for timing the code
    #tic()	#To start timing the code
    #eval <- expand.grid(lambda = seq(0, 1, 0.01),gamma = seq(0, 1, 0.01))		#The different values of lamda and gamma we want to search
                    

    #expected_sample_size <- rep(NULL, nrow(eval))	# The expected sample size is store in a vector for each pair in the grid,  
    #for(i in 1: nrow(eval)) {
    #expected_sample_size[i] <- BOP2_design(10^4, eval[i, 1], eval[i, 2], n1 = 30, n2 = 70, theta = 0.5)[1]
    #}
    #toc()    # To stop timing the algorithm and return the time taken to execute the code


#######################################################################

#Improving the efficiency of the code in computing the effective sample size using exact method

   prob_y1 <- function(y1, n1, theta){
   #calculating the probability of observing y1 responses in n1 trials 
   dbinom(y1,n1,theta)  #Binomial distribution conditioned on theta
   }

   BOP2_design_improve <- function(lambda, gamma, n1, n2, theta) {
  
   y1_s<- 0: n1  # vectors of possible stage 1 outcomes
  
  c1<- 1 - lambda * (n1 / n2)^gamma   #Go or no go threshold based on decision rule
  
  quits<- pbeta(0.5, y1_s + 0.5,  n1 - y1_s + 0.5)  > c1  # stage 1 data conditioned on the null hypothesis
  
  y1_probability<- prob_y1(y1_s, n1, theta)    # To calculate probability of each outcome
  
  sum(n1 *quits *  y1_probability  +  n2 * (!quits) * y1_probability)
  }
  
  #######################################################################
  #Estimating the sample size using the improved function (exact method instead of Monte Carlo simulation)
  #lambda = 0.5, gamma = 0.8,  n1 = 30, n2 = 70, theta = 0.5
  
  BOP2_design_improve(0.5, 0.8, 30, 70, 0.5)
  
  #######################################################################

  #To calculate the time required to execute the algorithm
  
  library(tictoc)   #Library function require for timing the algorithm
  
  tic()	#To start timing the algorithm	
  
  y <- replicate (20, BOP2_design_improve(0.5, 0.8, 30, 70, 0.5))    #To replicate the procedure 20 time to improve the accuracy 
  
  toc()	# To stop timing the algorithm and return the time taken to execute the algorithm
  
  #The result obtained is the same as using the Monte Carlo method but the time for its execution has seriously reduced 
  #In other words, the efficiency has been improved.
  
  
  
  
  #######################################################################
  
  #Finding the optimal value for lambda and gamma using the improved function
  
  library(tictoc)   #Library function required for timing the code
  
  tic()	#To start timing the code
  
  eval <- expand.grid(lambda = seq(0, 1, 0.01),		#The different values of lambda and gamma we want to search
                      gamma = seq(0, 1, 0.01))
  
  
  expected_sample_size <- rep(NULL, nrow(eval))	# The expected sample size is store in a vector for each pair in the grid,  
  for(i in 1: nrow(eval)) {
    expected_sample_size[i] <- BOP2_design_improve(eval[i, 1], eval[i, 2], n1 = 30, n2 = 70, 0.5)[1]
  }
  toc()    # To stop timing the algorithm and return the time taken to execute the code
  
  eval  # Listing the various combinations of lambda and gamma
  expected_sample_size  #obtaining the respective effective sample sizes for the combinations of lambda and gamma
  
  
  ########################################################################
  #Unit testing 
  # To test whether the probability function is correct even when the nature of the distribution changes
  #COMMENT: I added the code for testing whether the probability function is correct even 
  #when the nature of the distribution changes. Normally, the prob_y1 is a standard binomial 
  #distribution which will always sum to 1, I tested my code by writing the binomial
  #distribution using real formula instead of inbuilt function so that when the distribution 
  #is change one can check whether it sum to one or not.
    
    prob_y1 <- function(y1, n1, theta) {
    #calculating the probability of observing y1 responses in n1 trials 
    #I rewrite the prob_y1 function using formula instead of standard dbinom(y1, n1, theta)
    
    choose(n1, y1) * theta^y1 * (1-theta)^(n1-y1) #Binomial distribution with probaility of theta
    }
    
    testing_prob_y1 <- function() {
    n1 <- 30			            #Stage 1 sample size
    theta <- seq(0,1, 0.1)    # To state the values of theta which is form 0 to 1 since it is a probability
    sum_prob<-rep(NA, length(theta)) # Vector to store the sum of probabilities from 0:n1 for each theta
    for (i in 1:length(theta)){
      sum_prob[i]<-sum(prob_y1(0:n1, n1, theta[i]))	#to sum all probability from 0 to n1 for each theta
    }
    return(all.equal(sum_prob[i], 1))		#To return TRUE if the function sum up to 1 and FALSE otherwise for each theta
  }
  
  #Illustration:
  testing_prob_y1()
  
  
  
  ########################################################################  
  #Calculating Type I error for BOP2 Design
  
    TypeI_error <- function(N, lambda, gamma, n1, n2, theta){
    # N is the Number of samples to be simulated
    
    # creating and empty vector for cases where we reject the null hypothesis in stage 2, conditioned on stage 1 being passed already
    
    samples1<-rep(NA, N)
    
    for (i in 1:N) {
      
    y1<- rbinom(1, n1, theta) # stage 1 data conditioned on the null hypothesis
      
    # The posterior distribution parameters
    
    a1 <- 0.5 + y1
    
    b1 <- 0.5 + n1 - y1
      
    prob_futility <- pbeta(0.5, a1, b1)  # Computing the probability of futility
      
    c1 <- 1 - lambda * (n1 / n2)^gamma  # Threshold to determine whether to proceed or not based on the decision rule parameters
      
    proceed<- prob_futility < c1  # Condition for moving to stage 2
      
    if (proceed == TRUE){
        
    y2<- rbinom(1, n2, theta) 		# stage 2 data conditioned on the null hypothesis
        
    #The posterior distribution parameters
    a2 <- 0.5 + y2
    
    b2 <- 0.5 + n2 - y2
        
    prob_futility2 <- pbeta(0.5, a2, b2)	          # Computing the probability of futility at stage 2
        
    c2 <- 1 - lambda * (n2/ n2)^gamma               # Threshold to determine whether to proceed or not based on the decision rule parameters
        
    samples2[i]<-prob_futility2 < c2
        
    } else {
        
    samples2[i]<-0
    
    }
    }
    return (mean(samples2)) #Type 1 error is returned 
    }
  

 
  ########################################################################
  #This cope is mainly and only for plotting the graphs of expected sample sizes under different null hyoothesis
  # It is the same as the code for obtaining the expected sample size using exact method written above as an improvement instead the Monte Carlo method
  # I only included a function so that I can vary lambda and gamma for specified values of theta
  # It is just for convenience 
  
    sample_size_graph<-function(lambda, gamma){
    thetas<- expand.grid(theta = seq(0.1, 0.9, 0.1))		#The different values of lamda and gamma we want to search
    
    expected_sample_size <- rep(NULL, nrow(thetas))	# The expected sample size is store in a vector for each pair in the grid,  
    
    for(i in 1: nrow(thetas)) {
      
      expected_sample_size[i] <- BOP2_design_improve(lambda, gamma, n1 = 30, n2 = 70, thetas[i,1])
    }
    
    return(expected_sample_size)
    
  }
  
  ########################################################################
  #Plotting the graph of expected sample sizes versus the null hypothesis for different values of the paramters
  
  theta_s<-seq(0.1, 0.9, 0.1)  # values of theta examined 
  
  plot(theta_s, sample_size_graph(0.8, 0.3), type ="b", ylab= "Expected Sample Size", xlab="Null Hypothesis", lwd=2, col="red") # To plot the graph of expected samoles again the null hypothesis
  
  lines(theta_s, sample_size_graph(0.2, 0.7), type ="b", lwd=2, col="green") # To add other plots for different lambda and gamma 
  
  lines(theta_s, sample_size_graph(0.5, 0.5), type ="b", lwd=2, col="purple") ## To add other plots for different lambad and gamma
  
  #To add legend
  legend(.1, 70, c('(lambda=0.8, gamma=0.3)','(lambda=0.5, gamma=0.5)', '(lambda=0.2, gamma=0.7)'),
         lty=c(1,1,1), col=c('red', 'purple', 'green'))
  
  
  
  ########################################################################
  # to obtain or view the expected sample sizes for the different selected Null hypothesis and selected decision parameters
  #Case where lambda is greater than lambda
  sample_size_graph(0.8, 0.3) #The result is the same as using the function BOP2_design_improve() for the null hypothesis examined above
  sample_size_graph(0.5, 0.5) # Case where lambda and gamma are equal
  sample_size_graph(0.2, 0.7) # Case where lambda is less than gamma
  
  
  
  
