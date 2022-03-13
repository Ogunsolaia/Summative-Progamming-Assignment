
testthat::test_dir("./Tests/test-practical assignmnet codes.R")

source("./R_codes/practical assignment codes.R")

testing_prob_y1()

BOP2_design(N, lambda, gamma, n1, n2, theta)

BOP2_design_improve(lambda, gamma, n1, n2, theta)

sample_size_graph(lambda, gamma)

