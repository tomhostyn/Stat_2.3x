# 1. A simple random sample of size 300 is taken from a population of hundreds of thousands of adults. 
# The average weight of the sampled people is 150 pounds and the SD of their weights is 30 pounds.
# 
# a) The average weight of the population is estimated to be _______ pounds; 
#       the SE for this estimate is about ______ pounds.
# b) An approximate 99%-confidence interval for the average weight in the population goes from ________ pounds
#       to _________ pounds.
# 




q1 <- function () {
  N<- 300;
  avg <- 150;
  SD <- 30;
  
  z<- 2.58
  
  answer <- c();
  answer$pop_avg <- avg;
  answer$SE <- SD/sqrt(N)
  answer$CI_min <- avg - z*answer$SE
  answer$CI_max <- avg + z*answer$SE
  print (answer)
}


# 2. In a simple random sample of size 400 taken from over 500,000 workers, 21% of the sampled workers are 
# in carpools.
# 
# a) In the population, the percent of workers in carpools is estimated to be _______%; the SE for this estimate
# is about _________%.
# b) An approximate 95%-confidence interval for the percent of carpooling workers in the population goes
# from _______ % to _______ %.

q2 <- function () {
  N <- 400
  pop <- 500000
  carpool <- 0.21
  
  z<- 2
  
  answer <- c()
  answer$carpool <- carpool*100
  
  finite_population_correction <- sqrt((pop - N)/(pop-1))
  bootstrap_SD <- sqrt(carpool * (1-carpool))*100
  answer$SE <- finite_population_correction * bootstrap_SD/sqrt(N)
  answer$SE_min <- answer$carpool - z* answer$SE
  answer$SE_max <- answer$carpool + z* answer$SE
  
  
  print (answer)
}


# 3. A simple random sample of 150 undergraduates is taken at a large university. The average MSAT score
# of the sampled students is 528 with an SD of 90. Construct an approximate 90%-confidence interval for the
# average MSAT score of undergraduates at the university.

q3 <- function () {
  N <- 150
  MSAT <- 528
  SD <- 90
  
  z <- 1.64
  
  SE <- SD / sqrt(N)
  
  c(MSAT - z * SE, MSAT + z*SE)
}


# 4. In a simple random sample of 500 students taken at a large university, 180 have undeclared majors. 
# Construct an approximate 85%-confidence interval for the percent of students at the university who have
# undeclared majors.

q4 <- function () {
  N <- 500
  p <- 180/500
  
  z <- 1.45
 
  bootstrap_SD <- sqrt(p * (1-p))
  
  answer <- c()
  answer$SE <- bootstrap_SD/sqrt(N)
  answer$SE_min <- p - z* answer$SE
  answer$SE_max <- p + z* answer$SE  
}


q5e <- function (){
  N <- 900
  avg <- 2.2
  SD <- 2
  
  SE <- SD/sqrt(N)
  z <- 2
  c(avg - z * SE, avg + z * SE)
}
 

q6b<- function (){
  N <- 275
  avg <- 920
  SD <- 500
  
  SE <- SD/sqrt(N)
  z <- 1
  c(avg - z * SE, avg + z * SE)
}