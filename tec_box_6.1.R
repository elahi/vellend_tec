##' Robin Elahi
##' 17 Nov 2016
##' Vellend TEC - Box 6.1
##' http://mvellend.recherche.usherbrooke.ca/Box1.htm

## specify initial community and time instructions
J <- 50
init.1 <- J / 2
COM <- vector(length = J)
COM

COM[1:init.1] <- 1
COM
COM[(init.1 + 1):J] <- 2
COM

num.years <- 50
year <- 2

## Set up vector for data collection
freq.1.vec <- vector(length = num.years)
freq.1.vec
freq.1.vec[1] <- init.1 / J

## run simulation
for(i in 1:(J * (num.years - 1))) {
  
  # What is the frequency of spp 1 at this time point?
  freq.1 <- sum(COM == 1) / J 
  
  # Use frequency as the probability of selecting spp 1 at random
  Pr.1  <- freq.1
  
  # Select one individual from the existing vector randomly, and replace it with an individual of species 1 with probability determined from its frequency in the vector
  COM[ceiling(J * runif(1))] <- sample(c(1, 2), 1, prob = c(Pr.1, 1 - Pr.1))
  
  
  # %% is modulo operator - gives remainder when i is divided by j
  if (i %% J == 0){
    freq.1.vec[year] <- sum(COM == 1) / J
    year <- year + 1
  }
}

# graph the results
plot(1:num.years, freq.1.vec, type = "l", 
     xlab = "Time", 
     ylab = "Frequency of species 1", 
     ylim = c(0, 1))
