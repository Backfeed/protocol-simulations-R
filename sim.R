# script for running simulations of the backfeed protocol
# dddd
a
# clear workspace
rm(list=ls());

# source functions and scenario input.
source('evaluate.R') ;
source('scenario.R') ;

# define variables for simulation output.
userRelativeRepVec <- rep(0,numUsers) ;
userRelativeRep <- rep(userRelativeRepVec, numUsers + 1) ; # +1 is for before the first evaluation.
userRelativeRep <- matrix(userRelativeRep, numUsers, numUsers + 1) ;
userRelativeRep[,1] <- rep(1/numUsers,numUsers) ;

for( i in 1:numEvaluations) {
  # extract params for evaluation
  evaluatorInd <- scenario$evaluators[i] ;
  contribInd <- scenario$evaluatedContribs[i] ;
  vote <- scenario$voteValues[i] ;
  
  # perform evaluation
  x <- evaluate(users, contributions, contribInd, evaluatorInd, vote,
                alpha, beta, s, d, 
                tokenRewardFactor, reputationRewardFactor, rewardScoreThreshold) ;
  
  # update users and contributions data frames.
  users <-x[[1]] ;
  contributions <-x[[2]] ;
  
  # record user normalized reputation after last evaluation.
  userRelativeRep[,i+1] <- users$reputation/sum(users$reputation) ;
}

# define plot axes properties
ylim1 <- min(userRelativeRep) - 0.0001;
ylim2 <- max(userRelativeRep) + 0.0001;

# plot normalized reputation of different users
plot(userRelativeRep[1,],type="o", col="blue", ylim = c(ylim1,ylim2)) ;
lines(userRelativeRep[2,], type="o", col="purple") ;
lines(userRelativeRep[10,], type="o", col="brown") ;
lines(userRelativeRep[25,], type="o", col="green") ;
lines(userRelativeRep[50,], type="o", col="red") ;
lines(userRelativeRep[75,], type="o", col="magenta") ;
lines(userRelativeRep[100,], type="o", col="yellow") ;

# plot users updated reputation vs, order of evaluation.
plot(userRelativeRep[,101]);

