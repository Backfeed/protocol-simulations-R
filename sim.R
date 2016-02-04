# script for running simulations of the backfeed protocol

rm(list=ls());
source('evaluate.R') ;

#alphas <- seq(from = 0, to = 1, by = 0.1) ;
#maxReputationGainOverStake <- numeric(length(alphas)) ;

#for( k in 1:length(alphas) ){
  source('scenario.R') ;
 # alpha <- alphas[k] ;
userRelativeRepVec <- rep(0,numUsers) ;
userRelativeRep <- rep(userRelativeRepVec, numUsers) ;
userRelativeRep <- matrix(userRelativeRep, numUsers, numUsers) ;

  for( i in 1:numEvaluations) {
    evaluatorInd <- scenario$evaluators[i] ;
    contribInd <- scenario$evaluatedContribs[i] ;
    vote <- scenario$voteValues[i] ; #sample(0:1,1) ;
    x <- evaluate(users, contributions, contribInd, evaluatorInd, vote,
                  alpha, beta, s, 
                  tokenRewardFactor, reputationRewardFactor, rewardScoreThreshold) ;
    users <-x[[1]] ;
    contributions <-x[[2]] ;
    userRelativeRep[,i] <- users$reputation/sum(users$reputation) ;
    print(userRelativeRep[100,i]); #[1]/sum(users$reputation)) ;
    #print(contributions) ;
  }
  #maxReputationGainOverStake[k] <- (numUsers * users$reputation[1]/sum(users$reputation) - 1)/s ;
#}

#plot(alphas, maxReputationGainOverStake) ;

plot(userRelativeRep[1,],type="o", col="blue", ylim = c(0.0095,0.011)) ;
lines(userRelativeRep[25,], type="o", col="green") ;
lines(userRelativeRep[50,], type="o", col="red") ;
lines(userRelativeRep[75,], type="o", col="magenta") ;
lines(userRelativeRep[100,], type="o", col="yellow") ;

plot(userRelativeRep[,100]);

