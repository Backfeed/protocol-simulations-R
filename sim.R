# script for running simulations of the backfeed protocol

# clear workspace
rm(list=ls());

library("colorspace", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2") ;
# source functions and scenario input.
source('evaluate.R') ;
source('scenario.R') ;

for( i in 1:numEvaluations) {
  # extract params for evaluation
  evaluatorInd <- scenario$evaluators[i] ;
  contribInd <- scenario$evaluatedContribs[i] ;
  vote <- scenario$voteValues[i] ;
  eventIndex <- i+1 ;
  # perform evaluation
  x <- evaluate(users, contributions, contribInd, evaluatorInd, vote, alpha, beta, s, d, 
                tokenRewardFactor, reputationRewardFactor, rewardScoreThreshold, 
                results, eventIndex) ;
  
  # update users and contributions data frames.
  users <-x[[1]] ;
  contributions <-x[[2]] ;
  results <- x[[3]] ;
}

# write results to file
write.table(results, file = "results.csv", append = FALSE, quote = FALSE, sep = ", ",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE) ;


# plot relative reputation ferom results.
userRelativeRep <- results[,2:(numUsers+1)] / matrix(rep(rowSums(results[,2:(numUsers+1)]), numUsers), 
                                                     length(results$events),numUsers, byrow = FALSE) ;

# plot total reputation
totalUsersReputation <- rowSums(results[,2:(numUsers+1)]) ;

# define plot axes properties
ylim1 <- min(userRelativeRep) - 0.0001;
ylim2 <- max(userRelativeRep) + 0.0001;
# 
# plot normalized reputation of different users
plot(5, 5, type="n", axes=TRUE, ann=TRUE, 
     xlim=c(0, numEvaluations +1), ylim = c(ylim1,ylim2), 
     ylab = "normalized reputation", xlab = "Event index") ;

numLines = min(numUsers,10) ;
plotIndex <- 1 ;
myCol <-rainbow_hcl(numLines+1) ;
for (i in 1:(numLines+1)) {
  lines(userRelativeRep[,plotIndex], type="o", col = myCol[i]) ;
  plotIndex <- i*numUsers/numLines ;
  print(plotIndex) ;
}

plot(as.numeric(userRelativeRep[length(userRelativeRep),]));
plot(as.numeric(totalUsersReputation)) ;


# lines(userRelativeRep[,1], type="o", col = rgb(1,0,0)) ;
# lines(userRelativeRep[,2], type="o", col = rgb(0,1,0)) ;
# lines(userRelativeRep[,3], type="o", col = rgb(0,0,1)) ;
# lines(userRelativeRep[,4], type="o", col = rgb(1,0,1)) ;
# lines(userRelativeRep[,5], type="o", col = rgb(0,1,1)) ;


