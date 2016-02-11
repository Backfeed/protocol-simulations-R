 # the main function in the BF protocol - allows for a single evaluation to be made along with its two sub-functions.


# define a function for the stake distribution
stakeDistribution <- function(currentEvaluatorRep, votedRep, totalRep, equallyVotedRep, alpha) {
  x <- (currentEvaluatorRep / equallyVotedRep) * (equallyVotedRep / totalRep)^alpha
  return(x) ;
}

# define a function for the stake payment
stakeFee <- function(currentEvaluatorRep, votedRep, totalRep, equallyVotedRep, beta, bidDuration = 1, voteTime = 0) {
  x <-  (  1 - ((votedRep - currentEvaluatorRep) / totalRep)^beta ) ;
  y <- (1 - voteTime/bidDuration) ;
  # votedRep inculdes currentEvaluatorRep and therfore >=0.
  return(x*y);
}

evaluate <- function(users, contribs, contribInd, evaluatorInd, vote, bidDuration, voteTime, alpha, beta, s, d,
                     tokenRewardFactor, reputationRewardFactor, rewardScoreThreshold, results, eventIndex)
{
  
  eventDescription <- as.character(paste(users$name[evaluatorInd], " evaluates ", 
                            contribs[contribInd,1], " by ", as.character(vote), 
                            " at time ", as.character(voteTime))) ;
  print(eventDescription) ;
  # print results in the case that function returns without any update.
  results[eventIndex,1] <- eventDescription ;
  results[eventIndex,2:(length(users$name)+1)] <- users$reputation ;
  results[eventIndex,(length(users$name)+2):(2*length(users$name)+1)] <- users$tokens ;
  
  contrib<-contribs[contribInd,];
  
  # check whether user has voted
  if (contrib[2+evaluatorInd] != -1) {
    print("user has voted") ;
    if (vote == contrib[2+evaluatorInd]) {
      print("Unchanged vote") ;
      return ;
    } else {
      print("changed vote") ;
      contrib[2+evaluatorInd] <- -1 ;
    }
  }
  
  previousUpvote <- sum(users$reputation[contrib[3:length(contrib)] == 1]) ;
  currentEvaluatorRep <- users$reputation[evaluatorInd] ;
  contrib[2+evaluatorInd] <- vote ; # evaluator makes the vote.
  votedRep <- sum(users$reputation[contrib[3:length(contrib)] != -1]) ; #includes current evaluator
  totalRep <- sum(users$reputation) ;
  equallyVotedRep <- sum(users$reputation[contrib[3:length(contrib)] == vote]) ;
  upVotedRep <- sum(users$reputation[contrib[3:length(contrib)] == 1]) ;
  previousScore <- previousUpvote/totalRep ;
  currentScore <- upVotedRep/totalRep ;
  
  # update users reputation 1) stake distribution
  stakeDistVal <- stakeDistribution(currentEvaluatorRep, votedRep, totalRep, equallyVotedRep, alpha) ;
  users$reputation[contrib[3:length(contrib)] == vote] <- users$reputation[contrib[3:length(contrib)] == vote]*
  (1 + d * stakeDistVal ) ;

  # stake fee for current evaluator.
  print(users$reputation[evaluatorInd]) ;
  users$reputation[evaluatorInd] <- users$reputation[evaluatorInd] - s * currentEvaluatorRep *
    stakeFee(currentEvaluatorRep, votedRep, totalRep, equallyVotedRep, beta, bidDuration, voteTime) ;
  print(users$reputation[evaluatorInd]) ;
  # update contribs data frame
  contribs[contribInd,] <- contrib ;
  
  # reward contributor
  rewardBase <- 0 ;
  if(previousScore > rewardScoreThreshold) {
    rewardBase <- currentScore - previousScore ;
  } else if (currentScore > rewardScoreThreshold){
    rewardBase <- currentScore ;
  }
  
  tokenReward <- tokenRewardFactor * rewardBase ;
  reputationReward <- reputationRewardFactor * rewardBase ;
  users$tokens[contribInd] <- users$tokens[contribInd] + tokenReward ;
  users$reputation[contribInd] <- users$reputation[contribInd] + reputationReward ;
  
  results[eventIndex,2:(length(users$name)+1)] <- users$reputation ;
  results[eventIndex,(length(users$name)+2):(2*length(users$name)+1)] <- users$tokens ;
  
  # return updated data frames
  return(list(users,contribs, results)) ;
}