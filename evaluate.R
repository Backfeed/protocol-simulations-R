 # the main function in the BF protocol - allows for a single evaluation to be made 

evaluate <- function(users, contribs, contribInd, evaluatorInd, vote, alpha, beta, s,
                     tokenRewardFactor, reputationRewardFactor, rewardScoreThreshold)
{
  
  print(paste(users$name[evaluatorInd], " evaluates ", contribs[contribInd,1], "by ", as.character(vote)));
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
  contrib[2+evaluatorInd] <- vote ;
  votedRep <- sum(users$reputation[contrib[3:length(contrib)] != -1]) ;
  totalRep <- sum(users$reputation) ;
  equallyVotedRep <- sum(users$reputation[contrib[3:length(contrib)] == vote]) ;
  upVotedRep <- sum(users$reputation[contrib[3:length(contrib)] == 1]) ;
  previousScore <- previousUpvote/totalRep ;
  currentScore <- upVotedRep/totalRep ;
  
  # update users reputation 1) stake distribution
  users$reputation[contrib[3:length(contrib)] == vote] <- users$reputation[contrib[3:length(contrib)] == vote]*
    (1 + s * stakeDistribution(currentEvaluatorRep, equallyVotedRep, totalRep, alpha) ) ;
  
  # stake fee for current evaluator.
  users$reputation[evaluatorInd] <- users$reputation[evaluatorInd] - currentEvaluatorRep * s * 
    stakeFee(votedRep, totalRep, feeThreshold, feeInvWidth) ;
  
  # update users reputation 2) voting incentive
  votingIncentive <- 1 / (1 - s*(currentEvaluatorRep / totalRep)^beta) ;
  users$reputation[contrib[3:length(contrib)] > -1 ] <- users$reputation[contrib[3:length(contrib)] > -1 ] * votingIncentive ;
  contribs[contribInd,] <- contrib ;
  
  rewardBase <- 0 ;
  
  # reward contributor
  if(previousScore > rewardScoreThreshold) {
    rewardBase <- currentScore - previousScore ;
  } else if (currentScore > rewardScoreThreshold){
    rewardBase <- currentScore ;
  }
#   if (rewardBase > 0) {
#     print("reward") ;
#   }
  tokenReward <- tokenRewardFactor * rewardBase ;
  reputationReward <- reputationRewardFactor * rewardBase ;
  users$tokens[contribInd] <- users$tokens[contribInd] + tokenReward ;
  users$reputation[contribInd] <- users$reputation[contribInd] + reputationReward ;
  
  return(list(users,contribs)) ;
}