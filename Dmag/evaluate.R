 # the main function in the BF protocol - allows for a single evaluation to be made along with its two sub-functions.


# define a function for the stake distribution
stakeDistribution <- function(currentEvaluatorRep, votedRep, totalRep, equallyVotedRep, alpha, beta) {
  #x <- (currentEvaluatorRep / equallyVotedRep) * (equallyVotedRep / totalRep)^alpha
  #x <- (currentEvaluatorRep/equallyVotedRep) * (1 - (equallyVotedRep/totalRep)^beta) ;
  x<- currentEvaluatorRep/equallyVotedRep ;
  return(x) ;
}

burnFactor <-function(currentEvaluatorRep, votedRep, totalRep, equallyVotedRep, alpha, beta){
  x <- (equallyVotedRep / totalRep)^alpha
  #x <- exp(-alpha*currentEvaluatorRep/totalRep) ;
  #x <- 1 ;
}

# define a function for the stake payment
stakeFee <- function(currentEvaluatorRep, votedRep, totalRep, equallyVotedRep, beta, bidDuration = 1, voteTime = 0) {
  x <- 1 - (votedRep/totalRep)^beta ;
  y <- 1 - voteTime/bidDuration ;
  #y <-1 ;
  # votedRep inculdes currentEvaluatorRep and therfore >=0.
  #x <- 1 - (equallyVotedRep/totalRep) ;
  return(currentEvaluatorRep * x * y);
}

evaluate <- function(users, contribs, contribInd, evaluatorInd, vote, alpha, beta, s, d,
                     tokenRewardFactor, reputationRewardFactor, rewardScoreThreshold, results, eventIndex)
{
  
  eventDescription <- as.character(paste(users$name[evaluatorInd], " evaluates ", 
                            contribs[contribInd,1], " by ", as.character(vote))) ;
  print(eventDescription) ;
  
  # print results in the case that function returns without any update.
  results[eventIndex,1] <- eventDescription ;
  results[eventIndex,2:(length(users$name)+1)] <- users$reputation ;
  results[eventIndex,(length(users$name)+2):(2*length(users$name)+1)] <- users$tokens ;
  
  contrib<-contribs[contribInd,];
  currentComment <- "" ;
  
  
  # check whether user has voted
  if (contrib[3+evaluatorInd] != -1) {
    currentComment <- paste(currentComment, users$name[evaluatorInd], "already voted.") ;
    if (vote == contrib[3+evaluatorInd]) {
      currentComment <- paste(currentComment, "Vote unchanged.") ;
      results$comments[eventIndex] <- currentComment ;
      return ;
    } else {
      currentComment <- paste(currentComment, "Vote changed.") ;
      results$comments[eventIndex] <- currentComment ;
      contrib[3+evaluatorInd] <- -1 ;
    }
  }
  
  previousUpvoteRep <- sum(users$reputation[contrib[4:length(contrib)] == 1]) ;
  currentEvaluatorRep <- users$reputation[evaluatorInd] ;
  previousVotersLogical <- contrib[4:length(contrib)] != -1 ;
  currentVotersLogical <- previousVotersLogical | (1:length(users$reputation) == evaluatorInd) ;
  previousEqualVotersLogical <- contrib[4:length(contrib)] == vote ;
  previousUpVotersLogical <- contrib[4:length(contrib)] == 1 ;
  
  # evaluator makes the vote.
  contrib[3+evaluatorInd] <- vote ;
  
  equalVotersLogical <- previousEqualVotersLogical | (1:length(users$reputation) == evaluatorInd) ;
  currentUpVotersLogical <- contrib[4:length(contrib)] == 1 ;
  previousVotedRep <-sum(users$reputation[previousVotersLogical]) ;
  votedRep <- sum(users$reputation[currentVotersLogical]) ; #includes current evaluator
  totalRep <- sum(users$reputation) ;
  equallyVotedRep <- sum(users$reputation[equalVotersLogical]) ;
  upVotedRep <- sum(users$reputation[currentUpVotersLogical]) ;
  previousScore <- previousUpvoteRep/totalRep ;
  currentScore <- upVotedRep/totalRep ;
  
  # only if current vote is an upvote it triggers reputation dispersal and stake payment.
    
  # update users reputation 1) stake distribution
  stakeDistVal <- stakeDistribution(currentEvaluatorRep, votedRep, totalRep, equallyVotedRep, alpha, beta) ;
  burnF <- burnFactor(currentEvaluatorRep, votedRep, totalRep, equallyVotedRep, alpha, beta) ;
  
  # update previous voters
  users$reputation[previousEqualVotersLogical] <- users$reputation[previousEqualVotersLogical] * 
    (1 + d * stakeDistVal * burnF) ;

  # stake fee for current evaluator.
  
  fee <- stakeFee(currentEvaluatorRep, votedRep, totalRep, equallyVotedRep, beta) ;
  users$reputation[evaluatorInd] <- users$reputation[evaluatorInd] - s * fee ;
  
  # reward contributor
  rewardBase <- 0 ;
  maxScore <- contrib$maxScore ;
  if(currentScore > maxScore){
    if(maxScore > rewardScoreThreshold) {
      rewardBase <- currentScore - maxScore ;
    } else if (currentScore > rewardScoreThreshold){
      rewardBase <- currentScore ;
    }
    contrib$maxScore <- currentScore ;
  }
  
  # update contribs data frame
  contribs[contribInd,] <- contrib ;
  
  if(rewardBase > 0) {
    print(rewardBase);
    tokenReward <- tokenRewardFactor * rewardBase ;
    reputationReward <- reputationRewardFactor * rewardBase ;
    users$tokens[contributions$contributor[contribInd]] <- users$tokens[contributions$contributor[contribInd]] + tokenReward ;
    users$reputation[contributions$contributor[contribInd]] <- users$reputation[contributions$contributor[contribInd]] + reputationReward ;
    currentComment <- paste(currentComment, users$name[contributions$contributor[contribInd]], 
                            "rewarded with tokens and rep. for contribution ", contrib$id) ;
    results$comments[eventIndex] <- currentComment ;
  }
  
  results[eventIndex,2:(length(users$name)+1)] <- users$reputation ;
  results[eventIndex,(length(users$name)+2):(2*length(users$name)+1)] <- users$tokens ;
  
  # return updated data frames
  return(list(users,contribs, results)) ;
}