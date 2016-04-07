 # the main function in the BF protocol - allows for a single evaluation to be made along with its two sub-functions.

# define a function for the stake distribution
stakeDistribution <- function(currentEvaluatorRep, votedRep, totalRep, equallyVotedRep, alpha, beta) {
  x<- currentEvaluatorRep/equallyVotedRep ;
  return(x) ;
}

# define a function for the amount of reputation stake to be burnt.
burnFactor <-function(currentEvaluatorRep, votedRep, totalRep, equallyVotedRep, alpha, beta){
  x <- (equallyVotedRep / totalRep)^alpha
}

# define a function for the stake payment
stakeFee <- function(currentEvaluatorRep, votedRep, totalRep, equallyVotedRep, beta) {
  x <- 1 - (votedRep/totalRep)^beta ;
  return(currentEvaluatorRep * x);
}

# the main protocol function which updates the data frames of users and contributions
# users reputation and tokens are updated according to the protocol
# contributions data frame is updated to include the new vote.
evaluate <- function(users, contribs, contribInd, evaluatorInd, vote, alpha, beta, s, d,
                     tokenRewardFactor, reputationRewardFactor, rewardScoreThreshold, results, eventIndex)
{
  
  # events lod
  eventDescription <- as.character(paste(users$name[evaluatorInd], " evaluates ", 
                            contribs[contribInd,1], " by ", as.character(vote))) ;
  print(eventDescription) ;
  
  # print results in the case that function returns without any update.
  results[eventIndex,1] <- eventDescription ;
  results[eventIndex,2:(length(users$name)+1)] <- users$reputation ;
  results[eventIndex,(length(users$name)+2):(2*length(users$name)+1)] <- users$tokens ;
  
  # get the line of the evaluated contribution
  contrib<-contribs[contribInd,];
  
  # no comments at this point
  currentComment <- "" ;
  
  # check whether user has previously voted on this contribution
  # contrib[4:end] are the user votes or non-votes.
  if (contrib[3+evaluatorInd] != -1) {
    # add comment that the user has already voted
    currentComment <- paste(currentComment, users$name[evaluatorInd], "already voted.") ;
    
    # if the vote has not changed just add a comment and return to the simulation
    if (vote == contrib[3+evaluatorInd]) {
      currentComment <- paste(currentComment, "Vote unchanged.") ;
      results$comments[eventIndex] <- currentComment ;
      return ;
      
      # if vote has changed, remove existing vote, add a comment, and follow the rest of the code regularly
    } else {
      currentComment <- paste(currentComment, "Vote changed.") ;
      results$comments[eventIndex] <- currentComment ;
      contrib[3+evaluatorInd] <- -1 ;
    }
  }
  
  # calculate all necessary quantities to calculate users' reputation update.
  previousVotersLogical <- contrib[4:length(contrib)] != -1 ; # get the logical indexing of previous voters of the contribution.
  previousUpVotersLogical <- contrib[4:length(contrib)] == 1 ;# get the logical indexing of previous up voters of the contribution.
  previousEqualVotersLogical <- contrib[4:length(contrib)] == vote ; # get the logical indexing of previous equally voting users of the contribution.
  
  previousVotedRep <-sum(users$reputation[previousVotersLogical]) ; # sum over reputation of users which have voted prior to the current
  previousUpvoteRep <- sum(users$reputation[previousUpVotersLogical]) ; # sum over reputation of users which have upvoted prior to the current
  currentEvaluatorRep <- users$reputation[evaluatorInd] ;

  # evaluator makes the vote.
  contrib[3+evaluatorInd] <- vote ;
  
  currentEqualVotersLogical <- previousEqualVotersLogical | (1:length(users$reputation) == evaluatorInd) ; # get logical indexing of equal voters including current voter
  currentUpVotersLogical <- contrib[4:length(contrib)] == 1 ; # get logical indexing of up voters including current voter
  currentVotersLogical <- previousVotersLogical | (1:length(users$reputation) == evaluatorInd) ; # logical indexing of voters including current evaluator
  
  #includes current evaluator
  votedRep <- sum(users$reputation[currentVotersLogical]) ; 
  totalRep <- sum(users$reputation) ; # the total amount of reputation in the system
  equallyVotedRep <- sum(users$reputation[currentEqualVotersLogical]) ;
  upVotedRep <- sum(users$reputation[currentUpVotersLogical]) ;
  
  # score is defined by the upvoted reputation divided by the total amount of reputation
  currentScore <- upVotedRep/totalRep ;
  
  # update users reputation 1) stake distribution for previous equally voting users
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
    tokenReward <- tokenRewardFactor * rewardBase ; # calc token reward by score/score_delta * tokenRewardFactor
    reputationReward <- reputationRewardFactor * rewardBase ; # calc token reward by score/score_delta * reputationRewardFactor
    users$tokens[contributions$contributor[contribInd]] <- users$tokens[contributions$contributor[contribInd]] + tokenReward ;
    users$reputation[contributions$contributor[contribInd]] <- users$reputation[contributions$contributor[contribInd]] + reputationReward ;
    # add commnet to results data frame
    currentComment <- paste(currentComment, users$name[contributions$contributor[contribInd]], 
                            "rewarded with tokens and rep. for contribution ", contrib$id) ;
    results$comments[eventIndex] <- currentComment ;
  }
  
  # update results
  results[eventIndex,2:(length(users$name)+1)] <- users$reputation ;
  results[eventIndex,(length(users$name)+2):(2*length(users$name)+1)] <- users$tokens ;
  
  # return updated data frames
  return(list(users,contribs, results)) ;
}