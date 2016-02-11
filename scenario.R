# script for defining scenarios

s <- 0.02 ; # stake payment
d <- 0.1 ; # stake distribution factor
alpha <- 1; # burn power 
beta <- 1 ; # stake fee skewness
initialReputation <- 10 ;
initialTokens <- 20 ;
contributionsNum <- 3 ;
numUsers <- 5 ;
numEvaluations <- 10 ;
tokenRewardFactor <- 5 ;
reputationRewardFactor <- 2 ;
rewardScoreThreshold <- 0.5 ;
bidDuration <- 1000 ; # assume contributions are in a single bid
contributionFee <- 1 ;

# create user data frame
userNames <- paste("P",1:numUsers,sep = "");
userReputation <- rep(initialReputation, numUsers) ;
userTokens <- rep(initialTokens, numUsers) ;
users <- data.frame(userNames, userReputation, userTokens) ;
names(users)<- c("name", "reputation", "tokens");

#create contributions data frame
contribIds <- paste("C",1:contributionsNum,sep = "") ;
contributor <- sample(1:numUsers, contributionsNum, replace=T) ;
votes <- matrix(-1, nrow = contributionsNum, ncol = numUsers) ;
colnames(votes)<-userNames ;
contributions<-data.frame(id = contribIds, contributor = contributor, votes) ;

# deduct contribution fee for contributors
for (i in 1:contributionsNum)
{
  if (users$tokens[contributions$contributor[i]] >= contributionFee) {
    users$tokens[contributions$contributor[i]] <- users$tokens[contributions$contributor[i]] - contributionFee ;
  }
  else {
    stop("user does not have enough tokens") ;
  }
}

# create scenario data frame
evaluators <- sample(1:numUsers, numEvaluations, replace = TRUE) ;#c(1:numEvaluations) ;
evaluatedContribs <- sample(1:contributionsNum, numEvaluations, replace = TRUE) ;
voteValues <- sample(0:1, numEvaluations, replace = TRUE);
votingTimesUnsrt <- sample(1:bidDuration, numEvaluations, replace = FALSE) ;
votingTimes <- sort(votingTimesUnsrt) ;
scenario <- data.frame(evaluators, evaluatedContribs, voteValues, votingTimes) ;

# create scenarion results data frame
events <- character(length = numEvaluations + 1) ;
comments <- character(length = numEvaluations + 1) ;
events[1] <- "Initial state after contributions made" ;

initReputationVector <- rep(0,numUsers) ;
reputation <- rep(initReputationVector,numEvaluations + 1) ;
reputation <- matrix(reputation, numEvaluations + 1, numUsers, byrow = TRUE) ;
reputation[1,] <- rep(initialReputation,numUsers) ;
colnames(reputation) <- paste("P",1:numUsers, " rep", sep = "");

initTokensBalance <- rep(0,numUsers) ;
tokensBalance <- rep(initTokensBalance,numEvaluations + 1) ;
tokensBalance <- matrix(tokensBalance, numEvaluations + 1, numUsers, byrow = TRUE) ;
tokensBalance[1,] <- users$tokens ;
colnames(tokensBalance) <- paste("P",1:numUsers, " tok", sep = "");
results <- data.frame(events, reputation, tokensBalance, comments, stringsAsFactors=FALSE) ;

# # #define sheet scenario as dataframe.
# evaluators <- c(1,2,1,3,4,5,4,1,2,3) ;
# evaluatedContribs <- c(1,1,2,1,1,1,2,2,2,2) ;
# voteValues <- c(1,0,1,1,1,0,1,0,0,1) ;
# scenario <- data.frame(evaluators, evaluatedContribs, voteValues) ;
# numEvaluations <- length(evaluators) ;

