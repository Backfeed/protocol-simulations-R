# script for defining scenarios

s <- 0.01 ; # stake payment
d <- 0.1 ; # stake distribution factor
alpha <- 0.8; # burn power 
beta <- 1 ; # stake fee skewness
initialReputation <- 1 ;
initialTokens <- 20 ;
contributionsNum <- 1 ;
numUsers <- 100 ;
numEvaluations <- numUsers ;
tokenRewardFactor <- 0 ;
reputationRewardFactor <- 0 ;
rewardScoreThreshold <- 0.3 ;

# create user data frame
userNames <- paste("P",1:numUsers,sep = "");
userReputation <- rep(initialReputation, numUsers) ;
userTokens <- rep(initialTokens, numUsers) ;
users <- data.frame(userNames, userReputation, userTokens) ;
names(users)<- c("name", "reputation", "tokens");

#creat contributions data frame
contribIds <- paste("C",1:contributionsNum,sep = "") ;
contributor <- sample(1:numUsers, contributionsNum, replace=T) ;
votes <- matrix(-1, nrow = contributionsNum, ncol = numUsers) ;
colnames(votes)<-userNames ;
contributions<-data.frame(id = contribIds, contributor = contributor, votes) ;

# create scenario data frame
evaluators <- c(1:numEvaluations) ;
evaluatedContribs <- rep(1,numEvaluations) ;
voteValues <- rep(1,numEvaluations) ;
scenario <- data.frame(evaluators, evaluatedContribs, voteValues) ;

# # #define sheet scenario as dataframe.
# evaluators <- c(1,2,1,3,4,5,4,1,2,3) ;
# evaluatedContribs <- c(1,1,2,1,1,1,2,2,2,2) ;
# voteValues <- c(1,0,1,1,1,0,1,0,0,1) ;
# scenario <- data.frame(evaluators, evaluatedContribs, voteValues) ;
# numEvaluations <- length(evaluators) ;

