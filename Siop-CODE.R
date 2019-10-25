library(missForest)
library(dplyr)
###renaming data###
library(haven)
data <- read_sav("~/Data for Benjamin Carpentier External Research Request 8.1.2019.sav")
###renaming columns in data###
mydata <- data %>% rename(BOY_math_RITS_Score = BOYMath18.19TestRITScore,
                        MOY_math_RITScore = MOYMath18.19TestRITScore,
                        EOY_math_RITScore = EOYMath18.19TestRITScore,
                        BOY_read_RITScore = BOYRead18.19TestRITScore,
                        MOY_read_RITScore = MOYRead18.19TestRITScore,
                        EOY_read_RITScore = EOYRead18.19TestRITScore)

###NOTE: BOY = "Beginning Of Year", MOY = "Middle Of Year, EOY = "End Of Year"
###Counting Missing Values###
sum(is.na(mydata$BOY_math_RITS_Score))
sum(is.na(mydata$MOY_math_RITScore))
sum(is.na(mydata$EOY_math_RITScore))
###
sum(is.na(mydata$BOY_read_RITScore))
sum(is.na(mydata$MOY_read_RITScore))
sum(is.na(mydata$EOY_read_RITScore))
###


###Two time points will be -> BOY_math & EOY_math##
###Subsetting data from original data frame###
myvars <- c("BOY_math_RITS_Score", "EOY_math_RITScore")
newdatapoints <- mydata[myvars]
newdatapointsCLEAN <- na.omit(newdatapoints)

final <- matrix(nrow=4, ncol=6)
output <- matrix(nrow=30777, ncol=3)
missingness <- c(0,0.1,0.2,0.3)
missingness <- data.frame(missingness)

for (j in 1:4) {
  
  for (i in 1:3) {
    
    missingness <- missingness[j,1]
    intromiss <- prodNA(newdatapointsCLEAN, missingness)
    pretest <- intromiss[,1]
    posttest <- intromiss[,2]
    
    
    
    paired <- t.test(pretest,posttest,paired=TRUE)$p.value
    independent <- t.test(pretest,posttest,paired=FALSE)$p.value
    
    
    pretest <- data.frame(pretest)
    posttest <- data.frame(posttest)
    
    pretest[is.na(pretest)] <- mean(pretest[,1], na.rm=TRUE)
    posttest[is.na(posttest)] <- mean(posttest[,1], na.rm=TRUE)
    ###Mean Substitution###
    meansub <- t.test(pretest[,1],posttest[,1],paired=TRUE)$p.value
    ###Putting results information from data into table###
    output[i,1] <- paired
    output[i,2] <- independent
    output[i,3] <- meansub
    
    if(j == 1){
      final[j,1] <- (sum(output[,1] < 0.05))/30777
      final[j,2] <- (sum(output[,2] < 0.05))/30777
      final[j,3] <- (sum(output[,3] < 0.05))/30777
    }
    
    else{
      
      final[j,1] <- (sum(output[,1] > 0.05))/30777
      final[j,2] <- (sum(output[,2] > 0.05))/30777
      final[j,3] <- (sum(output[,3] > 0.05))/30777
      
    }
    
  }
  
}

error <- rep(c("I","II"), c(1,3))
preptable <- cbind("Missingness","final,error")
results <- data.frame(preptable)
colnames(results) <- c("Missingness", "Paired", "Independent", "Mean Substitution", "Type")

results
