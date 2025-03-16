data2 <- read.csv("E:\\Downloads\\ML_Data_DAST_Assignment1.csv")
sum(is.na(data2)) #determine if values are missing
#drop the 4 missing values/ observations 
data2 <- na.omit(data2) 

#calculate total Gad scores for each patient 
data2$ANXIETY <- data2$GAD1 + data2$GAD2 + data2$GAD3 + data2$GAD4 + data2$GAD5 + data2$GAD6 + data2$GAD7

# calculate the mean of this new variable 
mean(data2$ANXIETY)

#using standardized thresholds for audit in determining service levels 
# scores of 0 no risk, scores 1 - 2 indicate low level drug misuse may need breif Intervention, 
# score 3 - 10 moerate and may need a referral 

#AUDIT total score 
data2$DRUG_MISSUSE <- data2$DAST1 +  data2$DAST2 + data2$DAST3 +  data2$DAST4 +  data2$DAST5 +  data2$DAST6 +  data2$DAST7 +  data2$DAST8 +  data2$DAST9 +  data2$DAST10

install.packages("dplyr") #install package for re-code ing
require(dplyr) #allows me to start using the package

#recode the audit data
data2$SERVICE <- recode(data2$DRUG_MISSUSE, '0' = "No Risk", 
                        '1' = "BI", '2'= "BI", '3'= "RT", '4'= "RT", 
                        '5'= "RT", '6'= "RT", '7'= "RT", '8'= "RT",
                        '9'= "RT", '10'= "RT" )
#now it is no longer numerical and is categorcal we will use as factor 
data2$SERVICE <- as.factor(data2$SERVICE)

#check our recoding with this function
table(data2$SERVICE)

#Keep and continue to work with these 3 variables SERVICE, ANXIETY,  AGE
keep <- c("AGE", "ANXIETY", "SERVICE")
data <- data2[keep] #creating a new dataset from data2
plot(data) #ovrview of bivariate relationships 

rm(data)
data2 <- data2[keep] 
plot(data2) #ovrview of bivariate relationships. corrected the issue. plots shows.
#since the data is clean we can begin with the SVM 
# 60 percent of the dataset for training       
set.seed(9) #this will start the random sample generator at 2 
s <- sample(1496,898) #generate 897 numbers from data2
col <- c("AGE", "ANXIETY","SERVICE") #specific columns in dataset 
train <- data2[s, col] #training dataset created

#use remaining 599 observations for the testing dataset
test <- data2[-s, col]


# get ready to install machine learning package 
install.packages("e1071")


#estimate machine learning model with support vector  machine (SVM)
#train model using the training dataset 

require(e1071)
svmfit <- svm(SERVICE ~ ., data=train, kernel="linear", cost=0.01, scale=F, 
              type = 'C-classification')

print(svmfit)
svmfit$tot.nSV
plot(svmfit, train[,col]) #examine how age and anxiety predict service level 

# tune the model for best accuracy by choosing best cost parameter 
tuned <- tune(svm, SERVICE~. , data = train, kernel="linear",
              ranges=list(cost=c(0.001, 0.1, 1, 10, 100)))
summary(tuned) #change cost based on recommended value 





# make predictions with out trained mode on our test dataset

p <- predict(svmfit, test[,col], type="class")
table(p, test[,3]) # see how well the model does at predicting categories 
mean(p== test[,3])# percentage accuracy 
