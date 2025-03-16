

#selecting my file and correct file path
file.choose()


# assign the dataset to a variable in R
datarules <- read.csv("E:\\Downloads\\ML_Data_DAST_Assignment.csv")


# Check to see which values are missing if any 
sum(is.na(datarules))


# we decided to drop any missing data 
datarules <- na.omit(datarules)

#calculate the total GAD score for each patient 
datarules$ANXIETY <- datarules$GAD1 + datarules$GAD2 + datarules$GAD3 + datarules$GAD4 + datarules$GAD5 + datarules$GAD6 + datarules$GAD7


# calculate the mean of this new variable 
mean(datarules$ANXIETY)

#using standardized thresholds for audit in determining service levels 
# scores of 0 no risk, scores 1 - 2 indicate low level drug misuse may need breif Intervention, 
# score 3 - 10 moderate and may need a referral 

#drug survey score related to medical care 
datarules$DRUG_USE_SURVEY <- datarules$DAST1 + datarules$DAST2 + datarules$DAST3 +
datarules$DAST4 + datarules$DAST5 + datarules$DAST6 + datarules$DAST7 +
  datarules$DAST8 + datarules$DAST9 + datarules$DAST10 

#given that the package is already instlled I just need to require it in order to use it
# think of this as loading the package 
require(dplyr)

# per the instructions we need to recode the drug use data. 
datarules$SERVICE <- recode(datarules$DRUG_USE_SURVEY, '0' = "No Risk", '1' = "BI", '2' = "BI",
                            '3' = "RT", '4' = "RT", '5' = "RT", '6' = "RT", '7' = "RT", '8' = "RT",
                            '9' = "RT", '10' = "RT")

# we need to be able to convert the variable into a facotr so R doesn't treat 
# it like a character vector. 
datarules$SERVICE <- as.factor(datarules$SERVICE)

# I need to now check the recoded code 
table(datarules$SERVICE)

#creating an object
keep <- c("AGE", "ANXIETY", "SERVICE")

#creating a new dataset with just the needed vriables 
datarules <- datarules[keep]

#overview of bivariate relationships 
plot(datarules)


# we want to set seeed many that everytime we run it we will get the same
# exactly the same numbers each time 
set.seed(9)



#Now we take 60 percent of the dataset for training 
s <- sample(1496, 898) #generates 897 numbers from dataset

# specific columns in dataset. Character vector containing 3 elements 
# c() stands for combine/ concatenate vectors by combining multiple values into a single data structur
col <- c("AGE", "ANXIETY","SERVICE")


#Now we can create a subset of the dataset to store it in train
# s is the specific rows to select
# col is the specific columns to select 
#data2 refers to indexing a dataframe 
train <- datarules[s, col] #training dataset


#we use remianing 599 observations for the testing dataset
# -s excludes the rows listed in s, all remaining rows go into test
# same columns are still used. We need the same structure as a training subset
test <- datarules[-s, col]


#load machine learning package due to it was already installed
require(e1071)


#svmfit stores the trained SVM model in the vairable svmfit. This allows us to use it later for predictions 
#svm() this function trains an SVm model
# Service is the dependent model (target) variable which we are predicting 
# . means use all other columns in train as independent (predictor) variables 
# data=train the model being used 
#kernel = "linear. This defines the kernel type of SVM. 
# linear means that the decision boundary is staight line
# learn about other kernels 
#cost = 0.1 . Cost perameter controls the trad off beteen margin width and misclassfication 
# > low like 0.1 allows miscalssification making it more gneralizable 
# > high 100 fewer missclass but may be overfitting 
# scale =F means no scaling default is true
#> for SVM's default is reccomneded for SVM's, especially when features have different ranges 
#> # type = 'C-classification' -Standard SVM for classification problesm
#> 3 > use 'eps-regression' used for regression problems 

svmfit <- svm(SERVICE ~., data = train, kernel="linear", cost= 9.1 , scale = F,
              type = 'C-classification')

print(svmfit)
svmfit$tot.nSV # if i just need the number of support vectors 

# this will examine how age and anxety predict service level 
plot(svmfit, train[,col])


# too general the model for bet accuracy by choosing best cost. 
# tune() is a function from e1071 package that automatically searches 
#for best hyperparameters 
#it tries different values of cost from the range i suggested to find 
#the best performing model
#It trains multiple sVM models each with a diff cost value and evals them
# I will create a custom varaible for this 
tuned <- tune(svm, SERVICE ~. , data = train, kernel = "linear",
              ranges = list(cost =c(8.9, 9.0, 9.1, 9.2, 9.3)))

#display tuning results 
summary(tuned)


#make predictions with our trained mode on our test dataset 
p <- predict(svmfit, test[,col], type = "class")
table(p, test[,3])   # see how well the model does at predicting categories           
mean(p== test[,3]) #predict accuracy 
colnames(datarules)
