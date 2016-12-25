###########################################################################################
# I have created followig function to identify missing values in any dataframe 
###########################################################################################

Missing = function(dataFrame){
  
  # Check if the provided argument is a dataframe or not 
  if(! is.data.frame(dataFrame)){
    stop("Please provide data frame in the argument")
  }
  
  # compute total number of columns in the dataframe 
  totalCols = ncol(dataFrame)
  
  # Create empty vector to store total number of missing values 
  totalMissingValues <- numeric()
  
  # iterate from 1 to number of cols   
  for(i in 1:totalCols){
    # compute number of missing values for each column and save it into vector
    totalMissingValues <-  c(totalMissingValues,sum(is.na(dataFrame[,i])))  
  }
  
  # create new dataframe as required
  newFrame = data.frame(colnames(dataFrame),totalMissingValues)
  
  # provide column names 
  colnames(newFrame) <- c("Column_Name","Missing_Count")
  return(newFrame)  
}

###########################################################################################
# I have created following function to evaulaute model performance 
###########################################################################################


NumMetrics = function(a,m)  #here a is target value matrix and m is model value matrix
{
  metrics = c(MAD=0, MSE = 0, MAPE = 0, MPSE = 0, tMAD = 0, p90=0, R2 =0)
  metrics["MAD"] = mean(abs(a-m))
  metrics["MSE"] = mean((a-m)^2)
  metrics["MAPE"] = mean(abs(a-m)*100/a)
  metrics["MPSE"] = mean((((a-m)/a)^2)*100)
  metrics["tMAD"] = mean(abs(a-m),trim = 0.05)
  metrics["p90"] = quantile(abs(a-m),probs = 0.9)
  
  SST = sum((a-mean(a))^2)
  SSE = sum((a-m)^2)
  metrics["R2"] = 1- (SSE/SST)
  return(metrics)
}


###########################################################################################
# Connect to PostgreSQL AWS instance 
###########################################################################################

install.packages("DBI")
install.packages("RPostgreSQL")

library(RPostgreSQL)
library(DBI)

# load the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# Create connection object 
con <- dbConnect(
  drv,
  dbname = "dbName",
  host = "hostName",
  port = 5432,
  user = "userName",
  password = "pwd"
)
# Note: I have removed connectivity credentials from the above dbConnect code

###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################

# Note: To answer all the questions of the assesment, I have used lending_club_2015 data table 

###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################


###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################

# 1. Does the data have any outliers?

###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################

# I have checked for outliers in loan amount column

# Calculated Standard Deviation for loan amount 
StDevDF = dbGetQuery(con, "select stddev_pop(loan_amnt) AS sd from lending_club_2015")
SD <- StDevDF$sd

# Calculated mean for loan amount 
MeanDF = dbGetQuery(con, "select avg(loan_amnt) AS mean from lending_club_2015")
Mean <- MeanDF$mean

# Calculated upper and lower for loan amount 
lower = Mean - (1 * SD)
upper = Mean + (1 * SD)

# Filtered loan amount based on upper and lower values (Pulled all outlier values of loan amount)
Outliers15 = dbGetQuery(con, paste("select count(*) AS rowcount from lending_club_2015 where loan_amnt not between", lower, "and", upper))
outlierCount <- Outliers15$rowcount

# Calculated Percentage of Outliers in loan amount
totalRowsDF <- dbGetQuery(con, "select count(*) AS totalrows from lending_club_2015")
totalRows <- totalRowsDF$totalrows

# 36.14% outliers are present in loan amount 
percentage = round(outlierCount/totalRows,4)
paste(percentage*100,"% outliers were present in 2015")


#######################################################
# Question 1 Output 
#######################################################

# > paste(percentage*100,"% outliers were present in 2015")
# [1] "36.14 % outliers were present in 2015"

########################################################

########################################################################################### 
###########################################################################################
###########################################################################################
###########################################################################################

# 2. What is the monthly total loan volume by dollars and by average loan size?

###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################

# Save loan amount into the dataframe 
DF2 <- dbGetQuery(con, "select loan_amnt,issue_d from lending_club_2015")

# This shows that loan amount has one NA value 
sum(is.na(DF2$loan_amnt))

# Remove DF2 object from the workspace
rm(DF2)

########################################################
########################################################
# 2A. What is the monthly total loan volume by dollars?
########################################################
########################################################

Question2A= dbGetQuery(con, "SELECT SUM(loan_amnt) AS LOAN_VOLUME_BY_DOLLARS,
  to_date(issue_d,'Mon YYYY') AS MONTH
                       FROM lending_club_2015
                       WHERE loan_amnt IS NOT NULL
                       GROUP BY MONTH
                       ORDER BY MONTH")
Question2A


#######################################################
# Question 2A R Output 
#######################################################

#> Question2A
#loan_volume_by_dollars      month
#1               533132575 2015-01-01
#2               366908525 2015-02-01
#3               390003275 2015-03-01
#4               539401075 2015-04-01
#5               483189475 2015-05-01
#6               429777175 2015-06-01
#7               696238600 2015-07-01
#8               555331400 2015-08-01
#9               450246800 2015-09-01
#10              738221400 2015-10-01
#11              567247325 2015-11-01
#12              667910550 2015-12-01

#######################################################


##################################################################
##################################################################
# 2A. What is the monthly total loan volume by average loan size?
##################################################################
##################################################################

Question2B = dbGetQuery(con, "SELECT count(*) AS loan_count,avg(loan_amnt) AS LOAN_VOLUME_BY_AVG_LOAN_SIZE,
  to_date(issue_d,'Mon YYYY') AS MONTH
                        FROM lending_club_2015
                        WHERE loan_amnt IS NOT NULL
                        GROUP BY MONTH
                        ORDER BY MONTH")
Question2B

#######################################################
# Question 2B R Output 
#######################################################
#> Question2B
#loan_count loan_volume_by_avg_loan_size      month
#1       35107                     15185.93 2015-01-01
#2       23770                     15435.78 2015-02-01
#3       25400                     15354.46 2015-03-01
#4       35427                     15225.71 2015-04-01
#5       31913                     15140.84 2015-05-01
#6       28485                     15087.84 2015-06-01
#7       45962                     15148.14 2015-07-01
#8       35886                     15474.88 2015-08-01
#9       28641                     15720.36 2015-09-01
#10      48631                     15180.06 2015-10-01
#11      37530                     15114.50 2015-11-01
#12      44343                     15062.37 2015-12-01
#######################################################

########################################################################################### 
###########################################################################################
###########################################################################################
###########################################################################################

# 3. What are the default rates by Loan Grade?

###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
# Total Number of Default accounts by loan grade
DF3 <- dbGetQuery(con, "SELECT count(*), grade FROM lending_club_2015 WHERE loan_status = 'Default' GROUP BY grade order by grade")

# Total Number of accounts by loan grade
DF <- dbGetQuery(con, "SELECT count(*), grade FROM lending_club_2015 GROUP BY grade order by grade")

defaultRate<- round((DF3$count/DF$count)*100,2)

DefaultGradesDf <- data.frame(DF3$grade,defaultRate)
names(DefaultGradesDf) <- c("Loan-Grade","DefaultRate")
DefaultGradesDf

#######################################################
# Question 3 R Output 
#######################################################
#> DefaultGradesDf
#Loan-Grade DefaultRate
#1          A        0.01
#2          B        0.06
#3          C        0.13
#4          D        0.18
#5          E        0.29
#6          F        0.37
#7          G        0.74
#######################################################


#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

# 4. Are we charging an appropriate rate for risk?

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

# total_pymnt             - Total Payments Received 
# total_rec_prncp         - Total Principal Received 
# total_rec_int           - Total Interest Received 
# total_rec_late_fee      - Late Fees
# recoveries              - post charge off gross recovery
# collection_recovery_fee - Recovery Service Charge 


# Actual Return on Investments - (total principal rcvd + total interest rcvd + late fees rcvd + recoveries - collection recovery fees - total principal lent) / total principal lent 
# Expected Return on Investments - (total expected interest rcvd) / total principal lent 

ExpectedReturnDF <- dbGetQuery(con, "select sum(installment * cast(split_part(term,' ',1) AS INTEGER)) AS expected_return, grade from lending_club_2015 WHERE loan_status IN ('Charged Off','Default', 'Fully Paid') group by grade order by grade")
TotalLoanLentDF <-  dbGetQuery(con, "select sum(loan_amnt) AS total_amount_lent, avg(int_rate) AS avg_int_rate, grade from lending_club_2015 WHERE loan_status IN ('Charged Off','Default', 'Fully Paid') group by grade order by grade")
TotalPrincipalRcvdDF <- dbGetQuery(con, "select sum(total_rec_prncp) AS total_principal_rcvd, grade from lending_club_2015 WHERE loan_status IN ('Charged Off','Default', 'Fully Paid') group by grade order by grade")
TotalInterestRcvdDF <- dbGetQuery(con, "select sum(total_rec_int) AS total_interest_rcvd, grade from lending_club_2015 WHERE loan_status IN ('Charged Off','Default', 'Fully Paid') group by grade order by grade")
TotalLateFeesRcvdDF <- dbGetQuery(con, "select sum(total_rec_late_fee) AS total_late_fees_rcvd, grade from lending_club_2015 WHERE loan_status IN ('Charged Off','Default', 'Fully Paid') group by grade order by grade")
TotalRecoveriesRcvdDF <- dbGetQuery(con, "select sum(recoveries) AS total_recoveries_rcvd, grade from lending_club_2015 WHERE loan_status IN ('Charged Off','Default', 'Fully Paid') group by grade order by grade")
TotalRecoveryServiceCharge <- dbGetQuery(con, "select sum(collection_recovery_fee) AS total_recovery_service_charge, grade from lending_club_2015 WHERE loan_status IN ('Charged Off','Default', 'Fully Paid') group by grade order by grade")
LoanGrade <- TotalLoanLentDF$grade
AvgInterestRate <- round(TotalLoanLentDF$avg_int_rate,2)

ActualPercentageReturn <- round((TotalPrincipalRcvdDF$total_principal_rcvd + TotalInterestRcvdDF$total_interest_rcvd + TotalLateFeesRcvdDF$total_late_fees_rcvd + TotalRecoveriesRcvdDF$total_recoveries_rcvd - TotalRecoveryServiceCharge$total_recovery_service_charge - TotalLoanLentDF$total_amount_lent)/TotalLoanLentDF$total_amount_lent,4)*100
ExpectedPercentageReturn <- round((ExpectedReturnDF$expected_return - TotalLoanLentDF$total_amount_lent)/TotalLoanLentDF$total_amount_lent,4)*100


# Total Loan Lent in year 2015 By Loan Grade 
TotalLoanLent2015DF <-  dbGetQuery(con, "select sum(loan_amnt) AS total_amount_lent, grade from lending_club_2015 group by grade order by grade")
TotalAmountPercentage <- round(TotalLoanLent2015DF$total_amount_lent/sum(TotalLoanLent2015DF$total_amount_lent),4)*100 
TotalAmountPercentage

Question4 <- as.data.frame(cbind(LoanGrade,AvgInterestRate,ExpectedPercentageReturn,ActualPercentageReturn,TotalAmountPercentage))
Question4

#######################################################
# Question 4 R Output 
#######################################################

#> Question4
#LoanGrade AvgInterestRate ExpectedPercentageReturn ActualPercentageReturn TotalAmountPercentage
#1         A            7.06                    11.62                   0.18                 16.79
#2         B           10.22                    19.27                  -2.79                 26.12
#3         C           13.39                    27.72                  -6.37                  27.7
#4         D           16.73                    37.86                 -13.14                 15.57
#5         E           19.45                    51.37                  -15.4                 10.06
#6         F           23.79                    69.42                 -21.04                  3.07
#7         G           26.51                    79.74                 -23.46                  0.69


#######################################################


# The above results show yearly return on different types of loans of year 2015 
# (I have included charged off, default and fully paid loans to calculate all columns except TotalAmountPercentage)
# (To calculate "TotalAmountPercentage" column, I have considered all kind of loans current, paid, charged off etc.)

# The results indicate that Landing Club is facing loss in loan type B to G, and it shows that risk associated with lower order loans (towards G)
# is higher, however if we look at total amount lent in year 2015(TotalAmountPercentage), it is clear that LC is providing more of the 
# higher order loans to minimize the risk 


#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

# 5. What are the top 5 predictors of default rate by order of importance? Explain the model that you used and discuss 
#   how you validated it

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

# I have used SQL Query to pull random sample data from the data table 

# Calculate total number of rows 
totalRowsDF <- dbGetQuery(con, "select count(*) AS totalrows from lending_club_2015")
totalRows <- totalRowsDF$totalrows

# Sampling row count (taking 10% sample out of the dataset )
N <- round(totalRows*0.10)

# Get a random sample (10% of total population)
SampleData <- dbGetQuery(con, paste("SELECT * FROM lending_club_2015 OFFSET floor(random()*",totalRows,") LIMIT",N))


############################################################################################
# Missing Value Treatment 
############################################################################################

# Get list of column names and missing value count 
MissingDF <- Missing(SampleData)

# There are total 21 columns where 50% of the data is missing 
length(MissingDF$Column_Name[MissingDF$Missing_Count > N*0.2])

# We need to drop those 21 columns 
# Create vector of column names (where more than 50% data is missing)
colVec <- as.vector(MissingDF[MissingDF$Missing_Count > N*0.2,1])

# Remove those columns  
SampleData[colVec] <- NULL

############################################################################################

# desc column is an empty column 
SampleData$desc <- NULL

############################################################################################

# Following columns are generated by LC, thus we should exclude 
excludeColVector <- c("id", "member_id", "funded_amnt", "funded_amnt_inv", "term", "installment", "grade", "sub_grade", "verification_status", "issue_d", "loan_status",
"pymnt_plan", "url", "initial_list_status", "out_prncp", "out_prncp_inv", "total_pymnt", "total_pymnt_inv", "total_rec_prncp", "total_rec_int", "recoveries", 
"collection_recovery_fee", "last_pymnt_d", "last_pymnt_amnt", "next_pymnt_d", "last_credit_pull_d", "policy_code", "verification_status_joint")


DefaultRateFeatures <- SampleData
DefaultRateFeatures[c(excludeColVector)] <- NULL

############################################################################################

# Zip Codes are encoded so we can remove that column 
DefaultRateFeatures$zip_code <- NULL

############################################################################################

# Find total number of possible values for nominal variables addr_state, home_ownership, purpose, title, emp_title from population

AddressStateLevelsDF <- dbGetQuery(con, "select DISTINCT addr_state from lending_club_2015")
addr_state_levels <- as.character(AddressStateLevelsDF$addr_state)

HomeOwnershipLevelsDF <- dbGetQuery(con, "select DISTINCT home_ownership from lending_club_2015")
home_ownership_levels <- as.character(HomeOwnershipLevelsDF$home_ownership)

PurposeLevelsDF <- dbGetQuery(con, "select DISTINCT purpose from lending_club_2015")
purpose_levels <- as.character(PurposeLevelsDF$purpose)

TitleLevelsDF <- dbGetQuery(con, "select DISTINCT title from lending_club_2015")
title_levels <- as.character(TitleLevelsDF$title)

EmpTitleLevelsDF <- dbGetQuery(con, "select DISTINCT emp_title from lending_club_2015")
emp_title_levels <- as.character(EmpTitleLevelsDF$emp_title)

# Too many nominal values for emp_title variable (114479 distinct values), we can drop that variable 
DefaultRateFeatures$emp_title <- NULL 

# Factor data Conversion 
DefaultRateFeatures$home_ownership <- factor(DefaultRateFeatures$home_ownership,levels = home_ownership_levels)
DefaultRateFeatures$purpose <- factor(DefaultRateFeatures$purpose, levels = purpose_levels)
DefaultRateFeatures$title <- factor(DefaultRateFeatures$title, levels = title_levels)
DefaultRateFeatures$addr_state <- factor(DefaultRateFeatures$addr_state,levels = addr_state_levels)

############################################################################################

# Check different values of application_type variable 
application_typeDF <- dbGetQuery(con, "select application_type from lending_club_2015")
application_type_levels <- factor(as.character(application_typeDF$application_type))
summary(application_type_levels)


# application_type variable, 99.87%  (420584/421095) values are "INDIVIDUAL" 
DefaultRateFeatures$application_type <- NULL

############################################################################################

# install.packages("stringr")
library(stringr)

# revol_util_updated (convert percentage string to numeric value)
DefaultRateFeatures$revol_util_updated <-  as.numeric(str_split_fixed(DefaultRateFeatures$revol_util, "%",2)[,1])

# remove revol_util from feature list 
DefaultRateFeatures$revol_util <- NULL 

############################################################################################

# create new variable earliest_cr_line_age
DefaultRateFeatures$earliest_cr_line_year <-  as.numeric(str_split_fixed(DefaultRateFeatures$earliest_cr_line, "-",2)[,2])
DefaultRateFeatures$earliest_cr_line_age <- 2016 - DefaultRateFeatures$earliest_cr_line_year

# remove earliest_cr_line from feature list 
DefaultRateFeatures$earliest_cr_line <- NULL 

############################################################################################

# Create new ordinal variable for emp_length 
DefaultRateFeatures$emp_length_updated <- ifelse(DefaultRateFeatures$emp_length=="< 1 year",0,
                                                 ifelse(DefaultRateFeatures$emp_length=="1 year",1,
                                                        ifelse(DefaultRateFeatures$emp_length=="2 years",2,
                                                               ifelse(DefaultRateFeatures$emp_length=="3 years",3,
                                                                      ifelse(DefaultRateFeatures$emp_length=="4 years",4,
                                                                             ifelse(DefaultRateFeatures$emp_length=="5 years",5,
                                                                                    ifelse(DefaultRateFeatures$emp_length=="6 years",6,
                                                                                           ifelse(DefaultRateFeatures$emp_length=="7 years",7,
                                                                                                  ifelse(DefaultRateFeatures$emp_length=="8 years",8,
                                                                                                         ifelse(DefaultRateFeatures$emp_length=="9 years",9,
                                                                                                                ifelse(DefaultRateFeatures$emp_length=="10+ years",10,NA)))))))))))


# remove original length variable from the feature list 
DefaultRateFeatures$emp_length <- NULL

############################################################################################

# Check missing values 
MissingDF <- Missing(DefaultRateFeatures)
MissingDF$Column_Name[c(25,26,29,34,35,46,51,58,61)]

# install.packages("mice")
library(mice)

# See missing value patterns 
md.pattern(DefaultRateFeatures[,c(25,26,29,34,35,46,51,58)])


# from the following pattern we can see that bc_open_to_buy, bc_util, mths_since_recent_bc and percent_bc_gt_75 are missing all togather 
md.pattern(DefaultRateFeatures[,c(25,26,34,51)])

# Let's use all bank card related fields to impute these four variables 
DefaultRateBankCardFeaturesDF <- DefaultRateFeatures[c("bc_open_to_buy","bc_util","mths_since_recent_bc","num_actv_bc_tl",
                                                 "num_bc_sats","num_bc_tl","percent_bc_gt_75","total_bc_limit")]

DefaultRateBankCardFeaturesDF  <- mice(data = DefaultRateBankCardFeaturesDF, m = 5, method = "pmm", maxit = 50, seed = 500)

# Save imputed data to the DefaultRateBankCardFeaturesDF dataframe 
DefaultRateBankCardFeaturesDF <- complete(DefaultRateBankCardFeaturesDF,2)

# Replace imputed values to DefaultRateFeatures dataframe 
DefaultRateFeatures$bc_open_to_buy <- DefaultRateBankCardFeaturesDF$bc_open_to_buy
DefaultRateFeatures$bc_util <- DefaultRateBankCardFeaturesDF$bc_util
DefaultRateFeatures$mths_since_recent_bc <- DefaultRateBankCardFeaturesDF$mths_since_recent_bc
DefaultRateFeatures$percent_bc_gt_75 <- DefaultRateBankCardFeaturesDF$percent_bc_gt_75

# remove DefaultRateBankCardFeaturesDF object from workspace 
remove(DefaultRateBankCardFeaturesDF)

############################################################################################

# See missing value patterns again
md.pattern(DefaultRateFeatures[,c(29,35,46,58,61)])

# we can impute missing revol_util_updated with mean values since it has only 13 missing values out of which 8 are independently missing as per missing pattern 
DefaultRateFeatures$revol_util_updated[is.na(DefaultRateFeatures$revol_util_updated)] <- mean(DefaultRateFeatures$revol_util_updated, na.rm=TRUE)

############################################################################################

# we can impute missing emp_length_updated with median 
DefaultRateFeatures$emp_length_updated[is.na(DefaultRateFeatures$emp_length_updated)] <- median(DefaultRateFeatures$emp_length_updated, na.rm=TRUE)

DefaultRateFeatures$emp_length_updated <- ordered(DefaultRateFeatures$emp_length_updated)

############################################################################################

# Check different values of num_tl_120dpd_2m variable 
num_tl_120dpd_2mDF <- dbGetQuery(con, "select num_tl_120dpd_2m from lending_club_2015")
num_tl_120dpd_2m_levels <- factor(as.character(num_tl_120dpd_2mDF$num_tl_120dpd_2m))
summary(num_tl_120dpd_2m_levels)
remove(num_tl_120dpd_2m_levels)
remove(num_tl_120dpd_2mDF)


# For variable num_tl_120dpd_2m, 95.36%(401577/421095) of rows are having value 0 in the 2015 data
# We can remove num_tl_120dpd_2m 
DefaultRateFeatures$num_tl_120dpd_2m <- NULL

############################################################################################

# we can impute missing mo_sin_old_il_acct with median 
DefaultRateFeatures$mo_sin_old_il_acct[is.na(DefaultRateFeatures$mo_sin_old_il_acct)] <- median(DefaultRateFeatures$mo_sin_old_il_acct, na.rm=TRUE)

############################################################################################

# we can impute missing mths_since_recent_inq with median 
DefaultRateFeatures$mths_since_recent_inq[is.na(DefaultRateFeatures$mths_since_recent_inq)] <- median(DefaultRateFeatures$mths_since_recent_inq, na.rm=TRUE)

############################################################################################

# logically following variables would not be one of top 5 predictors of interest rates due to higher number of distinct nominal values 
    # addr_state variable has total 49 levels
    # purpose variable has total 12 levels 
    # title variable has total 13 levels 

DefaultRateFeatures[c("addr_state","purpose","title")] <- NULL

############################################################################################

# We need to create dummy variables for home_ownership different levels 
DefaultRateFeatures$home_ownership_1 <- ifelse(DefaultRateFeatures$home_ownership=="MORTGAGE",1,0)
DefaultRateFeatures$home_ownership_2 <- ifelse(DefaultRateFeatures$home_ownership=="RENT",1,0)
DefaultRateFeatures$home_ownership_3 <- ifelse(DefaultRateFeatures$home_ownership=="OWN",1,0)
DefaultRateFeatures$home_ownership_4 <- ifelse(DefaultRateFeatures$home_ownership=="ANY",1,0)

# remove home_ownership
DefaultRateFeatures$home_ownership <- NULL


############################################################################################
# Write the preprocessed data frame to csv
write.csv(DefaultRateFeatures,"F:\\PayOff-Assignment\\DefaultRateFeatures.csv")


############################################################################################
# Make 75-25 Split for Training and Test Dataset 
############################################################################################

sampleSize <- floor(0.75 * nrow(DefaultRateFeatures))

# Set the seed so that partition is reproductible
set.seed(123)

train_ind <- sample(seq_len(nrow(DefaultRateFeatures)), size = sampleSize)

trainDefaultRateData <- DefaultRateFeatures[train_ind, ]
testDefaultRateData <- DefaultRateFeatures[-train_ind, ]

############################################################################################
# Modeling : XGBOOST
############################################################################################

# install.packages("xgboost")
library(xgboost)

trainDefaultRateData[] <- lapply(trainDefaultRateData, as.numeric)
testDefaultRateData[] <- lapply(testDefaultRateData, as.numeric)

cnt <- rep(0, nrow(testDefaultRateData))
control <- 50

for (i in 1:control){
  
  bst1 <- xgboost(data = as.matrix(trainDefaultRateData[,-c(2)]),
                  label = as.matrix(trainDefaultRateData$int_rate),
                  eta = 0.3,
                  max_depth = 8,
                  subsample = 0.5,
                  colsample_bytree = 1,
                  nrounds = 50,
                  objective = "reg:linear",
                  eval_metric = "rmse",
                  early.stop.round = 3)
  
  pred = predict(bst1,as.matrix(testDefaultRateData[,-c(2)]))
  cnt <- cnt + pred
}
cnt <- cnt/control

# Save Predictions in a new column in test dataset
testDefaultRateData$predRate = cnt

############################################################################################
# XGBOOST Model Performance & Identifying Top 10 Features 
############################################################################################

install.packages("ggplot2")
library(ggplot2)
library(dplyr)

# Copmute Importance Matrix 
importance_matrix <- xgb.importance(names(DefaultRateFeatures[,-c(2)]), model = bst1)
xgb.plot.importance(importance_matrix)


# Top 5 influential features 
ImportanceMatrixDf <- as.data.frame(importance_matrix)
topFeaturesDF <- arrange(ImportanceMatrixDf,desc(Gain))

topFeaturesDF$Gain <- round(topFeaturesDF$Gain,2)

topFeaturesDF[1:5,]

##################################################
# Question 5 Output
##################################################
#> topFeaturesDF[1:5,]
#Feature Gain      Cover  Frequence
#1          loan_amnt 0.13 0.10028300 0.11170882
#2     bc_open_to_buy 0.12 0.04977539 0.03914387
#3         annual_inc 0.07 0.04193332 0.06478196
#4 num_tl_op_past_12m 0.06 0.02643941 0.01510816
#5                dti 0.05 0.03624846 0.05367975

##################################################


############################################################################################
# Top Five predictors of default rate by order of importance 
############################################################################################

# 1) bc_open_to_buy     - Total open to buy on revolving bankcards

# 2) loan_amnt	        - The listed amount of the loan applied for by the borrower. If at some point in time, 
#                         the credit department reduces the loan amount, then it will be reflected in this value

# 3) annual_inc	        - The self-reported annual income provided by the borrower during registration

# 4) num_tl_op_past_12m - Number of accounts opened in past 12 months

# 5) dti                - A ratio calculated using the borrower's total monthly debt payments on the total debt 
#                         obligations, excluding mortgage and the requested LC loan, divided by the borrower's 
#                         self-reported monthly income

############################################################################################
# Disconnect conn object 
############################################################################################

dbDisconnect(con)


