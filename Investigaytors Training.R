##This training material provides an introduction into descriptive analyses in R using the Sex Now 2014/15 data.

##Read in Data set - Note that the file path uses double slashes, which you will have to add.
data <- read.csv(file = "C:\\Users\\Kiffer\\Desktop\\sn1415.csv")

#Exploring your data
rownames(data) ## Will print the row names
colnames(data) ## Will print the column names
View(data) ## Will Open your dataset in a new window.

#Getting help with a function (e.g., Mean)
?mean ##Opens up the R documentation about the mean function in the Help Viewer (Bottom left panel).

#Demonstrating how syntax works
mean(x = data$Q60_Age, trim = 0, na.rm = TRUE) ##Includes all arguments and argument identifiers
mean(data$Q60_Age, 0, TRUE) ##Includes all arguments, but not the argument identifiers
mean(data$Q60_Age) ##includes only the object argument, will not run correctly due to missingness in the variable
mean(data$Q60_Age, na.rm = TRUE) ## Thus, you must override the default and exclude missing using the na.rm argument.
mean() ##if you include no arguments, R will return an error.

##Calculating Frequencies
table(data$Q29_GHB) ##Provides the frequency for the GHB variable

##Crosstabulating two variables
table(GHB_Use = data$Q29_GHB, HIV_Status = data$Q27_HIVStatus) ##Crosstabulates the frequencies of the GHB and HIV variable

##Calculating Percentages
prop.table(table(data$Q29_GHB)) ##Provides the percentages for the GHB variable

##Crosstabulating two variables
prop.table(table(GHB_Use = data$Q29_GHB, HIV_Status = data$Q27_HIVStatus), margin = 1) ##Crosstabulates the frequencies of the GHB and HIV variable
#Margin = 1 calculated row percentages, margin = 2 calculates column percentages

##Measures of Central Tendency
mean(data$Q22_AgeFirstSex, na.rm = TRUE) ##Calculates the Mean
median(data$Q22_AgeFirstSex, na.rm = TRUE) ##Calculates the median
summary(data$Q22_AgeFirstSex, na.rm = TRUE) ##Calculates summary statistics

##Measures of Spread
range(data$Q22_AgeFirstSex, na.rm = TRUE) ##Calculates the minimum and maximum values
IQR(data$Q22_AgeFirstSex, na.rm = TRUE) ##Calculates the width of the interquartile range (IQR or Q1-Q3, usually reported with Median)
sd(data$Q22_AgeFirstSex, na.rm = TRUE) ##calculates the standard deviation (usually reported with mean)

## Student's T-test (for equal variances and normally distributed data)
t.test(Q22_AgeFirstSex ~ Q2_SexualID_Gay, alternative = 'two.sided', var.equal = TRUE, data = data)

## Welche's T-test (for unequal variances)
t.test(Q22_AgeFirstSex ~ Q2_SexualID_Gay, alternative = 'two.sided', var.equal = FALSE, data = data)

##Mann-Whitney Test (for non-normally distributed continuous variable)
mwt <- wilcox.test(Q22_AgeFirstSex ~ Q2_SexualID_Gay, data = data)
mwt

##Test for normality
levels(data$Q29_Marijuana_Ever) ## Will tell you the levels for the grouping variable
shapiro.test(data$Q22_AgeFirstSex[data$Q29_Marijuana_Ever == 'Ever'])
shapiro.test(data$Q22_AgeFirstSex[data$Q29_Marijuana_Ever == 'No'])
## The null hypothesis that the data are normally distributed is rejected if the p-value is less than 0.05. 
## If the p-value is greater than 0.05, then the null hypothesis is not rejected.

##Test for equal variances of a normal variable
res.ftest <- var.test(Q22_AgeFirstSex ~ Q2_SexualID_Gay, data = data)
res.ftest 
## The null hypothesis of equal variances is rejected if the p-value is less than 0.05
## If the p-value is greater than 0.05, then the null hypothesis is not rejected.
##Note: If you have more than two samples the bartlett.test function can be used.

##Test for equal variances of a non-normal variable
library(car)
res.htest <- leveneTest(Q22_AgeFirstSex ~ Q2_SexualID_Gay, data = data)
res.htest 
## The null hypothesis of equal variances is rejected if the p-value is less than 0.05
## If the p-value is greater than 0.05, then the null hypothesis is not rejected.

##Chi-square test test the association between two or more categorical variables
chisq.test(x = data$Q2_SexualID_Gay,y = data$Q29_Marijuana_Ever)

##Correlation Tests
res.corP <- cor.test(data$Q22_AgeFirstSex, data$Q23_AgeCameOut, method = 'pearson')
res.corP ##Provides output pased on the pearson's correlation
res.corS <- cor.test(data$Q22_AgeFirstSex, data$Q23_AgeCameOut, method = 'spearman')
res.corS ##Provides output pased on the pearson's correlation
res.corK <- cor.test(data$Q22_AgeFirstSex, data$Q23_AgeCameOut, method = 'kendall')
res.corK ##Provides output pased on the Kendall's correlation

##Removing Specific Variables
dataR1 <- data[, -3] ##Removes the third variable in the dataset

##Removing Specific Observations
dataR2 <- data[-3, ] ##Removes the third observation in the dataset

##Subsetting Observations
levels(data$Q27_HIVStatus) ## Will tell you the levels for the grouping variable
dataR3_Neg <- data[data$Q27_HIVStatus == "HIV-negative (2)", ] ##moves only these observations into the new dataset
dataR3_Poz <- data[data$Q27_HIVStatus == "HIV-positive (1)", ] ##moves only these observations into the new dataset
dataR3_Untested <- data[data$Q27_HIVStatus == "I've never had an HIV test (or a test result) (3)", ] ##moves only these observations into the new dataset

##Recoding a categorical Variable
data$NewHIVVar <- NA
data$NewHIVVar[data$Q27_HIVStatus == "HIV-negative (2)"] <- "HIV-negative/unknown (0)"
data$NewHIVVar[data$Q27_HIVStatus == "HIV-positive (1)"] <- "HIV-Positive (1)"
data$NewHIVVar[data$Q27_HIVStatus == "I've never had an HIV test (or a test result) (3)"] <- "HIV-negative/unknown (0)"
##Check your work to make sure the NewHIVVar captures the numbers correctly
table(data$NewHIVVar) ##e.g., HIV Negative = 7316 which is the sum of the negatives and unknowns
table(data$Q27_HIVStatus)

##Categorizing a continuous variable
data$AgeGroups <- NA
data$AgeGroups[data$Q60_Age <= 30] <- "Less than or equal to 30"
data$AgeGroups[data$Q60_Age > 30] <- "Greater than 30"
##Check your work using a crosstabulation
table(data$AgeGroups, data$Q60_Age) ##You can check using a tabble function.
tapply(data$Q60_Age, data$AgeGroups, summary) ##You can check using tapply as well.

##Combining two variables
levels(data$Q27_HIVStatus) ##Print the levels for each variable being combined
levels(data$Q27_ViralLoad)
data$HIVStatVL <- NA ##Create an empty column for the new variable
data$HIVStatVL[data$Q27_HIVStatus == "HIV-negative (2)"] <- "Negative_Unknown"
data$HIVStatVL[data$Q27_HIVStatus == "I've never had an HIV test (or a test result) (3)"] <- "Negative_Unknown" 
data$HIVStatVL[data$Q27_HIVStatus == "HIV-positive (1)" & data$Q27_ViralLoad == "Detectable (4)"] <- "Poz_Detectable" 
data$HIVStatVL[data$Q27_HIVStatus == "HIV-positive (1)" & data$Q27_ViralLoad == "Undetectable (3)"] <- "Poz_Undetectable" 
data$HIVStatVL[data$Q27_HIVStatus == "HIV-positive (1)" & data$Q27_ViralLoad == "I didn't get a viral load test (1)"] <- "Poz_No VL Test or dont known" 
data$HIVStatVL[data$Q27_HIVStatus == "HIV-positive (1)" & data$Q27_ViralLoad == "I don't know (2)"] <- "Poz_No VL Test or dont known" 
##Check your work
table(HIVStatVL)
table(HIVStatVL, data$Q27_HIVStatus, data$Q27_ViralLoad)

##Data Visualizations - Bar Charts
counts <- table(data$HIVStatVL) 
barplot(counts, main = "Participants by HIV Status and Viral Load",
        xlab = "number of participants", ylab = "HIV Status and Viral Load", )

##Data Visualizations - Histograms
hist(data$Q22_AgeFirstSex, main = "Histogram of Age", 
     xlab = "Age at First Sex (in years)", ylab = "Number of Participants")

