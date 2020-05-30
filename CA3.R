library(dplyr)
library(lattice)
library(ggpubr)
library(pwr)

data <- read.csv("C:/Users/intel/Desktop/data scince pdf/Health.csv")
#data <- read.csv("Health.csv")
str(data)

data <- subset(data, select = c(TotalDischargesMale, TotalDischargesFemale, Year))

# to view the dataset in a tab
View(data)

# to view the top columns of dataset
head(data)

# to view the structure of dataset
str(data)


# Converting the data type from factor to numeric
data$TotalDischargesFemale <- as.numeric(gsub(",","",data$TotalDischargesFemale))
data$TotalDischargesMale <- as.numeric(gsub(",","",data$TotalDischargesMale))
data$Year <- as.numeric(data$Year)

# Omitting all the null values
data[data == "0"] <- NA
data <- na.omit(data)
view(data)
str(data)

# Selecting only Female Discharges by using Subsets
Female <- subset(data, select=c(TotalDischargesFemale,Year))
Female

# Selecting only Male Discharges by using Subsets
Male <- subset(data, select=c(TotalDischargesMale,Year))
Male


# To identify data distribution for Female discharges using Histogram
histogram(~TotalDischargesFemale  |  Year, data = Female, col= "olive")

# histogram graph for finding the distribution of data for MaleDischarges
histogram(~TotalDischargesMale  |  Year, data = Male, col="black")
qqnorm(Male$TotalDischargesMale, col="blue")


# To Check normality value for FeMale Discharges
ggqqplot(Female$TotalDischargesFemale, ylab="Total discharges Female by years",col= "olive", ggtheme = theme_minimal())

# To check normality value for Male Discharges
ggqqplot(Male$TotalDischargesMale, ylab="Total discharges Male by years", col= "gray")


#  Performing Shapiro Test - normality test - Female discharges
normality_test <- shapiro.test(Female$TotalDischargesFemale[0:236])
normality_test$p.value

# Performing Shapiro Test - normality test - Male discharges
normality_test <- shapiro.test(Male$TotalDischargesMale[0:236])
normality_test$p.value

# Filtering the Subsets for Female
DischargeFemale <- subset(Female, select=c(TotalDischargesFemale))
DischargeFemale

# Filtering the Subsets for Male
DischargeMale <- subset(Male, select=c(TotalDischargesMale)) 
DischargeMale

class(DischargeMale)
DischargeMale <-na.omit(DischargeMale)
DischargeFemale <-na.omit(DischargeFemale)

# Performing wilcoxon test and converting the data type to Numeric
DischargeMale <- as.numeric(DischargeMale$TotalDischargesMale)
DischargeFemale <- as.numeric(DischargeFemale$TotalDischargesFemale)

# Performing wilcoxon test- non - parameteric and not normally distributed data
wilcox.test <-wilcox.test(DischargeMale,DischargeFemale)
wilcox.test

#p-value is 0.89 from wilcoxon test
# The result from the above test shows that the  P values is more that 0.05 so we accept the null hypothesis
# Power Analysis test for sample size dtermination
# To find the mean value for Female Discharges
mean(DischargeFemale, each=234)

# To find the mean value for Male Discharges
mean(DischargeMale,each=234)

MaleDischargecount <-(DischargeMale [0:234])
FemaleDischargecount <- (DischargeFemale [0:234])

datanew <- rbind(MaleDischargecount,FemaleDischargecount)
datanew

# to find SD for Delta value
sdev=sd(datanew)

# delta value(D value) was found by substracting the mean value and dividing it by the SD
m=mean(datanew)
delta=(sdev-m)/sdev
delta
# SD = 28253 AND D value is 0.71 
# Power Test for determining the sample size
power_test <- power.t.test(delta = 0.71, n=NULL, sig.level = .06, power =.90, type="two.sample", alternative = "two.sided")

power_test

plot(power_test)

# The results  test shows the  sample size  - n was found to be 5838.4

Power.V = power.t.test(n= 5838.4, delta = 0.71, sd =1,type = c("two.sample"))
Power.V

# Performing  Cohens test 
cohen.ES(test = c("t"), size = c("small"))
data1 <- DischargeMale[0:85]
data2<- DischargeFemale[0:85]

#  Performing Correlation test 
correlation_t <- cor.test(data1,data2,method = "spearman")
correlation_t

#  The  rho value was found as 0.90 from correlation test which very strong correlation 


