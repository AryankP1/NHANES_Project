library("haven")
library("dplyr")
library("ggpubr")
library("ggplot2")
library("foreign")

setwd("/Users/aryankbhargava/Documents/Project")

# extract "Ever told you had high blood pressure" from BPQ_J.XPT
bp <- read_xpt("BPQ_J.XPT")
bp_sub <- bp[, c("SEQN", "BPQ020", "BPQ080")]
names(bp_sub) <- c("SEQN","High_BP", "Highchol")

# extract Weight, Standing Height (cm), Arm Circumference(cm), Hip Circumference(cm), and BMI from BMX_J.XPT
body_m <- read_xpt("BMX_J.XPT")
body_m_sub <- body_m[, c("SEQN", "BMXWT", "BMXHT", "BMXARML", "BMXARMC", "BMXWAIST", "BMXHIP", "BMXBMI")]
names(body_m_sub) <- c("SEQN", "Weight", "Height", "Arm_length", "Arm_circumference", "Waist_circumference", "Hip_circumference", "BMI")

# extract "How healthy is the diet" and "How often drank milk age 18-35?" from DBQ_J.XPT
dbqs <- read_xpt("DBQ_J.XPT")
dbqs_sub <- dbqs[, c("SEQN", "DBQ700", "DBQ235C")]
names(dbqs_sub)[2] <- "Diet"
names(dbqs_sub)[3] <- "Milk_age"


dbts <- read_xpt("DIQ_J.XPT")
dbts_sub <- dbts[, c("SEQN", "DIQ010")]
names(dbts_sub)[2] <- "Diabetes"

# extract "Past 12 mo how often have alcohol drink" and "days have 4 or 5 drinks/past 12 mos" from ALQ_J.XPT
alcohol_use <- read_xpt("ALQ_J.XPT")
alcohol_use_sub <- alcohol_use[, c("SEQN", "ALQ121", "ALQ142", "ALQ130", "ALQ290")]
names(alcohol_use_sub)[2] <- "Alcohol_consumption"
names(alcohol_use_sub)[3] <- "Alcohol_days"
names(alcohol_use_sub)[4] <- "Alcohol_drinkspday"
names(alcohol_use_sub)[5] <- "Alcohol_twelvemonths"

# merge all data into one matrix
merged_file <- inner_join(bp_sub, body_m_sub, by = "SEQN") %>% 
  inner_join(dbqs_sub, by = "SEQN") %>%
  inner_join(dbts_sub, by = "SEQN") %>% 
  inner_join(alcohol_use_sub, by = "SEQN")

# record those with High blood pressure as 1, those without High blood pressure as 0, those with uncertainty or refusal to answer as NA and exclude
merged_file$High_BP <- ifelse(merged_file$High_BP %in% c(1, 2), merged_file$High_BP - 1, NA)

# remove entries such as ‘Refused’, ‘Don't know’
merged_file$Alcohol_consumption[merged_file$Alcohol_consumption %in% c(777, 999, ".")] <- NA
merged_file$Alcohol_days[merged_file$Alcohol_days %in% c(777, 999, ".")] <- NA
merged_file$Weight[merged_file$Weight == "."] <- NA
merged_file$Height[merged_file$Height == "."] <- NA
merged_file$Arm_length[merged_file$Arm_length == "."] <- NA
merged_file$Arm_circumference[merged_file$Arm_circumference == "."] <- NA
merged_file$Waist_circumference[merged_file$Waist_circumference == "."] <- NA
merged_file$Hip_circumference[merged_file$Hip_circumference == "."] <- NA
merged_file$BMI[merged_file$BMI == "."] <- NA

# add column Overweight: 1 if BMI > 30, else 0
merged_file$Overweight <- ifelse(merged_file$BMI > 30, 1, 0)

# omit all the missing values (NA) from this matrix
final_file <- na.omit(merged_file[, c("High_BP", "Weight", "Height", "Arm_length", "Arm_circumference", "Waist_circumference", "Hip_circumference", "BMI", "Alcohol_consumption", "Diabetes","Highchol", "Alcohol_days", "Diet", "Milk_age","Alcohol_drinkspday","Alcohol_twelvemonths")])

# use the "summary()" function to print the results
summary(final_file)

final_file$Diabetes <- ifelse(final_file$Diabetes == 1, 1, 0)
final_file<-final_file%>%mutate(Overweight=if_else(BMI>30,1,0))

#Correlation between BMI and Height
cor.test(final_file$BMI,final_file$Height)
# Create a scatter plot of BMI and Height
plot(x = final_file$BMI, y = final_file$Height, 
     main = "Scatter Plot of BMI and Height",
     xlab = "BMI", ylab= "Height")
abline(lm(final_file$Height ~ final_file$BMI), col = "orange")

#Correlation between Alcohol Consumption and Cholesterol  
cor.test(final_file$Alcohol_consumption,final_file$Highchol)
# Create a scatter plot of alcohol consumption and Cholesterol
plot(x = final_file$Alcohol_consumption, y = final_file$Highchol, 
     main = "Scatter Plot of Alcohol Consumption and Cholesterol Levels",
     xlab = "Alcohol consumption", ylab= "Cholesterol")
abline(lm(final_file$Highchol ~ final_file$Alcohol_consumption), col = "skyblue")

#Correlation between BMI and Alcohol consumption
cor.test(final_file$Alcohol_consumption,final_file$BMI)
# Create a scatter plot of alcohol consumption and BMI
plot(x = final_file$Alcohol_consumption, y = final_file$BMI, 
     main = "Scatter Plot of Alcohol Consumption and BMI",
     xlab = "Alcohol consumption", ylab= "BMI")
abline(lm(final_file$BMI ~ final_file$Alcohol_consumption), col = "gold")

#Correlation between BMI and Cholesterol
cor.test(final_file$BMI,final_file$Highchol)
# Create a scatter plot of BMI and Cholesterol
plot(x = final_file$BMI, y = final_file$Highchol, 
     main = "Scatter Plot of BMI and Cholesterol Levels",
     xlab = "BMI", ylab= "Cholesterol")
abline(lm(final_file$Highchol ~ final_file$BMI), col = "magenta")

# Logistic Regression model
logit_model_1 <- glm(Diabetes ~ Height + Waist_circumference + High_BP, family = binomial(link = "logit"), data = final_file)
summary(logit_model_1)

logit_model_2 <- glm(Diabetes ~ Overweight, family = binomial(link = "logit"), data = final_file)
summary(logit_model_2)

logit_model_3 <- glm(Diabetes ~ Highchol, family = binomial(link = "logit"), data = final_file)
summary(logit_model_3)

logit_model_4 <- glm(Diabetes ~ Alcohol_drinkspday, family = binomial(link = "logit"), data = final_file)
summary(logit_model_4)

logit_model_5 <- glm(Diabetes ~ Alcohol_twelvemonths, family = binomial(link = "logit"), data = final_file)
summary(logit_model_5)

# BMI V/S Diabetes Relation
logit_model_6 <- glm(Diabetes ~ BMI, family = binomial(link = "logit"), data = final_file)
summary(logit_model_6)
BMI_range <- seq(from=0, to=150, by=0.1)
Temp.df<- data.frame(BMI = BMI_range)
Temp.df$Diabetes <- predict(logit_model_6, newdata = Temp.df, type = "response")
ggplot(Temp.df, aes(x = BMI, y = Diabetes)) + geom_line() + ggtitle("Diabetes v/s BMI") + ylab("Diabetes") + xlab("BMI") + ylim(0, 1)

# Distribution of Body Weight
wt <- ifelse(final_file$Diabetes == 1, final_file$Weight, NA_integer_)
wt <- na.omit(wt)
boxplot(wt, main = "Distribution of Body Weight", ylab = "Weights", ylim = c(40, 140), border = par("fg"), col = "red")
summary(wt)

## Distribution of Body Mass Index
BodyMI<-ifelse(final_file$Diabetes==1,final_file$BMI,NA_integer_)
BodyMI<-na.omit(BodyMI)
BodyMI_frame<-data.frame(BodyMI)
BodyMI_hist <- ggplot(BodyMI_frame, aes(BodyMI)) + 
  geom_histogram(fill = 'green', color = 'purple', breaks = seq(10,70,2)) + 
  labs(title = 'BMI HISTOGRAM', x = 'BMI', y = 'Number of Samples') + 
  xlim(10, NA)
summary((BodyMI))
BodyMI_hist

