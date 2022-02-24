## 1. Set up
# 1.1 Clear environment
rm(list=ls())

# 1.2 Load libraries
library(stringr)
library(tidyverse)
library(ggplot2)
library(RCurl)


## 2. Load data sets
# 2.1 Estimated Dog Population by Postcode District data set
# Read file
dog_link <- getURL("https://raw.githubusercontent.com/sofiariccomagno/msc_miniproject/main/Dog_Density_Postcode_District.csv")
dog <- read.csv(text = dog_link)
# Remove commas for thousands notation
dog$EstimatedDogPopulation <- as.character(dog$EstimatedDogPopulation)
dog$EstimatedDogPopulation <- gsub(",","",dog$EstimatedDogPopulation)
dog$EstimatedDogPopulation <- as.numeric(dog$EstimatedDogPopulation)

# 2.2 Gross Disposable Household Income (GDHI) Per Capita by Area Code data set
GDHI_link <- getURL("https://raw.githubusercontent.com/sofiariccomagno/msc_miniproject/main/GDHI_per_head.csv")
GDHI_per_capita <- read.csv(text = GDHI_link)
# Rename columns using values found in second row of data set
column_names <- as.character(GDHI_per_capita [2,])
names(GDHI_per_capita) <- column_names
# Remove unnecessary rows
GDHI_per_capita <- GDHI_per_capita[-c(1,2),]
# Remove unnecessary columns
GDHI_per_capita <- GDHI_per_capita[c("LAU1 code", "2015")]
# Rename columns to match across data sets
names(GDHI_per_capita) <- c("AreaCodes","GDHIPerCapita")

# 2.3 Mid Year Population Estimates by Area Code data set
population_link <- getURL("https://raw.githubusercontent.com/sofiariccomagno/msc_miniproject/main/Population.csv")
population <- read.csv(text = population_link)
# Rename columns using values found in second row of data set
column_names <- as.character(population [2,])
names(population) <- column_names
# Remove unnecessary rows
population <- population[-c(1,2),]
# Remove unnecessary columns
population <- population[,c(2,22)]
# Rename columns to match across data sets
names(population) <- c("AreaCodes", "Population")

# 2.4 Postcodes by Area Code data set
postcodes_link <- getURL("https://raw.githubusercontent.com/sofiariccomagno/msc_miniproject/main/ONSPD_FEB_2022_UK.csv")
postcodes <- read.csv(text = postcodes_link)
# Keep only postal district
postcodes$pcd <- substring(postcodes$pcd, 1, nchar(postcodes$pcd)-3)
# Remove duplicate rows
postcodes <- postcodes[!duplicated(postcodes), ]
# Remove rows with blank cells
postcodes <- postcodes %>% mutate_all(na_if,"")
postcodes <- na.omit(postcodes)
# Rename columns to match across data sets
names(postcodes) <- c("PostcodeDistrict", "AreaCodes")

# 2.5 Happiness by Area Code data set
happy_link <- getURL("https://raw.githubusercontent.com/sofiariccomagno/msc_miniproject/main/Happiness.csv")
happy <- read.csv(text = happy_link)
# Remove superfluous empty columns
happy <- happy[,c(1,2,6,7)]
# Remove empty rows
happy <- happy[!apply(happy == "", 1, all), ]
# Copy values of columns 1 and 2, row 4 into row 3 and delete row 4
copy <- as.character(happy[c(4), c(1,2)])
happy[c(3), c(1,2)] <- copy
happy <- happy[-4,]
# Rename columns using values found in third row of data set
column_names <- as.character(happy [3,])
names(happy) <- column_names
# Remove unnecessary rows with text
happy$`2014/15` <- as.numeric(as.character(happy$`2014/15`))
happy$`2015/16` <- as.numeric(as.character(happy$`2015/16`))
happy <- na.omit(happy)
# Average 2014/15 and 2015/16 data to get approx 2015 data
happy <- mutate(happy, Happiness = rowMeans(happy[,3:4]))
# Remove unnecessary columns
happy <- happy[,-c(2:4)]
# Rename columns to match across data sets
names(happy) <- c("AreaCodes", "Happiness")

# 2.6 Anxiety by Area Code data set
anxious_link <- getURL("https://raw.githubusercontent.com/sofiariccomagno/msc_miniproject/main/Anxiety.csv")
anxious <- read.csv(text = anxious_link)
# Remove superfluous empty columns
anxious <- anxious[,c(1,2,6,7)]
# Remove empty rows
anxious <- anxious[!apply(anxious == "", 1, all), ]
# Copy values of columns 1 and 2, row 4 into row 3 and delete row 4
copy <- as.character(anxious[c(4), c(1,2)])
anxious[c(3), c(1,2)] <- copy
anxious <- anxious[-4,]
# Rename columns using values found in third row of data set
column_names <- as.character(anxious [3,])
names(anxious) <- column_names
# Remove unnecessary rows with text
anxious$`2014/15` <- as.numeric(as.character(anxious$`2014/15`))
anxious$`2015/16` <- as.numeric(as.character(anxious$`2015/16`))
anxious <- na.omit(anxious)
# Average 2014/15 and 2015/16 data to get approx 2015 data
anxious <- mutate(anxious, Anxiety = rowMeans(anxious[,3:4]))
# Remove unnecessary columns
anxious <- anxious[,-c(2:4)]
# Rename columns to match across data sets
names(anxious) <- c("AreaCodes", "Anxiety")

# 2.7 Life Satisfaction by Area Code data set
satisfied_link <- getURL("https://raw.githubusercontent.com/sofiariccomagno/msc_miniproject/main/LifeSatisfaction.csv")
satisfied <- read.csv(text = satisfied_link)
# Remove superfluous empty columns
satisfied <- satisfied[,c(1,2,6,7)]
# Remove empty rows
satisfied <- satisfied[!apply(satisfied == "", 1, all), ]
# Copy values of columns 1 and 2, row 4 into row 3 and delete row 4
copy <- as.character(satisfied[c(4), c(1,2)])
satisfied[c(3), c(1,2)] <- copy
satisfied <- satisfied[-4,]
# Rename columns using values found in third row of data set
column_names <- as.character(satisfied [3,])
names(satisfied) <- column_names
# Remove unnecessary rows with text
satisfied$`2014/15` <- as.numeric(as.character(satisfied$`2014/15`))
satisfied$`2015/16` <- as.numeric(as.character(satisfied$`2015/16`))
satisfied <- na.omit(satisfied)
# Average 2014/15 and 2015/16 data to get approx 2015 data
satisfied <- mutate(satisfied, LifeSatisfaction = rowMeans(satisfied[,3:4]))
# Remove unnecessary columns
satisfied <- satisfied[,-c(2:4)]
# Rename columns to match across data sets
names(satisfied) <- c("AreaCodes", "LifeSatisfaction")

# 2.8 Create a single data set
# 2.8.1 Merge data sets that use postcodes
dog_postcode <- merge(dog,postcodes, by = "PostcodeDistrict")
# Add postcode populations with same area code together
dog_postcode <- dog_postcode %>% group_by(AreaCodes) %>%
  summarise(DogPopulation = sum(EstimatedDogPopulation)) 

# 2.8.2 Merge data sets that use area codes
happy_anxious <- merge(happy, anxious, by = "AreaCodes")
emotions <- merge(happy_anxious, satisfied, by = "AreaCodes")
emotion_money <- merge(emotions, GDHI_per_capita, by = "AreaCodes")
emotion_money_population <- merge(emotion_money, population, by = "AreaCodes")

# 2.8.3 Merge the data sets together to create a single data set
data <- merge(emotion_money_population, dog_postcode, by = "AreaCodes")

# 2.8.4 Estimated Dog Population Per Capita 
data <- mutate(data, Dog_per_capita = DogPopulation/Population)


## 3. Prepare data for Linear Regressions models
# 3.1 Response Variables
# 3.1.1 Happiness
hist(data$Happiness, breaks = "FD") # looks good
shapiro.test(data$Happiness) # double check

# 3.1.2 Anxiety
hist(data$Anxiety, breaks = "FD") # looks good
shapiro.test(data$Anxiety) # double check

# 3.1.3 Life Satisfaction
hist(data$LifeSatisfaction, breaks = "FD") # looks good
shapiro.test(data$LifeSatisfaction) # double check

# 3.2 Explanatory Variables
# 3.2.1 GDHI Per Capita
hist(data$GDHIPerCapita, breaks = "FD") # doesn't look good
shapiro.test(data$GDHIPerCapita) # double check
# Take the log to standardise the distribution
data$logGDHI <- log(data$GDHIPerCapita)
hist(data$logGDHI, breaks = "FD") # doesn't look good
shapiro.test(data$logGDHI) # double check
# Get Z-scores to remove outliers
data$z_scorelogGDHI <- (data$logGDHI-mean(data$logGDHI))/sd(data$logGDHI)
# Remove outliers
data <- subset(data, z_scorelogGDHI<3 &  z_scorelogGDHI>-3)
hist(data$logGDHI, breaks = "FD") # looks good
shapiro.test(data$logGDHI) # double check

# 3.2.2 Estimated Dog Population Per Capita
hist(data$Dog_per_capita, breaks = "FD") # doesn't look good
shapiro.test(data$Dog_per_capita) # double check
# Take the log to standardise the distribution
data$logDog <- log(data$Dog_per_capita) 
hist(data$logDog, breaks = "FD") # doesn't look good
shapiro.test(data$logDog) # double check
# Get Z-scores to remove outliers
data <- subset(data, Dog_per_capita>0)
data$z_scorelogDog <- (data$logDog-mean(data$logDog))/sd(data$logDog)
# Remove outliers
data <- subset(data, z_scorelogDog<3 &  z_scorelogDog>-3)
hist(data$logDog, breaks = "FD") # still not great :( but a little better at least
shapiro.test(data$logDog) # double check

# 3.2.3 Correlation between Explanatory Variables
# Plot
plot(data$logGDHI, data$logDog)
# Pearson's correlation test
cor.test(data$logGDHI, data$logDog, method = "pearson")


# 4. Classic Model Selection (Backward step wise selection)
# 4.1 Happiness Minimum Adequate Model
# 4.1.1 Happiness (Maximal) Model 1 (All Explanatory Variables + Interactions)
happy_model1 <- lm(Happiness ~ logGDHI + logDog + logGDHI:logDog, data = data)
plot(happy_model1)   
summary(happy_model1) # remove interaction (logGDHI:logDog)

# 4.1.2 Happiness Model 2 (All Explanatory Variables)
happy_model2 <- lm(Happiness ~ logGDHI + logDog, data = data)
plot(happy_model2)   
summary(happy_model2) # remove logGDHI because of higher Pr(>|t|)
# Compare Models via Analysis Of Variance (ANOVA)
anova(happy_model1,happy_model2) # Pr(>F)>0.05, RSS increase small
# Happiness Model 2 is not significantly worse than Happiness Model 1

# 4.1.3 Happiness Model 3
happy_model3 <- lm(Happiness ~ logDog, data = data)
plot(happy_model3)   
summary(happy_model3)
# Compare Models via Analysis Of Variance (ANOVA)
anova(happy_model2,happy_model3) # Pr(>F)>0.05, RSS increase small
# Happiness Model 3 is not significantly worse than Happiness Model 2
# Happiness Model 3 is our Minimum Adequate Model

# 4.1.4 Plot results of Happiness Minimum Adequate Model
ggplot(data, aes(x = logDog, y = Happiness)) + 
  geom_point() +
  labs(x = "log(Estimated Dog Population Per Capita)", y = "Happiness Score") +
  ggtitle("Happiness Minimum Adequate Model") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  stat_smooth(method = "lm", formula = y ~ x, color = "#7CAE00", se = FALSE)


## 4.2 Anxiety Minimum Adequate Model
# 4.2.1 Anxiety (Maximal) Model 1 (All Explanatory Variables + Interactions)
anxious_model1 <- lm(Anxiety ~ logGDHI + logDog + logGDHI:logDog, data = data)
plot(anxious_model1)   
summary(anxious_model1) # remove interaction (logGDHI:logDog)

# 4.2.2 Anxiety Model 2 (All Explanatory Variables)
anxious_model2 <- lm(Anxiety ~ logGDHI + logDog, data = data)
plot(anxious_model2)   
summary(anxious_model2) # remove logGDHI because of higher Pr(>|t|)
# Compare Models via Analysis Of Variance (ANOVA)
anova(anxious_model1,anxious_model2) # Pr(>F)>0.05, RSS increase small
# Anxiety Model 2 is not significantly worse than Anxiety Model 1

# 4.2.3 Anxiety Model 3
anxious_model3 <- lm(Anxiety ~ logDog, data = data)
plot(anxious_model3)   
summary(anxious_model3)
# Compare Models via Analysis Of Variance (ANOVA)
anova(anxious_model2,anxious_model3) # Pr(>F)>0.05, RSS increase small
# Anxiety Model 3 is not significantly worse than Anxiety Model 1
# Anxiety Model 3 is our Minimum Adequate Model

# 4.2.4 Plot results of Anxiety Minimum Adequate Model
ggplot(data, aes(x = logDog, y = Anxiety)) + 
  geom_point() +
  labs(x = "log(Estimated Dog Population Per Capita)", y = "Anxiety Score") +
  ggtitle("Anxiety Minimum Adequate Model") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  stat_smooth(method = "lm", formula = y ~ x, aes(colour = "red"), se = FALSE)


# 4.3 Life Satisfaction Minimum Adequate Model
# 4.3.1 Life Satisfaction (Maximal) Model 1 (All Explanatory Variables + Interactions)
satisfied_model1 <- lm(LifeSatisfaction ~ logGDHI + logDog + logGDHI:logDog, data = data)
plot(satisfied_model1)   
summary(satisfied_model1) # remove interaction (logGDHI:logDog)

# 4.3.2 Life Satisfaction Model 2 (All Explanatory Variables)
satisfied_model2 <- lm(LifeSatisfaction ~ logGDHI + logDog, data = data)
plot(satisfied_model2)   
summary(satisfied_model2)
# Compare Models via Analysis Of Variance (ANOVA)
anova(satisfied_model1,satisfied_model2) # Pr(>F)>0.05, RSS increase small
# Life Satisfaction Model 2 is not significantly worse than Life Satisfaction Model 1

# 4.3.3 Life Satisfaction Model 3
satisfied_model3 <- lm(LifeSatisfaction ~ logDog, data = data)
plot(satisfied_model3)   
summary(satisfied_model3)
# Compare Models via Analysis Of Variance (ANOVA)
anova(satisfied_model2,satisfied_model3) # Pr(>F)<0.05, RSS increase not small
# Life Satisfaction Model 3 is significantly worse than Life Satisfaction Model 2
# Life Satisfaction Model 2 is our Minimum Adequate Model

# 4.3.4 Plot results of Life Satisfaction Minimum Adequate Model
ggplot(data,aes(y=LifeSatisfaction,x=logDog,color=logGDHI)) +
  geom_point() +
  labs(x = "log(Estimated Dog Population Per Capita)", y = "Life Satisfaction Score") +
  ggtitle("Life Satisfaction Minimum Adequate Model") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_smooth(method="lm",se=FALSE)


## 5. Information Theoretic (IT) Approach
# Make function
IT_approach <- function(x,y,z){
  AICs<- AIC(x,y,z)
  DoF<- AICs[,1]
  AICsNum<- AICs[,2]
  minAW<- min(AICsNum) 
  Delta <- AICsNum-minAW
  RL <- exp(-0.5*Delta) 
  wi <- RL/sum(RL)
  AkaikeWeights_table<- data.frame(Model=1:3,DoF=DoF, AIC=round(AICsNum, digits = 2),
                                     AICDifferences= round(Delta, digits = 2),
                                     AkaikeWeights=round(wi, digits=2))
  return(AkaikeWeights_table)
}

# 5.1 Happiness IT Approach
happiness_it <- IT_approach(happy_model1,happy_model2,happy_model3)
happiness_it
# For a large number of repeats of the experiment, in 43% of cases Happiness Model 2 would be the optimal model, 
# and in 38% of cases Happiness Model 3 would the optimal model.

# 5.2 Anxiety IT Approach
anxiety_it <- IT_approach(anxious_model1,anxious_model2,anxious_model3)
anxiety_it
# For a large number of repeats of the experiment, in 66% of cases Anxiety Model 3 would be the optimal model, 
# and in 24% of cases Anxiety Model 3 would the optimal model.

# 5.3 Life Satisfaction IT Approach
life_satisfaction_it <- IT_approach(satisfied_model1,satisfied_model2,satisfied_model3)
life_satisfaction_it
# For a large number of repeats of the experiment, in 71% of cases Life Satisfaction Model 2 would be the optimal model, 
# and in 26% of cases Life Satisfaction Model 1 would the optimal model.