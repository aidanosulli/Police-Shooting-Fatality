
library(tidyverse)
library(Amelia)
library(randomForest)
library(corrplot)
library(leaps)
library(caret)
library(randomForest)
library(gbm)
library(xgboost)
library(data.table)
library(mlr)
library(parallel)
library(parallelMap) 
library(mice)


full_train <- read.csv("PoliceTrainNew.csv")
dim(full_train)
full_test <- read.csv("PoliceTestNoYNew (2).csv")
dim(full_test)


#First clean Subject Race and Subject Gender

for(i in 1:nrow(full_train)){
  if(is.na(full_train$SubjectRace[i])){
    next
  }
  if(full_train$SubjectRace[i] == "U"){
    full_train$SubjectRace[i] <- NA
  }
}

for(i in 1:nrow(full_train)){
  if(is.na(full_train$SubjectGender[i])){
    next
  }
  if(full_train$SubjectGender[i] == "U"){
    full_train$SubjectGender[i] <- NA
  }
}

for(i in 1:nrow(full_train)){
  if(is.na(full_train$SubjectGender[i])){
    next
  }
  if(full_train$SubjectGender[i] == "N/A"){
    full_train$SubjectGender[i] <- NA
  }
}

for(i in 1:nrow(full_train)){
  if(is.na(full_train$SubjectGender[i])){
    next
  }
  if(full_train$SubjectGender[i] == "M;U"){
    full_train$SubjectGender[i] <- NA
  }
}


#Next, let's clean Subject Armed and Officer Gender


for(i in 1:nrow(full_train)){
if(is.na(full_train$SubjectArmed[i])){
next
}
if(full_train$SubjectArmed[i] == "U"){
full_train$SubjectArmed[i] <- NA
}
}



### Cleaning Office Gender

full_train[which(full_train$OfficerGender == "F"),]$OfficerGender <- "Female"
full_train[which(full_train$OfficerGender == "F;F"),]$OfficerGender <- "Female"
full_train[which(full_train$OfficerGender == "F;F;M;F"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "F;F;M;F;M;F"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "F;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "F;M;F;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "F;M;F;M;M;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "F;M;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "F;M;M;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "F;M;M;M;F;F"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "F;M;M;M;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "F;M;M;M;M;M;M;M;M;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "F;Unknown;Unknown"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender == "FEMALE"),]$OfficerGender <- "Female"
full_train[which(full_train$OfficerGender == "FEMALE;MALE"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender == "M: M;M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender == "M: M;M;M;M;M;M;M;M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender == "M:M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender == "M:M;M;M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender == "M;F"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "M;F;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "M;F;M;M;M;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "M;F;M;M;M;M;M;M;M;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "M;F;Unknown"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender == "M;M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender == "M;M;F"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "M;M;F;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "M;M;F;M;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "M;M;F;M;M;F"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "M;M;M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender == "M;M;M;F"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "M;M;M;F;M;M;F;M;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "M;M;M;F;M;M;M;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender == "M;M;M;M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender == "M;M;M;M;/M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender == "M;M;M;M;F"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender ==  "M;M;M;M;F;M;M;M;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender ==  "M;M;M;M;M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender ==  "M;M;M;M;M;M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender ==  "M;M;M;M;M;M;F;F;M;M;M;M;M;M;M;M"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender ==  "M;M;M;M;M;M;M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender ==  "M;M;M;M;M;M;M;M;M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender ==  "M;M;M;M;M;M;M;M;M;M;"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender ==  "M;M;M;M;M;M;M;M;M;M;M;M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender ==  "M;M;M;M;M;M;M;M;M;M;M;M;M;M;M;M"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender ==  "M;M;M;U;U"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "M;M;U;M;M"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "M;M;U;U;U;U"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "M;M;Unknown;Unknown"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "M;M;Unknown;Unknown;Unknown"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "M;M;Unknown;Unknown;Unknown;Unknown"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "M;U"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "M;U;U"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "M;U;U;U"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "M;Unknown"),]$OfficerGender <- NA 
full_train[which(full_train$OfficerGender ==  "M;Unknown;Unknown"),]$OfficerGender <- NA 
full_train[which(full_train$OfficerGender ==  "MALE"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender ==  "MALE;FEMALE;MALE"),]$OfficerGender <- "Mixed"
full_train[which(full_train$OfficerGender ==  "MALE;MALE"),]$OfficerGender <- "Male"
full_train[which(full_train$OfficerGender ==  "N"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "U"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "U;U"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "U;U;M;M"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "U;U;U"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "U;U;U;U"),]$OfficerGender <- NA
full_train[which(full_train$OfficerGender ==  "W;M;M"),]$OfficerGender <- "Mixed"
full_train$OfficerGender <- as.factor(full_train$OfficerGender)
levels(full_train$OfficerGender)



#Next up, let's Clean Officer Race

full_train$OfficerRace <- as.vector(full_train$OfficerRace)

for (i in 1:nrow(full_train)) {
  if (!is.na(full_train$OfficerRace[i])) {
    if (str_detect(as.character(full_train$OfficerRace[i]), 'U')) {
      full_train$OfficerRace[i] <- NA
    }
  }
}

full_train[which(full_train$OfficerRace ==  "BLACK"),]$OfficerRace <- "B"
full_train[which(full_train$OfficerRace ==  "WHITE"),]$OfficerRace <- "W"
full_train[which(full_train$OfficerRace ==  "NA/W"),]$OfficerRace <- NA

for (i in 1:nrow(full_train)) {
  if (!is.na(full_train$OfficerRace[i])) {
    val <- as.character(full_train$OfficerRace[i])
    if (nchar(val) > 1) {
      if (str_detect(val, ';')) {
        l <- strsplit(as.character(full_train$OfficerRace[i]), ";") 
      }
      else if (str_detect(val, '/')) {
        l <- strsplit(as.character(full_train$OfficerRace[i]), "/") 
      }
      first <- l[[1]][1]
      check <- TRUE
      #print(first)
      #print(i)
      for (j in 2:length(l[[1]])) {
        if (l[[1]][j] != first) {
          check <- FALSE
          break
        }
      }
      if (check) {
        full_train$OfficerRace[i] <- first
      }
      else {
        full_train$OfficerRace[i] <- 'Mixed'
      }
    }
  }
}

full_train$OfficerRace <- as.factor(full_train$OfficerRace)
full_train$OfficerRace <- droplevels(full_train$OfficerRace)


#New addition, let's clean Subject Age


for (i in seq_along(full_train$SubjectAge)) {
  if (!is.na(full_train$SubjectAge[i]))  {
    if (str_detect(as.character(full_train$SubjectAge[i]), '0-19')) {
      full_train$SubjectAge[i] = round(runif(1, 15, 19), digits = 0)
    }
  }
  if (!is.na(full_train$SubjectAge[i]))  {
    if (str_detect(as.character(full_train$SubjectAge[i]), '20-29')) {
      full_train$SubjectAge[i] = round(runif(1, 20, 29), digits = 0)
    }
  }
  if (!is.na(full_train$SubjectAge[i]))  {
    if (str_detect(as.character(full_train$SubjectAge[i]), '30-39')) {
      full_train$SubjectAge[i] = round(runif(1, 30, 39), digits = 0)
    }
  }
  if (!is.na(full_train$SubjectAge[i]))  {
    if (str_detect(as.character(full_train$SubjectAge[i]), '40-49')) {
      full_train$SubjectAge[i] = round(runif(1, 40, 49), digits = 0)
    }
  }
  if (!is.na(full_train$SubjectAge[i]))  {
    if (str_detect(as.character(full_train$SubjectAge[i]), '50-59')) {
      full_train$SubjectAge[i] = round(runif(1, 50, 59), digits = 0)
    }
  }
  if (!is.na(full_train$SubjectAge[i]))  {
    if (str_detect(as.character(full_train$SubjectAge[i]), '21-23')) {
      full_train$SubjectAge[i] = round(runif(1, 21, 23), digits = 0)
    }
  }
  if (!is.na(full_train$SubjectAge[i]))  {
    if (str_detect(as.character(full_train$SubjectAge[i]), 'Juvenile')) {
      full_train$SubjectAge[i] = round(runif(1, 14, 18), digits = 0)
    }
  }
  if (!is.na(full_train$SubjectAge[i]))  {
    if (str_detect(as.character(full_train$SubjectAge[i]), 'N/A')) {
      full_train$SubjectAge[i] = NA
    }
  }
  if (!is.na(full_train$SubjectAge[i]))  {
    if (str_detect(as.character(full_train$SubjectAge[i]), 'U')) {
      full_train$SubjectAge[i] = NA
    }
  }
}



#Now that we have cleaned the data, we need to impute the missing NA values. But one last thing, lets coerce all the character vectors to factors so MICE can do its thing. 

character_col <- c("manner_of_death", "armed", "gender", "race", 'state', 'city', "threat_level", "flee", "SubjectArmed", "SubjectRace", "SubjectGender", "Fatal")
full_train[,character_col] <- lapply(full_train[,character_col], as.factor)

#full_train <- full_train[,-c(1)] #drop the Ob column
full_train$SubjectAge <- as.integer(full_train$SubjectAge)

# ------------------------------------------------------------------------------------------
#THIS LINE OF CODE TAKES HOURS TO RUN! RUN WITH CAUTION!!!!

#imputed_vals <- mice(full_train[,-c(1,7)], m = 3, seed = 123, nnet.MaxNWts = 5000)

# ------------------------------------------------------------------------------------------


#Here we add version one of the imputations to the original data set. Ideally, we would run the models with the long data frame with all imputations, but I will try that later. First, let's do this. 

imputed_vals$imp$SubjectAge
Subset <- full_train
Subset <- complete(imputed_vals, 1) 
full_train <- cbind(full_train$city, full_train$state, Subset)
names(full_train)[1:2] <- c("city", 'state')



levels(full_train$armed)
unique(full_train$armed)
total <- c(firearm,sharp_weapon, melee_weapon, other, unarmed)
length(total)



#Now we will create new variables, and after that import some variables from a demographics dataset. 
#Next time do this before using the MICE package

# Create a "Armed Groups" Variable

firearm <- c("gun", "gun and car", "gun and knife", "gun and sword", "gun and vehicle", "gun and explosives", "vehicle and gun")
sharp_weapon <- c("ax", "box cutter", "knife", "machete", "pole and knife", "sharp object", "sword", "RDM")
melee_weapon <- c("baseball bat", "cordless drill", "metal object", "metal pipe", "metal stick", "pick-axe", "rock", "screwdriver", "shovel", "unknown weapon")
other <- c("air conditioner", "brick", "chair", "flagpole", "flashlight", "toy weapon", "undetermined", "wasp spray", "vehicle")
unarmed <- c("unarmed")

armed_groups <- rep(NA, length(full_train$armed))
for (i in seq_along(full_train$armed)) {
  if(is.na(full_train$armed[i] == TRUE)) {
    armed_groups[i] <- NA
    next
  }
  if(any(full_train$armed[i] == firearm)) {
    armed_groups[i] <- "Firearm"
  }
  if(any(full_train$armed[i] == sharp_weapon)) {
    armed_groups[i] <- "Sharp Weapon"
  }
  if(any(full_train$armed[i] == melee_weapon)) {
    armed_groups[i] <- "Melee Weapon"
  }
  if(any(full_train$armed[i] == other)) {
    armed_groups[i] <- "Other"
  }
  if(any(full_train$armed[i] == unarmed)) {
    armed_groups[i] <- "Unarmed"
  }
  else { armed_groups[i] <- "Other"}
}

full_train$armed_group <- armed_groups
full_train$armed_group <- as.factor(full_train$armed_group)


#Creating City Population, Region, and State Registered Weapon Variables

full_train["city_population"] <- NA

full_train[full_train$city == "Albuquerque", "city_population"] <- 560513
full_train[full_train$city == "Atlanta", "city_population"] <- 498044
full_train[full_train$city == "Austin", "city_population"] <- 964254
full_train[full_train$city == "Boston", "city_population"] <- 694583
full_train[full_train$city == "Chicago", "city_population"] <- 2706000
full_train[full_train$city == "Cincinnati", "city_population"] <- 302605
full_train[full_train$city == "Cleveland", "city_population"] <- 383793
full_train[full_train$city == "Columbus", "city_population"] <- 892533
full_train[full_train$city == "Dallas", "city_population"] <- 1345000
full_train[full_train$city == "Denver", "city_population"] <- 727211
full_train[full_train$city == "El Paso", "city_population"] <- 682669
full_train[full_train$city == "Fort Worth", "city_population"] <- 895008
full_train[full_train$city == "Honolulu", "city_population"] <- 337256
full_train[full_train$city == "Houston", "city_population"] <- 2326000
full_train[full_train$city == "Indianapolis", "city_population"] <- 876862
full_train[full_train$city == "Jacksonville", "city_population"] <- 903889
full_train[full_train$city == "Kansas City", "city_population"] <- 491918
full_train[full_train$city == "Louisville", "city_population"] <- 617638
full_train[full_train$city == "Memphis", "city_population"] <- 650618
full_train[full_train$city == "Milwaukee", "city_population"] <- 592025
full_train[full_train$city == "Nashville", "city_population"] <- 692587
full_train[full_train$city == "New York", "city_population"] <- 8336817
full_train[full_train$city == "Newark", "city_population"] <- 282090
full_train[full_train$city == "Philadelphia", "city_population"] <- 1584064
full_train[full_train$city == "Phoenix", "city_population"] <- 1680992
full_train[full_train$city == "Portland", "city_population"] <- 653115
full_train[full_train$city == "San Antonio", "city_population"] <- 2269000
full_train[full_train$city == "San Francisco", "city_population"] <- 883305
full_train[full_train$city == "Seattle", "city_population"] <- 744955
full_train[full_train$city == "St. Louis", "city_population"] <- 300576
full_train[full_train$city == "Tampa", "city_population"] <- 392890
full_train[full_train$city == "Tucson", "city_population"] <- 545975

full_train$geographic <- "region"
full_test$geographic <- "region"
for(i in 1:nrow(full_train)) {
  if(full_train$state[i] == "ME" | full_train$state[i] == "MA" | full_train$state[i] == "NY" | full_train$state[i] == "NJ" | full_train$state[i] == "PA" | full_train$state[i] == "MD") {
    full_train$geographic[i] <- "Northeast"
  } else if (full_train$state[i] == "KY" | full_train$state[i] == "FL" | full_train$state[i] == "AL" | full_train$state[i] == "TN" | full_train$state[i] == "MS" | full_train$state[i] == "GA" | full_train$state[i] == "NC" | full_train$state[i] == "AR") {
    full_train$geographic[i] <- "Southeast"
  } else if (full_train$state[i] == "OH" | full_train$state[i] == "MN" | full_train$state[i] == "MO" | full_train$state[i] == "NE" | full_train$state[i] == "IN" | full_train$state[i] == "IL" | full_train$state[i] == "WI" | full_train$state[i] == "KS") {
    full_train$geographic[i] <- "Midwest"
  } else if (full_train$state[i] == "AZ" | full_train$state[i] == "NM" | full_train$state[i] == "TX") {
    full_train$geographic[i] <- "Southwest"
  } else {
    full_train$geographic[i] <- "West"
  }
}
full_train$geographic <- as.factor(full_train$geographic)
levels(full_train$geographic)

full_train$registeredweapons <- 0
for(i in 1:nrow(full_train)) {
  if(full_train$state[i] == "ME") {
    full_train$registeredweapons[i] <- 17410
  } else if (full_train$state[i] == "MA") {
    full_train$registeredweapons[i] <- 39886
  } else if (full_train$state[i] == "NY") {
    full_train$registeredweapons[i] <- 82917
  } else if (full_train$state[i] == "NJ") {
    full_train$registeredweapons[i] <- 90217
  } else if (full_train$state[i] == "PA") {
    full_train$registeredweapons[i] <- 271427
  } else if (full_train$state[i] == "MD") {
    full_train$registeredweapons[i] <- 128289
  } else if (full_train$state[i] == "KY") {
    full_train$registeredweapons[i] <- 93719
  } else if (full_train$state[i] == "FL") {
    full_train$registeredweapons[i] <- 432581
  } else if (full_train$state[i] == "AL") {
    full_train$registeredweapons[i] <- 168265
  } else if (full_train$state[i] == "TN") {
    full_train$registeredweapons[i] <- 121140
  } else if (full_train$state[i] == "MS") {
    full_train$registeredweapons[i] <- 52346
  } else if (full_train$state[i] == "GA") {
    full_train$registeredweapons[i] <- 225993
  } else if (full_train$state[i] == "NC") {
    full_train$registeredweapons[i] <- 181209
  } else if (full_train$state[i] == "AR") {
    full_train$registeredweapons[i] <- 108801
  } else if (full_train$state[i] == "OH") {
    full_train$registeredweapons[i] <- 175819
  } else if (full_train$state[i] == "MN") {
    full_train$registeredweapons[i] <- 98585
  } else if (full_train$state[i] == "MO") {
    full_train$registeredweapons[i] <- 88270
  } else if (full_train$state[i] == "NE") {
    full_train$registeredweapons[i] <- 29753
  } else if (full_train$state[i] == "IN") {
    full_train$registeredweapons[i] <- 133594
  } else if (full_train$state[i] == "IL") {
    full_train$registeredweapons[i] <- 147698
  } else if (full_train$state[i] == "WI") {
    full_train$registeredweapons[i] <- 79639
  } else if (full_train$state[i] == "KS") {
    full_train$registeredweapons[i] <- 54409
  } else if (full_train$state[i] == "AZ") {
    full_train$registeredweapons[i] <- 204817
  } else if (full_train$state[i] == "NM") {
    full_train$registeredweapons[i] <- 105836
  } else if (full_train$state[i] == "TX") {
    full_train$registeredweapons[i] <- 725368
  } else if (full_train$state[i] == "WA") {
    full_train$registeredweapons[i] <- 119829
  } else if (full_train$state[i] == "OR") {
    full_train$registeredweapons[i] <- 74722
  } else if (full_train$state[i] == "CO") {
    full_train$registeredweapons[i] <- 112691
  } else if (full_train$state[i] == "CA") {
    full_train$registeredweapons[i] <- 376666
  } else if (full_train$state[i] == "AK") {
    full_train$registeredweapons[i] <- 20520
  } else if (full_train$state[i] == "HI") {
    full_train$registeredweapons[i] <- 8665
  }
}
full_train %>% 
  select(city, state)
levels(full_train$city)



full_train["city_gundealers"] <- NA

full_train[full_train$city == "Albuquerque", "city_gundealers"] <- 165
full_train[full_train$city == "Atlanta", "city_gundealers"] <- 294
full_train[full_train$city == "Austin", "city_gundealers"] <- 239
full_train[full_train$city == "Boston", "city_gundealers"] <- 145
full_train[full_train$city == "Chicago", "city_gundealers"] <- 101
full_train[full_train$city == "Cincinnati", "city_gundealers"] <- 307
full_train[full_train$city == "Cleveland", "city_gundealers"] <- 241
full_train[full_train$city == "Columbus", "city_gundealers"] <- 206
full_train[full_train$city == "Dallas", "city_gundealers"] <- 646
full_train[full_train$city == "Denver", "city_gundealers"] <- 501
full_train[full_train$city == "El Paso", "city_gundealers"] <- 92
full_train[full_train$city == "Fort Worth", "city_gundealers"] <- 596
full_train[full_train$city == "Honolulu", "city_gundealers"] <- 32
full_train[full_train$city == "Houston", "city_gundealers"] <- 686
full_train[full_train$city == "Indianapolis", "city_gundealers"] <- 188
full_train[full_train$city == "Jacksonville", "city_gundealers"] <- 170
full_train[full_train$city == "Kansas City", "city_gundealers"] <- 341
full_train[full_train$city == "Louisville", "city_gundealers"] <- 206
full_train[full_train$city == "Memphis", "city_gundealers"] <- 121
full_train[full_train$city == "Milwaukee", "city_gundealers"] <- 162
full_train[full_train$city == "Nashville", "city_gundealers"] <- 175
full_train[full_train$city == "New York", "city_gundealers"] <- 159
full_train[full_train$city == "Newark", "city_gundealers"] <- 158
full_train[full_train$city == "Philadelphia", "city_gundealers"] <- 230
full_train[full_train$city == "Phoenix", "city_gundealers"] <- 640
full_train[full_train$city == "Portland", "city_gundealers"] <- 330
full_train[full_train$city == "San Antonio", "city_gundealers"] <- 241
full_train[full_train$city == "San Francisco", "city_gundealers"] <- 63
full_train[full_train$city == "Seattle", "city_gundealers"] <- 199
full_train[full_train$city == "St. Louis", "city_gundealers"] <- 362
full_train[full_train$city == "Tampa", "city_gundealers"] <- 357
full_train[full_train$city == "Tucson", "city_gundealers"] <- 184


#Read in the demographics Data

demographic <- read_csv("Demographic Statistics per city per state averages (1).csv", col_names = TRUE)
demographic <- demographic %>%
  filter(City == "Albuquerque city" | City == "Atlanta city" | City == "Austin city" | City == "Boston city" | City == "Chicago city" | City == "Cincinnati city" | City == "Cleveland city" | City == "Columbus city" | City == "Dallas city" | City == "Denver city" | City == "El Paso city" | City == "Fort Worth city" | City == "Honolulu city" | City == "Houston city" | City == "Indianapolis city" | City == "Jacksonville city" | City == "Kansas City city" | City == "Louisville city" | City == "Memphis city" | City == "Milwaukee city" | City == "Nashville city" | City == "New York City" | City == "Newark city" | City == "Philadelphia city" | City == "Phoenix city" | City == "Portland city" | City == "San Antonio city" | City == "San Francisco city" | City == "Seattle city" | City == "St. Louis city" | City == "Tampa city" | City == "Tucson city" | Ob == 10014 | Ob == 24205 | Ob == 5642 | Ob == 8659)

demographic_cities <- demographic[c(8, 9, 11, 12, 15, 17, 18, 26, 30, 36, 40, 45, 46, 60, 72:77, 84:85, 88, 89, 93, 96:99, 104, 105, 107), ]

demographic_cities$City[8] <- "Honolulu city"
demographic_cities$City[10] <- "Indianapolis city"
demographic_cities$City[12] <- "Louisville city"
demographic_cities$City[24] <- "Nashville city"

demographic_cities[, 4:11] <- apply(demographic_cities[, 4:11], 2, as.numeric)

full_train["share_white"] <- NA
full_train["share_black"] <- NA
full_train["share_native_american"] <- NA
full_train["share_asian"] <- NA
full_train["share_hispanic"] <- NA
full_train["median_income"] <- NA
full_train["poverty_rate"] <- NA
full_train["percent_completed_hs"] <- NA

full_train[full_train$city == "Albuquerque", 27:34] <- demographic_cities[demographic_cities$City == "Albuquerque city", 4:11]
full_train[full_train$city == "Atlanta", 27:34] <- demographic_cities[demographic_cities$City == "Atlanta city", 4:11]
full_train[full_train$city == "Austin", 27:34] <- demographic_cities[demographic_cities$City == "Austin city", 4:11]
full_train[full_train$city == "Boston", 27:34] <- demographic_cities[demographic_cities$City == "Boston city", 4:11]
full_train[full_train$city == "Chicago", 27:34] <- demographic_cities[demographic_cities$City == "Chicago city", 4:11]
full_train[full_train$city == "Cincinnati", 27:34] <- demographic_cities[demographic_cities$City == "Cincinnati city", 4:11]
full_train[full_train$city == "Cleveland", 27:34] <- demographic_cities[demographic_cities$City == "Cleveland city", 4:11]
full_train[full_train$city == "Columbus", 27:34] <- demographic_cities[demographic_cities$City == "Columbus city", 4:11]
full_train[full_train$city == "Dallas", 27:34] <- demographic_cities[demographic_cities$City == "Dallas city", 4:11]
full_train[full_train$city == "Denver", 27:34] <- demographic_cities[demographic_cities$City == "Denver city", 4:11]
full_train[full_train$city == "El Paso", 27:34] <- demographic_cities[demographic_cities$City == "El Paso city", 4:11]
full_train[full_train$city == "Fort Worth", 27:34] <- demographic_cities[demographic_cities$City == "Fort Worth city", 4:11]
full_train[full_train$city == "Honolulu", 27:34] <- demographic_cities[demographic_cities$City == "Honolulu city", 4:11]
full_train[full_train$city == "Houston", 27:34] <- demographic_cities[demographic_cities$City == "Houston city", 4:11]
full_train[full_train$city == "Indianapolis", 27:34] <- demographic_cities[demographic_cities$City == "Indianapolis city", 4:11]
full_train[full_train$city == "Jacksonville", 27:34] <- demographic_cities[demographic_cities$City == "Jacksonville city", 4:11]
full_train[full_train$city == "Kansas City", 27:34] <- demographic_cities[demographic_cities$City == "Kansas City city", 4:11]
full_train[full_train$city == "Louisville", 27:34] <- demographic_cities[demographic_cities$City == "Louisville city", 4:11]
full_train[full_train$city == "Memphis", 27:34] <- demographic_cities[demographic_cities$City == "Memphis city", 4:11]
full_train[full_train$city == "Milwaukee", 27:34] <- demographic_cities[demographic_cities$City == "Milwaukee city", 4:11]
full_train[full_train$city == "Nashville", 27:34] <- demographic_cities[demographic_cities$City == "Nashville city", 4:11]
full_train[full_train$city == "New York", 27:34] <- demographic_cities[demographic_cities$City == "New York City", 4:11]
full_train[full_train$city == "Newark", 27:34] <- demographic_cities[demographic_cities$City == "Newark city", 4:11]
full_train[full_train$city == "Philadelphia", 27:34] <- demographic_cities[demographic_cities$City == "Philadelphia city", 4:11]
full_train[full_train$city == "Phoenix", 27:34] <- demographic_cities[demographic_cities$City == "Phoenix city", 4:11]
full_train[full_train$city == "Portland", 27:34] <- demographic_cities[demographic_cities$City == "Portland city", 4:11]
full_train[full_train$city == "San Antonio", 27:34] <- demographic_cities[demographic_cities$City == "San Antonio city", 4:11]
full_train[full_train$city == "San Francisco", 27:34] <- demographic_cities[demographic_cities$City == "San Francisco city", 4:11]
full_train[full_train$city == "Seattle", 27:34] <- demographic_cities[demographic_cities$City == "Seattle city", 4:11]
full_train[full_train$city == "St. Louis", 27:34] <- demographic_cities[demographic_cities$City == "St. Louis city", 4:11]
full_train[full_train$city == "Tampa", 27:34] <- demographic_cities[demographic_cities$City == "Tampa city", 4:11]
full_train[full_train$city == "Tucson", 27:34] <- demographic_cities[demographic_cities$City == "Tucson city", 4:11]

lapply(full_train, class)


#Something within the cleaning process of Officer Race went astray, so let's clean that up  

for (i in seq_along(full_train$OfficerRace)){
  if (full_train$OfficerRace[i] == 'm'){
    full_train$OfficerRace[i] <- 'Mixed'
  }
  if (full_train$OfficerRace[i] == 'H'){
    full_train$OfficerRace[i] <- 'L'
  }
  if (full_train$OfficerRace[i] == 'WHITE'){
    full_train$OfficerRace[i] <- 'W'
  }
}
levels(full_train$OfficerRace)[levels(full_train$OfficerRace)=="O"] <- "Other"
full_train$OfficerRace <- droplevels(full_train$OfficerRace)

safety <- full_train


levels(full_train$OfficerRace)
apply(is.na(full_train), 2, any)
sum(is.na(full_train$armed_group))

#Finally ====================================================================================

## Writing a New CSV
write_csv(full_train, "PoliceTrainNewCleaned.csv")

