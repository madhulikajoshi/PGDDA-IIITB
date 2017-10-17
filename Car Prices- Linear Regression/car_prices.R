##load libraries#####################
library(stringr)
library(stringi)
library(MASS)
library(car)
#load the Automobile companies data
getwd()
##set working directory
#setwd("C:/Users/Madhulika/Desktop/Upgrad/Linear Regression/Assignment")
cars <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = T)
str(cars)
View(cars)

#Business requirement is to find out variables which may be useful to predict price######

##########Data Understanding,Preparation and EDA ################################################
###########################################################################################

nrow(cars) #205 observations
ncol(cars) #26 columns

##No missing values or NA found in any columns, all the values are present
colSums(is.na(cars))

#Converting to lower case
cars$CarName <- tolower(cars$CarName)

##Cheking structure of the dataframe
str(cars)

##Checking summary of the dataframe
summary(cars)

##extracting company name as first value before space in carName column
cars$company_name <-
  stri_extract_first_regex(cars$CarName, "^[^ ]+")

cars$company_name

##Removing CarName column
cars <- cars[,-3]

##Data Correction needs to be done for company name, as there are few spelling mistakes for

#the list of names is -

#alfa-romero, audi, bmw, buick, chevrolet, dodge, honda,
#isuzu, jaguar, mazda, mercury,
#mitsubishi, nissan, peugot, plymouth, porsche,
#renault, saab, subaru, toyota, volkswagen, volvo

table(cars$company_name) ##showing 27, should be 22 distinct names, 5 needs to be corrected

#Replacing maxda with mazda
cars$company_name <- gsub('maxda', 'mazda', cars$company_name)

#Replacing porcshce with porsche
cars$company_name <- gsub('porcshce', 'porsche', cars$company_name)

#Replacing toyouta with toyota
cars$company_name <- gsub('toyouta', 'toyota', cars$company_name)

#Replacing vokswagen, vw with volkswagen
cars$company_name <-
  gsub('vokswagen', 'volkswagen', cars$company_name)
cars$company_name <- gsub('vw', 'volkswagen', cars$company_name)

#symboling: -2, -1, 0, 1, 2, 3
table(cars$symboling)

#dividing the symboling data into three levels and creating a new variable risk_rating
## -2,-1 = safe
## 0, 1 = neutral
## 2,3 = risky
cars$risk_rating <- cut (
  cars$symboling ,
  breaks = c(-3, -1, 1, 3),
  labels = c('safe', 'neutral', 'risky')
)

# fuel-type: diesel, gas.
#For fuel type,there are only 20 cars using diesel and others uses gas.
table(cars$fueltype)

# aspiration: std, turbo.
#168 cars have standard aspiration and 37 have turbo
table(cars$aspiration)

# num-of-doors: four, two.
# 115 cars have four doors and 90 cars have 2 doors
table(cars$doornumber)

# body-style: hardtop, wagon, sedan, hatchback, convertible.
table(cars$carbody)

# drive-wheels: 4wd, fwd, rwd.
table(cars$drivewheel)

# engine-location: front, rear.
## Most of the cars have engines located in front, only 3 have at rear
table(cars$enginelocation)

# wheel-base: continuous from 86.6 120.9.
summary(cars$wheelbase)
#Box plot shows that values are positively skewed and there are two outliers
boxplot(cars$wheelbase)
IQR(cars$wheelbase)

# length: continuous from 141.1 to 208.1.
summary(cars$carlength)
boxplot(cars$carlength)

# width: continuous from 60.3 to 72.3.
summary(cars$carwidth)
##boxplot shows 4 outliers
boxplot(cars$carwidth)
boxplot.stats(cars$carwidth)
quantile(cars$carwidth, 0.99)

# height: continuous from 47.8 to 59.8.
summary(cars$carheight)
boxplot(cars$carheight)

# curb-weight: continuous from 1488 to 4066.
summary(cars$curbweight)
boxplot(cars$curbweight)

# engine-type: dohc, dohcv, l, ohc, ohcf, ohcv, rotor.
table(cars$enginetype)

# num-of-cylinders: eight, five, four, six, three, twelve, two.
table(cars$cylindernumber)

# engine-size: continuous from 61 to 326.
boxplot(cars$enginesize)
#Shows some outliers
boxplot.stats(cars$enginesize)$out

# fuel-system: 1bbl, 2bbl, 4bbl, idi, mfi, mpfi, spdi, spfi.
table(cars$fuelsystem)

# bore: continuous from 2.54 to 3.94.
summary(cars$boreratio)
boxplot(cars$boreratio)

# stroke: continuous from 2.07 to 4.17.
summary(cars$stroke)
boxplot(cars$stroke)
#Some outliers are present
boxplot.stats(cars$stroke)$out

# compression-ratio: continuous from 7 to 23.
summary(cars$compressionratio)
boxplot(cars$compressionratio)
#Outliers are present
boxplot.stats(cars$compressionratio)$out

# horsepower: continuous from 48 to 288.
summary(cars$horsepower)
boxplot(cars$horsepower)
#Outliers
boxplot.stats(cars$horsepower)$out

# peak-rpm: continuous from 4150 to 6600.
summary(cars$peakrpm)
boxplot(cars$peakrpm)

# city-mpg: continuous from 13 to 49.
summary(cars$citympg)
boxplot(cars$citympg)

# highway-mpg: continuous from 16 to 54.
summary(cars$highwaympg)
boxplot(cars$highwaympg)
#outliers
boxplot.stats(cars$highwaympg)$out

# price: continuous from 5118 to 45400.
summary(cars$price)
boxplot(cars$price)
##The plot shows price is positively skewed and 15 outliers are
#present for high price segment
boxplot.stats(cars$price)$out

##Althugh there are many variables having outliers, we are not removing or fixing them up 
##as these variables are very closely related to the properties of the car whose price we want 
##to predict. So, we can't change these values. Removing the values will result in data loss.
## so keeping all the data intact, preparing the models.

#########Creating Dummy Variables for all categorical variables######################
#####################################################################################

##dummy for company name##
dummy_1 <- data.frame(model.matrix( ~ company_name, data = cars))
View(dummy_1)
dummy_1 <- dummy_1[,-1]

##Two numeric levels for fuel type
levels(cars$fueltype) <- c(1, 0)
cars$fueltype <- as.numeric(levels(cars$fueltype))[cars$fueltype]

##Two levels for aspiration
levels(cars$aspiration) <- c(1, 0)
cars$aspiration <-
  as.numeric(levels(cars$aspiration))[cars$aspiration]

##Two levels for door number
levels(cars$doornumber) <- c(1, 0)
cars$doornumber <-
  as.numeric(levels(cars$doornumber))[cars$doornumber]

##dummy variable for car body type
dummy_2 <- data.frame(model.matrix( ~ carbody, data = cars))
View(dummy_2)
dummy_2 <- dummy_2[,-1]

#dummy variable for car drive wheel
dummy_3 <- data.frame(model.matrix( ~ drivewheel, data = cars))
View(dummy_3)
dummy_3 <- dummy_3[,-1]

##Two levels for engine location
levels(cars$enginelocation) <- c(1, 0)
cars$enginelocation <-
  as.numeric(levels(cars$enginelocation))[cars$enginelocation]

##Dummy variable for engine type
dummy_4 <- data.frame(model.matrix( ~ enginetype, data = cars))
View(dummy_4)
dummy_4 <- dummy_4[,-1]

#Dummy vaiable for number of cylinders
dummy_5 <- data.frame(model.matrix( ~ cylindernumber, data = cars))
View(dummy_5)
dummy_5 <- dummy_5[,-1]

#Dummy variable for fuel system
dummy_6 <- data.frame(model.matrix( ~ fuelsystem, data = cars))
View(dummy_6)
dummy_6 <- dummy_6[,-1]

#Dummy variable for risk_rating
dummy_7 <- data.frame(model.matrix( ~ risk_rating, data = cars))
View(dummy_7)
dummy_7 <- dummy_7[,-1]

#Remove all the categorical variables and combine dummy variables created in a new dataset
cars_final <-
  cars[, setdiff(
    names(cars),
    c(
      "company_name",
      "carbody",
      "drivewheel",
      "enginetype",
      "cylindernumber",
      "fuelsystem",
      "risk_rating",
      "symboling"
    )
  )]

cars_final <-
  cbind(cars_final,
        dummy_1,
        dummy_2,
        dummy_3,
        dummy_4,
        dummy_5,
        dummy_6,
        dummy_7)

#Removing carid as it is just an id not any parameter
cars_final <- cars_final[,-1]

str(cars_final)
ncol(cars_final) #66 columns

####Checking correlation among all variables except the dependent variable price
corr_matrix <-
  cor(cars_final[, setdiff(names(cars_final), c("price"))])

write.csv(corr_matrix, file = "corr_matrix_all.csv")

##From the matrix, we can get few insights
## One Highly correlated group of 6 variables are wheel base, length, width, curb weight,
##enginesize and boreratio
##wheel base, length, width are different
##measures of the size of vehicles, so it makes sense they are highly correlated.

## citympg and highwaympg are highly correlated

## citympg and highwaympg are strongly negatively correlated with wheel base, length, width,
##curb weight,enginesize and boreratio

##horsepower is highly correlated with carwidth, enginesize, curbweight

##Price is higher for higher values of carwidth, curbweight and enginesize

# separate training and testing data
set.seed(100)
trainindices = sample(1:nrow(cars_final), 0.7 * nrow(cars_final))
train = cars_final[trainindices,]
test = cars_final[-trainindices,]

# Build model 1 containing all variables
model_1 <- lm(price ~ ., data = train)
summary(model_1)

# We have a total of 66 variables considered into the model
#Now let;s run the code.

step <- stepAIC(model_1, direction = "both")
step

##creating model 2 after variable reduction

model_2 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      curbweight + enginesize + stroke + peakrpm + citympg + company_namebmw +
      company_namebuick + company_namedodge + company_namehonda +
      company_nameisuzu + company_namejaguar + company_namemazda +
      company_namemercury + company_namemitsubishi + company_namenissan +
      company_namepeugeot + company_nameplymouth + company_nameporsche +
      company_namerenault + company_namesaab + company_namesubaru +
      company_nametoyota + company_namevolkswagen + company_namevolvo +
      carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon +
      drivewheelrwd + enginetyperotor + cylindernumberfive + fuelsystem2bbl +
      fuelsystemmpfi + risk_ratingrisky,
    data = train
  )

summary(model_2) #Multiple R-squared:  0.9806,	Adjusted R-squared:  0.9738

#Removing company_nameporsche as the p value is high and creating model 3
model_3 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      curbweight + enginesize + boreratio + stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan + company_namepeugeot +
      company_nameplymouth + company_namerenault +
      company_namesaab + company_namesubaru + company_nametoyota +
      company_namevolkswagen + carbodyhardtop + carbodyhatchback +
      carbodysedan + carbodywagon + drivewheelrwd + enginetyperotor +
      cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + highwaympg,
    data = train
  )

summary(model_3) #Multiple R-squared:  0.9785,	Adjusted R-squared:  0.9719


# Removing fuelsystemmpfi as p value is high and creating model 4
model_4 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      curbweight + enginesize + boreratio + stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan + company_namepeugeot +
      company_nameplymouth + company_namerenault +
      company_namesaab + company_namesubaru + company_nametoyota +
      company_namevolkswagen + carbodyhardtop + carbodyhatchback +
      carbodysedan + carbodywagon + drivewheelrwd + enginetyperotor +
      cylindernumberfive + fuelsystem2bbl + highwaympg,
    data = train
  )

summary(model_4) #Multiple R-squared:  0.9781,	Adjusted R-squared:  0.9717

#Removing boreratio

model_5 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      curbweight + enginesize + stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan + company_namepeugeot +
      company_nameplymouth + company_namerenault +
      company_namesaab + company_namesubaru + company_nametoyota +
      company_namevolkswagen + carbodyhardtop + carbodyhatchback +
      carbodysedan + carbodywagon + drivewheelrwd + enginetyperotor +
      cylindernumberfive + fuelsystem2bbl + highwaympg,
    data = train
  )

summary(model_5) #Multiple R-squared:  0.9775,	Adjusted R-squared:  0.9712

#Removing cylindernumberfive
model_6 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      curbweight + enginesize + stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan + company_namepeugeot +
      company_nameplymouth + company_namerenault +
      company_namesaab + company_namesubaru + company_nametoyota +
      company_namevolkswagen + carbodyhardtop + carbodyhatchback +
      carbodysedan + carbodywagon + drivewheelrwd + enginetyperotor
    + fuelsystem2bbl + highwaympg,
    data = train
  )

summary(model_6) #Multiple R-squared:  0.9769,	Adjusted R-squared:  0.9707

#Removing company_namesaab
model_7 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      curbweight + enginesize + stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan + company_namepeugeot +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen + carbodyhardtop +
      carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + enginetyperotor
    + fuelsystem2bbl + highwaympg,
    data = train
  )

summary(model_7) #Multiple R-squared:  0.9762,	Adjusted R-squared:  0.9701

#No insignificant variable found, checking vif
vif(model_7)

#Remove curbweight as it has very high VIF and comparatively insignificant
model_8 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      enginesize + stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan + company_namepeugeot +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen + carbodyhardtop +
      carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + enginetyperotor
    + fuelsystem2bbl + highwaympg,
    data = train
  )

summary(model_8) #Multiple R-squared:  0.9752,	Adjusted R-squared:  0.9691

#Removing company_namepeugeot with higher p value
model_9 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      enginesize + stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen + carbodyhardtop +
      carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + enginetyperotor
    + fuelsystem2bbl + highwaympg,
    data = train
  )

summary(model_9) #Multiple R-squared:  0.9745,	Adjusted R-squared:  0.9685

#Checking for collinearity
vif(model_9)

#Removing highwaympg as it has high VIF and low significance
model_10 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      enginesize + stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen + carbodyhardtop +
      carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + enginetyperotor
    + fuelsystem2bbl,
    data = train
  )

summary(model_10) #Multiple R-squared:  0.9736,	Adjusted R-squared:  0.9676


#Removing fuelsystem2bbl as it has higher p value (>0.05)

model_11 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      enginesize + stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen + carbodyhardtop +
      carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + enginetyperotor
    ,
    data = train
  )

summary(model_11) #Multiple R-squared:  0.9729,	Adjusted R-squared:  0.9671

vif(model_11)

#Removing carbodywagon as it has high VIF and lower significance as compared to others
model_12 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      enginesize + stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen + carbodyhardtop +
      carbodyhatchback + carbodysedan + drivewheelrwd + enginetyperotor
    ,
    data = train
  )

summary(model_12) #Multiple R-squared:  0.9713,	Adjusted R-squared:  0.9655

#Removing carbodysedan as it has low significance
model_13 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      enginesize + stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen + carbodyhardtop +
      carbodyhatchback + drivewheelrwd + enginetyperotor
    ,
    data = train
  )

summary(model_13) #Multiple R-squared:  0.9711,	Adjusted R-squared:  0.9655

#Removing carbodyhatchback as it has low significance
model_14 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      enginesize + stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen + carbodyhardtop +
      drivewheelrwd + enginetyperotor
    ,
    data = train
  )

summary(model_14) #Multiple R-squared:  0.9709,	Adjusted R-squared:  0.9655

#Removing carbodyhardtop as it is not significant
model_15 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      enginesize + stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen +
      drivewheelrwd + enginetyperotor
    ,
    data = train
  )

summary(model_15) #Multiple R-squared:  0.9706,	Adjusted R-squared:  0.9655

vif(model_15)

#Removing engine size as it has highest VIF and is highly correlated with carwidth
model_16 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen +
      drivewheelrwd + enginetyperotor
    ,
    data = train
  )

summary(model_16) #Multiple R-squared:  0.919,	Adjusted R-squared:  0.9057

#Now many variables have become insignificant, removing enginetyperotor
model_17 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      stroke + peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen +
      drivewheelrwd
    ,
    data = train
  )
summary(model_17) #Multiple R-squared:  0.9188,	Adjusted R-squared:  0.9063

#Removing stroke as it has lowest significance
model_18 <-
  lm(
    formula = price ~ aspiration + enginelocation + carwidth +
      peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen +
      drivewheelrwd
    ,
    data = train
  )
summary(model_18) #Multiple R-squared:  0.9187,	Adjusted R-squared:  0.9069

#Removing aspiration
model_19 <-
  lm(
    formula = price ~ enginelocation + carwidth +
      peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen +
      drivewheelrwd
    ,
    data = train
  )
summary(model_19) #Multiple R-squared:  0.9184,	Adjusted R-squared:  0.9073

#Removing drivewheelrwd
model_20 <-
  lm(
    formula = price ~ enginelocation + carwidth +
      peakrpm +
      company_namebmw + company_namebuick + company_namedodge +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen,
    data = train
  )
summary(model_20) #Multiple R-squared:  0.917,	Adjusted R-squared:  0.9065

#Removing company_namedodge
model_21 <-
  lm(
    formula = price ~ enginelocation + carwidth +
      peakrpm +
      company_namebmw + company_namebuick +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi + company_namenissan +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen,
    data = train
  )
summary(model_21) #Multiple R-squared:  0.9146,	Adjusted R-squared:  0.9045

#Removing company_namenissan
model_22 <-
  lm(
    formula = price ~ enginelocation + carwidth +
      peakrpm +
      company_namebmw + company_namebuick +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_nametoyota + company_namevolkswagen,
    data = train
  )
summary(model_22) #Multiple R-squared:  0.9128,	Adjusted R-squared:  0.9032

#Removing company_nametoyota
model_23 <-
  lm(
    formula = price ~ enginelocation + carwidth +
      peakrpm +
      company_namebmw + company_namebuick +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi +
      company_nameplymouth + company_namerenault + company_namesubaru +
      company_namevolkswagen,
    data = train
  )
summary(model_23) #Multiple R-squared:  0.9114,	Adjusted R-squared:  0.9025

#Removing company_namesubaru
model_24 <-
  lm(
    formula = price ~ enginelocation + carwidth +
      peakrpm +
      company_namebmw + company_namebuick +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi +
      company_nameplymouth + company_namerenault +
      company_namevolkswagen,
    data = train
  )
summary(model_24) #Multiple R-squared:   0.91,	Adjusted R-squared:  0.9017

#Removing company_nameplymouth
model_25 <-
  lm(
    formula = price ~ enginelocation + carwidth +
      peakrpm +
      company_namebmw + company_namebuick +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi +
      company_namerenault +
      company_namevolkswagen,
    data = train
  )

summary(model_25) #Multiple R-squared:  0.9087,	Adjusted R-squared:  0.901

#Removing company_namevolkswagen
model_26 <-
  lm(
    formula = price ~ enginelocation + carwidth +
      peakrpm +
      company_namebmw + company_namebuick +
      company_namehonda + company_namejaguar + company_namemazda +
      company_namemitsubishi +
      company_namerenault,
    data = train
  )

summary(model_26) #Multiple R-squared:  0.9062,	Adjusted R-squared:  0.899

#Removing company_namehonda

model_27 <-
  lm(
    formula = price ~ enginelocation + carwidth +
      peakrpm +
      company_namebmw + company_namebuick +
      company_namejaguar + company_namemazda +
      company_namemitsubishi +
      company_namerenault,
    data = train
  )

summary(model_27) #Multiple R-squared:  0.9036,	Adjusted R-squared:  0.8971

vif(model_27)

##Removing company_namemitsubishi as p value is very close to 0.05
model_28 <-
  lm(
    formula = price ~ enginelocation + carwidth +
      peakrpm +
      company_namebmw + company_namebuick +
      company_namejaguar + company_namemazda +
      company_namerenault,
    data = train
  )

summary(model_28) #Multiple R-squared:  0.9008,	Adjusted R-squared:  0.8948

#Removing company_namerenault as p value is close to 0.05
model_29 <-
  lm(
    formula = price ~ enginelocation + carwidth +
      peakrpm +
      company_namebmw + company_namebuick +
      company_namejaguar + company_namemazda,
    data = train
  )

summary(model_29) #Multiple R-squared:  0.8979,	Adjusted R-squared:  0.8926

#Removing company_namemazda as p value is close to 0.05
model_30 <-
  lm(
    formula = price ~ enginelocation + carwidth +
      peakrpm +
      company_namebmw + company_namebuick +
      company_namejaguar,
    data = train
  )

summary(model_30) #Multiple R-squared:  0.8949,	Adjusted R-squared:  0.8903

#Checking multicollinearity
vif(model_30)

##All Vif values are under 2 and all p values are also very low

##Running model on test data
View(test)
# predicting the results in test dataset
Predict_1 <-
  predict(model_30, test[, setdiff(names(test), c("price"))])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales.
r <- cor(test$price, test$test_price)
rsquared <- cor(test$price, test$test_price) ^ 2
rsquared #0.8206845

##Find the error and plot
test$error <- (test$price - test$test_price)

##No pattern means it is white noise and the model is good
plot(test$error)

#Can we remove any further variables ? Removing peakrpm and checking the Adjusted R-Squared
model_31 <-
  lm(
    formula = price ~ enginelocation + carwidth +
      company_namebmw + company_namebuick +
      company_namejaguar,
    data = train
  )

summary(model_31) #Multiple R-squared:  0.8894,	Adjusted R-squared:  0.8853

##Since R-squared didn't reduce by much and Multiple R-suared and Adjusted R-squared
##are quite close, we can say that model 31 and model 30 both are equally robust

##Running model on test data on model 31
View(test)
# predicting the results in test dataset
Predict_2 <-
  predict(model_31, test[, setdiff(names(test), c("price"))])
test$test_price_2 <- Predict_2

# Now, we need to test the r square between actual and predicted sales.
r <- cor(test$price, test$test_price_2)
rsquared <- cor(test$price, test$test_price_2) ^ 2
rsquared #0.8200359

test$error_2 <- (test$price - test$test_price_2)

##No pattern means it is white noise and the model is good enough to give accuracy
plot(test$error_2)

###So the predictors are enginelocation,carwidth,company_namebmw,
###company_namebuick,company_namejaguar are able to explain 82% variance
### on the test data.