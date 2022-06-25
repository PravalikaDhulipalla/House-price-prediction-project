########### HOUSE PREDICTION ###########

# loading packages 

library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)

house_data <- read.csv("C:/Users/sasee/Downloads\\house prediction.csv", header=T)
house_data

##################Data Exploration#############################################

#structure 
str(house_data)
#dimensions
dim(house_data)
# checking for missing values 
colSums(is.na(house_data))

#################Data visualizations###########################################

priceConverted <- house_data$price/100000

#Visual 1: Distribution of price of the houses 
hist(priceConverted,
     data= house_data,
     main = 'Distribution of House Prices',
     ylab= 'Frequency',
     xlab= 'Price in $100,000',
     col= '#ffc40c',
     xlim = c(0,50),
     breaks = 90,
     labels = TRUE)


# Visual 2: Distribution of house price against square ft.
sq_ft <- house_data$sqft_lot/100
plot(priceConverted, x=sq_ft, xlab = 'Square Feet', ylab = 'Price in $100,000')

# splitting testing and training data

RNGkind(sample.kind = "Rounding")
set.seed(417)

idx <- sample(nrow(house_data), nrow(house_data)* 0.8)

house_data_train <- house_data[idx,]    #80% train data 

house_data_test <- house_data[ -idx,]   #20% test data 

##################### Mapping House price with its longitude and latitude####### 
library(maps)
wa_m <- map_data("state") %>% filter(region =="Washington") #getting washinton map

counties <- map_data("county")
wa_c <- subset(counties, region == "Washington")

wa_base <- ggplot(data = wa_m, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

wa_map <- wa_base +
  geom_polygon(data = wa_c, fill = NA, color = "white") +
  geom_polygon(color = "#cc0000", fill = NA)

wa_map +
  #ggplot (housing, mapping = aes(y = latitude, x = longitude)) +
  geom_jitter(data = house_data,aes(x = longitude, 
                                 y = latitude,size = population, 
                                 col = price,
                                 group = statezip))+
  labs(title = "Population Distribution", 
       subtitle ="Median Housing Price",
       col = "Median Price",
       size = "Population",
       x = "Longitude",
       y = "Latitude"
  )


##############################Fitting Model ##################################


# Removing unecessary colums for modelling 
new_data <- subset(house_data, select = c(-1,-country ) )
new_data


newdata.model <- lm(price ~ ., data = new_data)
summary(newdata.model)

#Predecting into test data 
housing_test$pred <- predict(newdata.model, housing_test)

hist(housing_test$pred)

#####Accuracy testing 
library(MLmetrics)
RMSE(y_pred = housing_test$pred, y_true = housing_test$price)


