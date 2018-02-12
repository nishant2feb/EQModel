library(caret)
library(ggplot2)
library(dplyr)
library(lubridate)



#importing data

DataEq = read.csv("oneyear.csv", stringsAsFactors = FALSE)

summary(DataEq)

#removing NA columns

DataEq = DataEq[ , -7]

class(DataEq$time)

#converting time to usable format

DataEq$time = ymd_hms(as_datetime(DataEq$time))
DataEq$updated = ymd_hms(as_datetime(DataEq$updated))
DataEq$year = year(DataEq$time)
DataEq$month = month(DataEq$time)
DataEq$day = day(DataEq$time)
#pattern in width and magnitude of the earthquake

ggplot(DataEq, aes(x = mag, y = depth, col = magType))+
  geom_point(alpha = 0.6)+
  facet_wrap(~ month)+
  theme_light()

# Removing no variance data

eqClean = DataEq[ , c(-10, -11, -14, -19, -20,-21)]

#scatterplot of lan and lat

ggplot(eqClean, aes(x = latitude, y = longitude, col = factor(mag))) +
  geom_point(alpha = 0.4)+
  facet_wrap(~ month)+
  theme_classic()

summary(eqClean$latitude)

eqClean$latCre = eqClean$latitude + 11.7622
summary(eqClean$latCre)

ggplot(eqClean, aes(x = latCre, y = longitude, col = factor(mag))) +
  geom_point(alpha = 0.4)+
  facet_wrap(~ month)+
  theme_classic()

eqClean$longCre = sqrt(eqClean$longitude)

ggplot(eqClean, aes(x = latCre, y = longCre, col = factor(mag))) +
  geom_point(alpha = 0.4)+
  facet_wrap(~ month)+
  theme_classic()

eqClean$Constant = eqClean$latCre * eqClean$longitude
summary(eqClean$Constant)

ggplot(eqClean, aes(x = Constant, y = longitude, col = factor(mag))) +
  geom_point(alpha = 0.4)+
  facet_wrap(~ month)+
  theme_classic()

#data for svr
dataset_all = eqClean[ , c(2, 3, 4, 5, 17)]
dataset = dataset_all[ , c(1, 2)]

#fitting the fault Line

library(e1071)
regressor = svm(formula = latitude ~ .,
                data = dataset,
                type = 'eps-regression',
                kernel = 'radial')

summary(regressor)
names(regressor)

y_pred = predict(regressor, newdata = dataset)

# plotting real and predicted

ggplot() +
  geom_point(aes(x = dataset$longitude, y = dataset$latitude),
             colour = 'red') +
  geom_line(aes(x = dataset$longitude, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('fitting fault line') +
  xlab('longitude') +
  ylab('lattitude')

# dividing the data to have a better view of the fault line

lon_indx = which(dataset$longitude <= 120)
dataset_cut = dataset[lon_indx, ]


#Plotting the cut dataset

ggplot() +
  geom_point(aes(x = dataset_cut$longitude, y = dataset_cut$latitude),
             colour = 'red') +
  geom_line(aes(x = dataset_cut$longitude, y = predict(regressor, newdata = dataset_cut)),
            colour = 'blue') +
  ggtitle('fitting fault line') +
  xlab('longitude') +
  ylab('lattitude')

#looking into second index of the data

dataset_cut1 = dataset[-lon_indx, ]

ggplot(dataset_cut1, aes(x = latitude, y = longitude)) +
  geom_point(alpha = 0.4)

#applying cluster analysis

kmeanCluster = kmeans(dataset_cut1, centers = 5, nstart = 30)
summary(kmeanCluster)

#centers of the cluster

clusterCenters = data.frame(kmeanCluster$centers)

ggplot(dataset_cut1, aes(x = latitude, y = longitude)) +
         geom_point(aes(col = factor(kmeanCluster$cluster)))
  

#setting variance for the input values accordingly and representing the risk for the cordinates


#data exploration for magnitude and month

magClean = eqClean[, c(5, 17, 18)]
magClean = magClean%>%
  mutate(mgIntensity = ifelse(magClean$mag < 4, "low", 
                              ifelse(magClean$mag <= 5.5, "medium", "high")))

indx_high = which(magClean$mgIntensity == "high")
indx_medium = which(magClean$mgIntensity == "medium")
summary(magClean[indx_high, ])
summary(magClean[indx_medium, ])

summ_month_mag = magClean %>%
  group_by(month, mgIntensity, day)%>%
  summarise(no = n())


ggplot(summ_month_mag, aes(x = month, fill = mgIntensity))+
  geom_histogram(bins = 12, binwidth =  1)+
  scale_x_continuous()+
  scale_y_continuous()+
  theme_classic()
