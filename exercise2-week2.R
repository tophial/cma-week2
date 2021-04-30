#exercise 2 - Alissa Tophinke

#Task 0: Import your data
## Load the necessary libraries ################################################

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times
library(tidyr)
## Import the downloaded csv ##################################################

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",") # adjust path
#setting remove = FALSE preserves the original (E/N) column:
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

#TASK 1- getting an overview-------
#new column "timelag" using lead
wildschwein_BE <- mutate(wildschwein_BE,timelag = as.numeric(difftime(lead(DatetimeUTC),
                                                                      DatetimeUTC,
                                                                      units = "secs")))
summary(wildschwein_BE$timelag)

# timelag should just be calculated between subsequent rows of the same individual
#therefore we have to makes group of the animals by using TierID

wildschwein_BE <- group_by(wildschwein_BE,TierID)
wildschwein_BE <- mutate(wildschwein_BE,timelag = as.numeric(difftime(lead(DatetimeUTC),
                                                                      DatetimeUTC,
  
                                                                                                                                        units = "secs")))
summary(wildschwein_BE$timelag)

#Q1: How many individuals were tracked?
#calculate mean of each (TierID) group
summarise(wildschwein_BE, mean = mean(timelag, na.rm = T))
#with piping and removing of the last column gemetry:
wildschwein_BE %>%                     
  st_set_geometry(NULL) %>%            
  group_by(TierID) %>%                 
  summarise(mean_timelag = mean(timelag,na.rm = T))


#plot a histogramm
ggplot(wildschwein_BE, mapping = aes(x=timelag))+
  geom_histogram(binwidth = 10)+
  coord_cartesian(xlim = c(0, 15000))+
  scale_y_log10()

#Q2: For how long were the individual tracked? Are there gaps?
ggplot(wildschwein_BE, aes(x=TierID, na.rm = TRUE)) + 
  geom_linerange(aes(ymin=DatetimeUTC, ymax=DatetimeUTC + timelag), size=1)+
  coord_flip()+
  ylab("DatetimeUTC")

#Q3: Were all individuals tracked concurrently or sequentially?
#extract the months (sep-jan) out of DateitmeUTC first:

wildschwein_BE_14 <-wildschwein_BE %>%
  filter(year(DatetimeUTC)  == 2014)  

ggplot(wildschwein_BE_14, aes(x=DatetimeUTC, y=timelag, color=TierID, na.re=TRUE)) + 
  geom_point()+
  geom_line()
#What is the temporal sampling interval between the locations?

#task 2 - Deriving movement parameters I: Speed------------

#calculate the Euclidean distance 
wildschwein_BE <- group_by(wildschwein_BE,TierID)
E1 <- c(wildschwein_BE$E)
E2 <-lead(wildschwein_BE$E,1, na.re=TRUE)
N1 <- c(wildschwein_BE$N)
N2 <-lead(wildschwein_BE$N,1, na.re=TRUE)

#add column steplength
wildschwein_BE <- mutate(wildschwein_BE,steplength = sqrt((E1-E2)^2+(N1-N2)^2))
#calculate the speed by dividing steplenth / timelag
wildschwein_BE <- mutate(wildschwein_BE,speed = (steplength/timelag))
##ERROR occurs!! im not sure why??

#try again with piping:
wildschwein_BE <- wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC,units = "secs")),
    steplength = sqrt((E-lead(E))^2+(N-lead(N))^2),
    speed = steplength/timelag)


#Task 3: Cross-scale movement analysis------------------
caro = read.delim("caro60.csv",sep=",",dec=".",header=TRUE)
#Data cleansing
#set timezone
class(caro60$DatetimeUTC)
caro$DatetimeUTC <- as.POSIXct(as.character(caro$DatetimeUTC), format = "%Y-%m-%dT%H:%M:%SZ",tz = "UTC")

#transform dataframe into sf -->  N, and E are already in CH1903+ LV95 Format , therfore crs 2056
caro60 = st_as_sf(caro, 
                  coords = c("N", "E"), 
                  crs = 2056)

#no reduce the granularity of our sampling interval by selecting every 3rd, 6th and 9th position
nrow(caro60)
## [1] 200

###slice  to subset the dataset by row number---------
caro60_3<-slice(caro60, seq(1,200,3))
nrow(caro60_3)
## [1] 67
caro60_6<-slice(caro60, seq(1,200,6))
nrow(caro60_6)
## [1] 34
caro60_9<-slice(caro60, seq(1,200,9))
nrow(caro60_9)
## [1] 23


###calculate timelag for each subset--------

caro60 <- group_by(caro60,TierID)
caro60 <- mutate(caro60,timelag = as.numeric(difftime(lead(DatetimeUTC),
                                                      DatetimeUTC,
                                                      units = "sec")))
caro60_3 <- group_by(caro60_3,TierID)
caro60_3 <- mutate(caro60_3,timelag = as.numeric(difftime(lead(DatetimeUTC),
                                                          DatetimeUTC,
                                                          units = "sec")))
caro60_6 <- group_by(caro60_6,TierID)
caro60_6 <- mutate(caro60_6,timelag = as.numeric(difftime(lead(DatetimeUTC),
                                                          DatetimeUTC,
                                                          units = "sec")))
caro60_9 <- group_by(caro60_9,TierID)
caro60_9 <- mutate(caro60_9,timelag = as.numeric(difftime(lead(DatetimeUTC),
                                                          DatetimeUTC,
                                                          units = "sec")))
###calculate steplength for each subset-----------------

# Store coordinates in a new variable
coordinates <- st_coordinates(caro60)
coordinates_3 <- st_coordinates(caro60_3)
coordinates_6 <- st_coordinates(caro60_6)
coordinates_9 <- st_coordinates(caro60_9)

head(coordinates)
#rename the columns 
colnames(coordinates) <- c("E","N")
colnames(coordinates_3) <- c("E","N")
colnames(coordinates_6) <- c("E","N")
colnames(coordinates_9) <- c("E","N")
#use cbind() to “glue” the columns to our original sf-object.
caro60 <- cbind(caro60,coordinates)
caro60_3 <- cbind(caro60_3,coordinates_3)
caro60_6 <- cbind(caro60_6,coordinates_6)
caro60_9 <- cbind(caro60_9,coordinates_9)
head(caro60)

#make extra column trajectory
caro60 <- mutate(caro60,trajectory = "1min")
caro60_3 <-mutate(caro60_3,trajectory = "3min")
caro60_6 <-mutate(caro60_6,trajectory = "6min")
caro60_9 <-mutate(caro60_9,trajectory = "9min")

#bind data together 
#1min and 3min 
caro_1_3 <-rbind(caro60,caro60_3)
#1min and 6min 
caro_1_6 <-rbind(caro60,caro60_6)
#1min and 9min 
caro_1_9 <-rbind(caro60,caro60_9)

#compare 1min, 3min in a plot
ggplot(caro_1_3)+
  geom_path(mapping= aes(x=N, y=E, color=trajectory))+
  geom_point(mapping= aes(x=N, y=E, color=trajectory))
#compare 1min, 6min in a plot
ggplot(caro_1_6)+
  geom_path(mapping= aes(x=N, y=E, color=trajectory))+
  geom_point(mapping= aes(x=N, y=E, color=trajectory))
#compare 1min, 9min in a plot
ggplot(caro_1_9)+
  geom_path(mapping= aes(x=N, y=E, color=trajectory))+
  geom_point(mapping= aes(x=N, y=E, color=trajectory))


###calculate speed---------------


#steplength calculation for 1min
E1 <- c(caro60$E)
E2 <-lead(caro60$E,1)
N1 <- c(caro60$N)
N2 <-lead(caro60$N,1)

caro60 <- mutate(caro60,steplength = sqrt((E1-E2)^2+(N1-N2)^2))
caro60 <- mutate(caro60,speed = (steplength/timelag))

#steplength calculation for 3min
E1_3 <- c(caro60_3$E)
E2_3 <-lead(caro60_3$E,1)
N1_3 <- c(caro60_3$N)
N2_3 <-lead(caro60_3$N,1)

caro60_3 <- mutate(caro60_3,steplength = sqrt((E1_3-E2_3)^2+(N1_3-N2_3)^2))
caro60_3 <- mutate(caro60_3,speed = (steplength/timelag))

#steplength calculation for 6min
E1_6 <- c(caro60_6$E)
E2_6 <-lead(caro60_6$E,1)
N1_6 <- c(caro60_6$N)
N2_6 <-lead(caro60_6$N,1)

caro60_6 <- mutate(caro60_6,steplength = sqrt((E1_6-E2_6)^2+(N1_6-N2_6)^2))
caro60_6 <- mutate(caro60_6,speed = (steplength/timelag))

#steplength calculation for 9min
E1_9 <- c(caro60_9$E)
E2_9 <-lead(caro60_9$E,1)
N1_9 <- c(caro60_9$N)
N2_9 <-lead(caro60_9$N,1)

caro60_9 <- mutate(caro60_9,steplength = sqrt((E1_9-E2_9)^2+(N1_9-N2_9)^2))
caro60_9 <- mutate(caro60_9,speed = (steplength/timelag))


geschw <- rbind(caro60, caro60_3, caro60_6, caro60_9)

#plot the speed
ggplot(geschw)+
  geom_line(mapping = aes(x=DatetimeUTC, y=speed, color=trajectory))

