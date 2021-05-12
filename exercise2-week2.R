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

#####TASK 1- getting an overview-------
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

#####task 2 - Deriving movement parameters I: Speed------------

#calculate the Euclidean distance 
wildschwein_BE <- group_by(wildschwein_BE,TierID)
E1 <- c(wildschwein_BE$E)
E2 <-lead(wildschwein_BE$E,1, na.re=TRUE)
N1 <- c(wildschwein_BE$N)
N2 <-lead(wildschwein_BE$N,1, na.re=TRUE)

#add column steplength
wildschwein_BE <- mutate(wildschwein_BE,steplength = sqrt((E1-E2)^2+(N1-N2)^2))

wildschwein_BE <- wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(
    steplength = sqrt((E-lead(E))^2+(N-lead(N))^2)
  )

#calculate the speed by dividing steplenth / timelag
wildschwein_BE <- mutate(wildschwein_BE,speed = (steplength/timelag))
##ERROR occurs!! im not sure why??

# I try again with piping:
wildschwein_BE <- wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC,units = "secs")),
    steplength = sqrt((E-lead(E))^2+(N-lead(N))^2),
    speed = steplength/timelag)


####Task 3: Cross-scale movement analysis------------------
caro = read.delim("caro60.csv",sep=",",dec=".",header=TRUE)
#Data cleansing
#set timezone
class(caro60$DatetimeUTC) # here you are referring $caro60$DatetimeUTC to a non existing dataset
                          # you defined as caro -- caro is later assigned to

# Why did you do that? That's not necessary!!
caro$DatetimeUTC <- as.POSIXct(as.character(caro$DatetimeUTC), format = "%Y-%m-%dT%H:%M:%SZ",tz = "UTC")

#transform dataframe into sf -->  N, and E are already in CH1903+ LV95 Format , therfore crs 2056
caro60 = st_as_sf(caro, 
                  coords = c("N", "E"), 
                  crs = 2056)

#no reduce the granularity of our sampling interval by selecting every 3rd, 6th and 9th position
nrow(caro)
## [1] 200


movement_analysis_by_interval <- function(datenset, interval){
  ###slice  to subset the dataset by row number---------
  caro_i <- slice(datenset, seq(1,200,interval))
  print(nrow(caro_i))

  #calculate timelag, steplength and speed in one for each subset
  caro_i <- caro_i %>%
    group_by(TierID) %>%
    mutate(timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC,units = "secs")),
           steplength = sqrt((E-lead(E))^2+(N-lead(N))^2),
           speed = (steplength/timelag),
           trajectory =  paste(toString(interval), "min")) 
  
  #calculate timelag, steplength and speed in one for reference (=whole dataset)
  datenset <- datenset %>%
    group_by(TierID) %>%
    mutate(timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC,units = "secs")),
           steplength = sqrt((E-lead(E))^2+(N-lead(N))^2),
           speed = (steplength/timelag),
           trajectory =  paste(toString(1), "min")) 
  
  #bind data together 
  #1min and 3min 
  caro_1_i <-rbind(datenset,caro_i)
  
  my_plot = ggplot(caro_1_i)+
    geom_path(mapping= aes(x=N, y=E, color=trajectory))+
    geom_point(mapping= aes(x=N, y=E, color=trajectory))
  
  print(my_plot)
  
  return(caro_i)
  
}

## NICE WORK WITH THE FUNCTION ## 
## VERY EFFICIENT WAY TO GO THROUGH -- WELL DONE!!

caro_1 <-movement_analysis_by_interval(caro, 1)
caro_3 <-movement_analysis_by_interval(caro, 3)
caro_6 <-movement_analysis_by_interval(caro, 6)
caro_9 <-movement_analysis_by_interval(caro, 9)

#make one set of all data
caro_all <- rbind(caro_1, caro_3, caro_6, caro_9)

#comparing derived speed at different sampling intervals
ggplot(caro_all)+
  geom_line(mapping = aes(x=DatetimeUTC, y=speed, color=trajectory))

###task 4- Deriving movement parameters II: Rolling window functions---------------
library(zoo)
#k is windowsize! 
example <- rnorm(10)
example
rollmean(example,k = 3,fill = NA,align = "left")
##  [1]  0.93634335  0.31709038  0.02370048  0.67869801  0.73369105  0.50401344
##  [7] -0.56144365 -0.56902598          NA          NA
rollmean(example,k = 4,fill = NA,align = "left")
##  [1]  0.6775521  0.2045005  0.5848215  0.5255629  0.3446928  0.1459635
##  [7] -0.4102301         NA         NA         NA

#now run rollmean on the speed variable of the subset (caro_all?)

rolling_window_function <- function(fenster){
  caro_all_k <- mutate(caro_all, movingwindow = rollmean(caro_all$speed, k=fenster, fill=NA, align="left"))
  
  myplot <-ggplot(caro_all_k)+
    geom_line(mapping = aes(x=DatetimeUTC, y=movingwindow, color=trajectory))
  #print the plot
  print(myplot)
  

}

#try different k values (fenster)
fenster_3 <- rolling_window_function(3) 
fenster_5 <- rolling_window_function(5)
fenster_8 <- rolling_window_function(8)

fenster_3
      
