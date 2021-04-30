#exercise 2 - Alissa Tophinke

#Task 0: Import your data
## Load the necessary libraries ################################################

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

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
h <-summarise(wildschwein_BE, mean = mean(timelag, na.rm = T))
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

#task 2 - Deriving movement parameters I: Speed


