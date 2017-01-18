# 2017-01-17
# Code by Scott Ogletree
# Based on code from https://rpubs.com/bradleyboehmke/weather_graphic ,  Brad Boehmke January 2, 2015



# Preprocessing & summarizing data
library(dplyr)
library(tidyr)
library(lubridate)

# Visualization development
library(ggplot2)

# Variables
placename <- "Clemson"

#### Fetch Data ####
# This data can be obtained from weatherunderground too. For your area you will have to do some research.
# Fetch each year into a data frame
for (i in 1930:2016){
  urli <- paste("https://www.wunderground.com/history/airport/KCEU/", i, "/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=", i, "&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1", sep = "")
  name <- paste("Y", i, sep = "")
  assign(name,read.csv(urli))
}

# add all df to list so that we can apply over them
all_list <- mget( ls( pattern = "^Y[0-2][0-9][0-9][0-9]$" ) )
# Change all of the first columns to name 'Date'
names(all_list$Y1995)
for (i in 1:length(all_list)) names(all_list[[i]])[1] <- "Date"
# change the 'Date' type to date
for (i in 1:length(all_list)) all_list[[i]][,1] <- as.Date(all_list[[i]][,1])
str(all_list$Y1930)
# combine all data into a data frame
clem_wx <- do.call("rbind", all_list)
rm(list=setdiff(ls(), "clem_wx"))
# Reduce to the cols needed
DAY <- clem_wx %>% select(Date, Mean.TemperatureF)

DAY$Year <- as.numeric(format(DAY$Date,'%Y'))
DAY$Month <- as.numeric(format(DAY$Date,'%m'))
DAY$Day <- as.numeric(format(DAY$Date,'%d'))
# create col with day of year
#DAY$doy <- yday(DAY$Date)
names(DAY)[2] <- "Temp"
DAY[,2][is.na(DAY[, 2])] <- -101
# Deal with bogus numbers
DAY <-  DAY %>% mutate(Temp = replace(Temp, Temp< (-100), NA))
# make the day of year var
DAY$newDay <- yday(DAY$Date)
DAY <- DAY %>% select(-Date)
#---------------------------------------------------------------
# Step 
#---------------------------------------------------------------
# rename variables
# names(DAY) <- c("Month", "Day", "Year", "Temp")

# create dataframe that represents 1930-2015 historical data
# On the first try the day of year numbering isn't working. I will make it from the date.
Past <- DAY %>%
  # group_by(Year, Month) %>%
  # arrange(Day) %>%
  # ungroup() %>%
  # group_by(Year) %>%
  # arrange(Month, Day) %>% 
  # mutate(newDay = seq(1, length(Day))) %>%   # label days as 1:365 (will represent x-axis)         
  # ungroup() %>%
  filter(Temp != -99 & Year != 2016) %>%     # filter out missing data (identified with '-99' value) & current year data
  group_by(newDay) %>%
  mutate(upper = max(Temp), # identify max value for each day
         lower = min(Temp), # identify min value for each day
         avg = mean(Temp),  # calculate mean value for each day
         se = sd(Temp)/sqrt(length(Temp))) %>%  # calculate standard error of mean
  mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
         avg_lower = avg-(2.101*se)) %>%  # calculate 95% CI for mean
  ungroup()

# create dataframe that represents current year data
Present <- DAY %>%
  # group_by(Year, Month) %>%
  # arrange(Day) %>%
  # ungroup() %>%
  # group_by(Year) %>%
  # arrange(Month, Day) %>%
  # mutate(newDay = seq(1, length(Day))) %>%  # create matching x-axis as historical data
  # ungroup() %>%
  filter(Temp != -99 & Year == 2016)  # filter out missing data & select current year data
#---------------------------------------------------------------
# Step 
#---------------------------------------------------------------
# create dataframe that represents the lowest temp for each day for the historical data
PastLows <- Past %>%
  group_by(newDay) %>%
  summarise(Pastlow = min(Temp)) # identify lowest temp for each day from 1930-2015

# create dataframe that identifies the days in 2016 in which the temps were lower than all previous 19 years
PresentLows <- Present %>%
  left_join(PastLows) %>%  # merge historical lows to current year low data
  mutate(record = ifelse(Temp<Pastlow, "Y", "N")) %>% # identifies if current year was record low
  filter(record == "Y")  # filter for days that represent current year record lows

# create dataframe that represents the highest temp for each day for the historical data
PastHighs <- Past %>%
  group_by(newDay) %>%
  summarise(Pasthigh = max(Temp))  # identify highest temp for each day from 1930-2015

# create dataframe that identifies the days in 2016 in which the temps were higher than all previous 19 years
PresentHighs <- Present %>%
  left_join(PastHighs) %>%  # merge historical highs to current year low data
  mutate(record = ifelse(Temp>Pasthigh, "Y", "N")) %>% # identifies if current year was record high
  filter(record == "Y")  # filter for days that represent current year record highs

#---------------------------------------------------------------
# Step 
#---------------------------------------------------------------
# function to turn y-axis labels into degree formatted values
dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

# create y-axis variable
a <- dgr_fmt(seq(-20,100, by=10))

# create a small dataframe to represent legend symbol for 2016 Temperature
legend_data <- data.frame(x=seq(175,182),y=rnorm(8,15,2))

#########################################################
#### Creating the Visual ################################
#########################################################

p <- ggplot(Past, aes(newDay, Temp)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),  
        axis.title = element_blank()) +
  geom_linerange(Past, mapping=aes(x=newDay, ymin=lower, ymax=upper), colour = "wheat2", alpha=.1)

print(p)
#---------------------------------------------------------------
# Step 
#---------------------------------------------------------------
p <- p + 
  geom_linerange(Past, mapping=aes(x=newDay, ymin=avg_lower, ymax=avg_upper), colour = "wheat4")

print(p)
#---------------------------------------------------------------
# Step 
#---------------------------------------------------------------
p <- p + 
  geom_line(Present, mapping=aes(x=newDay, y=Temp, group=1)) +
  geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1)

print(p)
#---------------------------------------------------------------
# Step 
#---------------------------------------------------------------
p <- p + 
  geom_hline(yintercept = -20, colour = "white", linetype=1) +
  geom_hline(yintercept = -10, colour = "white", linetype=1) +
  geom_hline(yintercept = 0, colour = "white", linetype=1) +
  geom_hline(yintercept = 10, colour = "white", linetype=1) +
  geom_hline(yintercept = 20, colour = "white", linetype=1) +
  geom_hline(yintercept = 30, colour = "white", linetype=1) +
  geom_hline(yintercept = 40, colour = "white", linetype=1) +
  geom_hline(yintercept = 50, colour = "white", linetype=1) +
  geom_hline(yintercept = 60, colour = "white", linetype=1) +
  geom_hline(yintercept = 70, colour = "white", linetype=1) +
  geom_hline(yintercept = 80, colour = "white", linetype=1) +
  geom_hline(yintercept = 90, colour = "white", linetype=1) +
  geom_hline(yintercept = 100, colour = "white", linetype=1)

print(p)
#---------------------------------------------------------------
# Step 
#---------------------------------------------------------------
p <- p + 
  geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5) 

print(p)
#---------------------------------------------------------------
# Step 
#---------------------------------------------------------------
p <- p +
  coord_cartesian(ylim = c(-20,100)) +
  scale_y_continuous(breaks = seq(-20,100, by=10), labels = a) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("January", "February", "March", "April",
                                "May", "June", "July", "August", "September",
                                "October", "November", "December"))

print(p)
#---------------------------------------------------------------
# Step 
#---------------------------------------------------------------
p <- p +
  geom_point(data=PresentLows, aes(x=newDay, y=Temp), colour="blue3") +
  geom_point(data=PresentHighs, aes(x=newDay, y=Temp), colour="firebrick3")

print(p)
#---------------------------------------------------------------
# Step 
#---------------------------------------------------------------
p <- p +
  ggtitle(paste(placename, "'s Weather in 2016", sep = "")) +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20)) +
  annotate("text", x = 19, y = 98, label = "Temperature", size=4, fontface="bold")

print(p)
#---------------------------------------------------------------
# Step 
#---------------------------------------------------------------
# there might be better annotation now in ggplot2
p <- p +
  annotate("text", x = 66, y = 93, 
           label = "Data represents average daily temperatures. Accessible data dates back to", size=3, colour="gray30") +
  annotate("text", x = 62, y = 89, 
           label = "January 1, 1930. Data for 2016 is through December 31.", size=3, colour="gray30") +
  annotate("text", x = 64, y = 85, 
           label = "Average temperature for the year was 51.9° making 2014 the 9th coldest", size=3, colour="gray30") +
  annotate("text", x = 18, y = 81, label = "year since 1995", size=3, colour="gray30")

print(p)
#---------------------------------------------------------------
# Step 
#---------------------------------------------------------------
# the exact placement of these elements will need some trial and error
p <- p +
  annotate("segment", x = 30, xend = 40, y = -5, yend = -10, colour = "blue3") +
  annotate("text", x = 65, y = -10, label = "We had 35 days that were the", size=3, colour="blue3") +
  annotate("text", x = 56, y = -14, label = "coldest since 1995", size=3, colour="blue3") +
  annotate("segment", x = 302, xend = 307, y = 74, yend = 82, colour = "firebrick3") +
  annotate("text", x = 333, y = 82, label = "We had 19 days that were the", size=3, colour="firebrick3") +
  annotate("text", x = 324, y = 78, label = "hottest since 1995", size=3, colour="firebrick3")

print(p)
#---------------------------------------------------------------
# Step 
#---------------------------------------------------------------
p <- p +
  annotate("segment", x = 181, xend = 181, y = 5, yend = 25, colour = "wheat2", size=3) +
  annotate("segment", x = 181, xend = 181, y = 12, yend = 18, colour = "wheat4", size=3) +
  geom_line(data=legend_data, aes(x=x,y=y)) +
  annotate("segment", x = 183, xend = 185, y = 17.7, yend = 17.7, colour = "wheat4", size=.5) +
  annotate("segment", x = 183, xend = 185, y = 12.2, yend = 12.2, colour = "wheat4", size=.5) +
  annotate("segment", x = 185, xend = 185, y = 12.2, yend = 17.7, colour = "wheat4", size=.5) +
  annotate("text", x = 196, y = 14.75, label = "NORMAL RANGE", size=2, colour="gray30") +
  annotate("text", x = 162, y = 14.75, label = "2016 TEMPERATURE", size=2, colour="gray30") +
  annotate("text", x = 193, y = 25, label = "RECORD HIGH", size=2, colour="gray30") +
  annotate("text", x = 193, y = 5, label = "RECORD LOW", size=2, colour="gray30")

print(p)

