library(readxl)
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)


### Data Cleaning
cook_mobility <- data.table(fread("/Users/jeffmcbass/econ21320/Global_Mobility_Report.csv",header=TRUE,sep=","))[133801:133876]
cook_initclaim <- data.table(read_excel("/Users/jeffmcbass/econ21320/County_Claims.xlsx",skip = 3))[16]
cook_crimes <- data.table(fread("/Users/jeffmcbass/econ21320/Crimes_-_2020.csv",header=TRUE,sep=","))
IL_initclaims <- data.table(read_excel("/Users/jeffmcbass/econ21320/state_weeklyinitclaims.xlsx",skip = 4))

cook_mobility$pubavg <- (cook_mobility$retail_and_recreation_percent_change_from_baseline + 
                           cook_mobility$grocery_and_pharmacy_percent_change_from_baseline+
                           cook_mobility$parks_percent_change_from_baseline+
                           cook_mobility$transit_stations_percent_change_from_baseline+
                           cook_mobility$workplaces_percent_change_from_baseline)/5

cook_crimes$asdate <- as.Date(substr(as.character(cook_crimes$Date),0,9),"%m/%d/%y")
crimesperday <- as.data.frame(table(cook_crimes$asdate))
crimesperday$Date <- as.Date(x=crimesperday$Var1)

### Making Crimes data into Weekly Averages:
first <- as.Date("2020-01-11")
last <- as.Date("2020-04-25")
dt <- last-first
crimesperday$week <- floor_date(crimesperday$Date,"week")+6
crimes_weeklyavg <- ddply(crimesperday, .(week), function(z) mean(z$Freq))
colnames(crimes_weeklyavg) <- c("week", "Average Crimes per Week")


### Data Frames
series <- data.table(date=c(seq(from = as.Date("2020-02-15"), to = as.Date("2020-05-01"), by='day')),
                     crimes_per_day=c(crimesperday$Freq[46:120],NA,NA),
                     mobility_perc_change=c(cook_mobility$pubavg,NA))

wdata <- data.table(date=c(seq(from=as.Date("2020-01-04"), to = as.Date("2020-04-25"), by = 'week')),
                    IL_UI_initclaims = c(IL_initclaims$`Initial Claims`[53:69]),
                    crimes_week = c(crimes_weeklyavg$`Average Crimes per Week`)[2:18])

View(wdata)



### Plots
crime_t <- ggplot(series, aes(x=date, y=crimes_per_day)) +
  geom_line(color="#69b3a2", size=1) +
  geom_vline(xintercept=as.Date("2020-03-21"), color="red", size=.5)+
  ggtitle("Crimes per day")
  

mob_t <- ggplot(series, aes(x=date, y=mobility_perc_change)) +
  geom_line(color="grey",size=1) +
  geom_vline(xintercept=as.Date("2020-03-21"), color="red", size=.5)+
  ggtitle("Mobility % Change")

cookUI_t <- barplot(c(13112,176953,228451),
              main = "Cook County Monthly UI Initial Claims",
              xlab = "Month",
              ylab = "Number of Claims",
              names.arg = c("Feb","Mar","Apr"),
              col = "red")

ILUI_t <- ggplot(wdata,aes(x=date,y=IL_UI_initclaims))+
  geom_line(color="darkred",size=1)+
  geom_point(shape=21, color="black", fill="black",size=3)+
  geom_vline(xintercept=as.Date("2020-03-21"),color="red",size=.5)+
  ggtitle("IL State Weekly UI Initial Claims")

crime_mob_scatter <- ggplot(series, aes(x=mobility_perc_change,y=crimes_per_day))+
  geom_point()+
  ggtitle("Crimes vs. Mobility Scatter")

crime_UI_scatter <- ggplot(wdata, aes(x=IL_UI_initclaims, y=crimes_week))+
  geom_point()+
  ggtitle("Crimes vs. State_UI Scatter")

crime_mob_scatter
crime_UI_scatter







