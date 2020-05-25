library(readxl)
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)




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

### Different types of crimes:
cook_crimes_dom <- cook_crimes[cook_crimes$Domestic == TRUE,]
domcrimesperday <- as.data.frame(table(cook_crimes_dom$asdate))


cook_crimes_ass <- cook_crimes[cook_crimes$'Primary Type' == 'ASSAULT',]
ass_perday <- as.data.frame(table(cook_crimes_ass$asdate))

cook_crimes_bat <- cook_crimes[cook_crimes$'Primary Type' == 'BATTERY',]
bat_perday <- as.data.frame(table(cook_crimes_bat$asdate))

cook_crimes_bur <- cook_crimes[cook_crimes$'Primary Type' == 'BURGLARY',]
bur_perday <- as.data.frame(table(cook_crimes_bur$asdate))

cook_crimes_narc <- cook_crimes[cook_crimes$'Primary Type' == 'NARCOTICS',]
narc_perday <- as.data.frame(table(cook_crimes_narc$asdate))

cook_crimes_dam <- cook_crimes[cook_crimes$'Primary Type' == 'CRIMINAL DAMAGE',]
dam_perday <- as.data.frame(table(cook_crimes_dam$asdate))

cook_crimes_rob <- cook_crimes[cook_crimes$'Primary Type' == 'ROBBERY',]
rob_perday <- as.data.frame(table(cook_crimes_rob$asdate))

cook_crimes_thft <- cook_crimes[cook_crimes$'Primary Type' == 'THEFT',]
thft_perday <- as.data.frame(table(cook_crimes_thft$asdate))

cook_crimes_motthft <- cook_crimes[cook_crimes$'Primary Type' == 'MOTOR VEHICLE THEFT',]
motthft_perday <- as.data.frame(table(cook_crimes_motthft$asdate))

cook_crimes_other <- cook_crimes[cook_crimes$'Primary Type' == 'OTHER OFFENSE',]
other_perday <- as.data.frame(table(cook_crimes_other$asdate))


### Crimes by location:
cook_crimes_res <- cook_crimes[cook_crimes$'Location Description' == 'APARTMENT'|
                                 cook_crimes$`Location Description` == 'RESIDENCE'|
                                 cook_crimes$`Location Description` == 'RESIDENCE - GARAGE'|
                                 cook_crimes$`Location Description` == 'RESIDENCE - PORCH / HALLWAY'|
                                 cook_crimes$`Location Description` == 'RESIDENCE PORCH/HALLWAY'|
                                 cook_crimes$`Location Description` == 'RESIDENCE - YARD (FRONT / BACK)'|
                                 cook_crimes$`Location Description` == 'RESIDENCE-GARAGE'|
                                 cook_crimes$`Location Description` == 'RESIDENCE YARD (FRONT/BACK)'|
                                 cook_crimes$`Location Description` == 'DRIVEWAY - RESIDENTIAL',]
res_perday <- as.data.frame(table(cook_crimes_res$asdate))

cook_crimes_tran <- cook_crimes[cook_crimes$'Location Description' == 'CTA ""L"" PLATFORM'|
                                  cook_crimes$'Location Description' == 'CTA BUS'|
                                  cook_crimes$'Location Description' == 'CTA BUS STOP'|
                                  cook_crimes$'Location Description' == 'CTA GARAGE / OTHER PROPERTY'|
                                  cook_crimes$'Location Description' == 'CTA PARKING LOT / GARAGE / OTHER PROPERTY'|
                                  cook_crimes$'Location Description' == 'CTA PLATFORM'|
                                  cook_crimes$'Location Description' == 'CTA PROPERTY'|
                                  cook_crimes$'Location Description' == 'CTA STATION'|
                                  cook_crimes$'Location Description' == 'CTA TRACKS - RIGHT OF WAY'|
                                  cook_crimes$'Location Description' == 'CTA TRAIN',]
tran_perday <- as.data.frame(table(cook_crimes_tran$asdate))

cook_crimes_gro <- cook_crimes[cook_crimes$`Location Description` == 'CONVENIENCE STORE'|
                                 cook_crimes$`Location Description` == 'DRUG STORE'|
                                 cook_crimes$`Location Description` == 'GROCERY FOOD STORE',]
gro_perday <- as.data.frame(table(cook_crimes_gro$asdate))

cook_crimes_ret <- cook_crimes[cook_crimes$`Location Description` == 'SMALL RETAIL STORE'|
                                 cook_crimes$`Location Description` == 'RETAIL STORE'|
                                 cook_crimes$`Location Description` == 'RESTAURANT'|
                                 cook_crimes$`Location Description` == 'DEPARTMENT STORE'|
                                 cook_crimes$`Location Description` == 'APPLIANCE STORE'|
                                 cook_crimes$`Location Description` == 'BAR OR TAVERN'|
                                 cook_crimes$`Location Description` == 'BARBERSHOP',]
ret_perday <- as.data.frame(table(cook_crimes_ret$asdate))

cook_crimes_streets <- cook_crimes[cook_crimes$`Location Description` == 'SIDEWALK'|
                                     cook_crimes$`Location Description` == 'STREET'|
                                     cook_crimes$`Location Description` == 'ALLEY',]
streets_perday <- as.data.frame(table(cook_crimes_streets$asdate))

cook_crimes_park <- cook_crimes[cook_crimes$`Location Description` == 'PARK PROPERTY',]
park_perday <- as.data.frame(table(cook_crimes_park$asdate))

cook_crimes_off <- cook_crimes[cook_crimes$`Location Description` == 'COMMERCIAL / BUSINESS OFFICE',]
off_perday <- as.data.frame(table(cook_crimes_off$asdate))

### NYC DATA
nyc_mobility <- data.table(fread("/Users/jeffmcbass/econ21320/Global_Mobility_Report.csv",header=TRUE,sep=","))[213467:213542]
nyc_mobility$pubavg <- (nyc_mobility$retail_and_recreation_percent_change_from_baseline+
                        nyc_mobility$grocery_and_pharmacy_percent_change_from_baseline+
                          nyc_mobility$parks_percent_change_from_baseline+
                          nyc_mobility$transit_stations_percent_change_from_baseline+
                          nyc_mobility$workplaces_percent_change_from_baseline)/5




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

DF_cookmob <- data.table(date = c(seq(from = as.Date("2020-02-15"), to = as.Date("2020-04-30"), by='day')),
                        retail_recreation = c(cook_mobility$retail_and_recreation_percent_change_from_baseline),
                        grocery_pharmacy = c(cook_mobility$grocery_and_pharmacy_percent_change_from_baseline),
                        parks = c(cook_mobility$parks_percent_change_from_baseline),
                        transit_station = c(cook_mobility$transit_stations_percent_change_from_baseline),
                        workplaces = c(cook_mobility$workplaces_percent_change_from_baseline),
                        residential = c(cook_mobility$residential_percent_change_from_baseline))

DF_cookmob <- melt(DF_cookmob, id.vars = 'date', variable.name = 'series')

nycdata <- data.table(date=c(seq(from = as.Date("2020-02-15"), to = as.Date("2020-05-01"), by = 'day')),
                      NYC_mobility_perc_change = c(nyc_mobility$pubavg,NA))

DF_nycmob <- data.table(date = c(seq(from = as.Date("2020-02-15"), to = as.Date("2020-04-30"), by='day')),
                        retail_recreation = c(nyc_mobility$retail_and_recreation_percent_change_from_baseline),
                        grocery_pharmacy = c(nyc_mobility$grocery_and_pharmacy_percent_change_from_baseline),
                        parks = c(nyc_mobility$parks_percent_change_from_baseline),
                        transit_station = c(nyc_mobility$transit_stations_percent_change_from_baseline),
                        workplaces = c(nyc_mobility$workplaces_percent_change_from_baseline),
                        residential = c(nyc_mobility$residential_percent_change_from_baseline))

DF_nycmob <- melt(DF_nycmob, id.vars = 'date', variable.name = 'series')

DF_Dom_crimes_t <- data.table(date=c(seq(from = as.Date("2020-01-01"), to = as.Date("2020-04-29"), by = 'day')),
                           dom_crimes_per_day = c(domcrimesperday$Freq))

DF_types_crimes <- data.table(date = c(seq(from = as.Date("2020-01-01"), to = as.Date("2020-04-29"), by='day')),
                              assaults = c(ass_perday$Freq),
                              battery = c(bat_perday$Freq),
                              burglary = c(bur_perday$Freq),
                              criminal_damage = c(dam_perday$Freq),
                              robbery = c(rob_perday$Freq),
                              motor_theft = c(motthft_perday$Freq),
                              domestic = c(domcrimesperday$Freq),
                              other = c(other_perday$Freq))
DF_types_crimes <- melt(DF_types_crimes, id.vars = 'date', variable.name = 'series')

DF_loc_crimes <- data.table(date = c(seq(from = as.Date("2020-01-01"), to = as.Date("2020-04-29"), by='day')),
                            residential = c(res_perday$Freq),
                            transportation = c(tran_perday$Freq),
                            groceries_pharma = c(gro_perday$Freq),
                            retail = c(ret_perday$Freq),
                            streets = c(streets_perday$Freq),
                            park = c(park_perday$Freq),
                            office = c(off_perday$Freq))
DF_loc_crimes <- melt(DF_loc_crimes, id.vars = 'date', variable.name = 'series')
### Plots
crime_t <- ggplot(series, aes(x=date, y=crimes_per_day)) +
  geom_line(color="#69b3a2", size=1) +
  geom_vline(xintercept=as.Date("2020-03-13"), color="red", size=.5)+
  ggtitle("Crimes per day")
  

cook_pubavgmob_t <- ggplot(series, aes(x=date, y=mobility_perc_change)) +
  geom_line(color="grey",size=1) +
  geom_vline(xintercept=as.Date("2020-03-13"), color="red", size=.5)+
  ggtitle("Mobility % Change")



ILUI_t <- ggplot(wdata,aes(x=date,y=IL_UI_initclaims))+
  geom_line(color="darkred",size=1)+
  geom_point(shape=21, color="black", fill="black",size=3)+
  geom_vline(xintercept=as.Date("2020-03-13"),color="red",size=.5)+
  ggtitle("IL State Weekly UI Initial Claims")

crime_mob_scatter <- ggplot(series, aes(x=mobility_perc_change,y=crimes_per_day))+
  geom_point()+
  ggtitle("Crimes vs. Mobility Scatter")

crime_UI_scatter <- ggplot(wdata, aes(x=IL_UI_initclaims, y=crimes_week))+
  geom_point()+
  ggtitle("Crimes vs. State_UI Scatter")

NYC_avgmob_t <- ggplot(nycdata, aes(x=date, y=NYC_mobility_perc_change)) +
  geom_line(color="blue",size=1) +
  geom_vline(xintercept=as.Date("2020-03-13"), color="red", size=.5)+
  ggtitle("NYC Public Average Mobility % Change")

NYC_mob_t <- ggplot(DF_nycmob, aes(date,value)) + geom_line(aes(colour = series))+
  geom_vline(xintercept=as.Date("2020-03-13"), color="red", size=.5)+
  ggtitle("NYC Mobility % Change from Baseline")

Cook_mob_t <- ggplot(DF_cookmob, aes(date,value)) + geom_line(aes(colour = series))+
  geom_vline(xintercept=as.Date("2020-03-13"), color="red", size=.5)+
  #geom_point(shape=21, color="black", fill="black",size=1)+
  ggtitle("Cook County Mobility % Change from Baseline")

dom_crime_t <- ggplot(DF_Dom_crimes_t, aes(x=date, y=dom_crimes_per_day)) +
  geom_line(color="pink", size=1) +
  geom_vline(xintercept=as.Date("2020-03-13"), color="red", size=.5)+
  ggtitle("Domestic Crimes per day")

types_crime <- ggplot(DF_types_crimes, aes(date,value)) + geom_line(aes(colour = series)) +
  geom_vline(xintercept=as.Date("2020-03-13"), color = "red", size = 0.5) +
  ggtitle("Chicago Crimes by Type")

loc_crime <- ggplot(DF_loc_crimes, aes(date, value)) + geom_line(aes(colour = series)) +
  geom_vline(xintercept=as.Date("2020-03-13"), color = "red", size = 0.5) +
  ggtitle("Chicago Crimes by Location")

NYC_mob_t
Cook_mob_t
dom_crime_t
types_crime
loc_crime