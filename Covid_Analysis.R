library(tidyverse)
#install.packages("data.table")
library(data.table)
library(dplyr)
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("lubridate")
library(lubridate)
library(stringr)
#install.packages("plotly")
#install.packages("scales")
#install.packages("RColorBrewer")
library(RColorBrewer)
library(scales)

library(plotly)
library(mlbench)
library(modelr)
library(nlme)
library(sp)
#install.packages("sp")
library(usmap)
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"
customYel = "#C9C271"

countries_cnt <- 12
covid_19 <- data.table(read.csv('C://Users/spati/OneDrive/Desktop/covid_19/covid_19_data.csv'))
summary(covid_19)

covid_19[str_length(ObservationDate)==8,ObservationDate1:=as.Date(ObservationDate,format='%d-%m-%y',origin = orgin)]
covid_19[str_length(ObservationDate)==10,ObservationDate1:=as.Date(ObservationDate,format='%m/%d/%Y',origin = orgin)]
#covid_19[str_length(ObservationDate)==10,ObservationDate1:=as.Date(ObservationDate,format='%d-%m-%y',origin = orgin)]


covid_19[,ObservationDate:=ObservationDate1]

covid_19$ObservationDate1=NULL

#covid_19$ObservationDate <- format(covid_19$ObservationDate, "%Y-%m-%d")

#FILLING MISSING VALUES

covid_19[grep('Korea',Country.Region),Country.Region:='South Korea']
covid_19[grep('Iran',Country.Region),Country.Region:='Iran']
covid_19[grep('Taiwan',Province.State),Country.Region:='Taiwan']
covid_19[ grepl('Diamond Princess cruise ship',Province.State),Country.Region:='Diamond Princess(SHIP)']
covid_19[ grepl('Cruise Ship',Province.State),Country.Region:='Diamond Princess(SHIP)']
covid_19[grepl('Italy',Country.Region),Country.Region:='Italy']

coronaData <- covid_19[,c(2,4,6:8)]

covid.by.country <- coronaData[,.(Confirmed=sum(Confirmed),Recovered=sum(Recovered),Deaths=sum(Deaths)),.(ObservationDate,Country.Region)]
covid.date <- covid.by.country %>% 
  group_by(ObservationDate) %>%
  summarise(Confirmed = sum(Confirmed,na.rm=TRUE),Death = sum(Deaths,na.rm=TRUE))

#typeof(covid.date$ObservationDate)


scatterPlot <- covid.date %>% 
  ggplot(aes(x = ObservationDate, y = Confirmed)) + 
  geom_line(alpha=1.0, colour = "#51A0D5") +
  labs(x = "Date", 
       y = "Confirmed Cases",
       title = "Worldwide Confirmed Cases Over Time") +
  scale_y_continuous(labels = comma) +
  theme_gray()
  

ggplotly(scatterPlot)
#ggsave('wwc_1stplot.png')
#-------------log--------------------

scatterPlot1 <- covid.date %>% 
  ggplot(aes(x = ObservationDate, y = log(Confirmed))) + 
  geom_line(alpha=1.0, colour = "#51A0D5") +
  labs(x = "Date", 
       y = "Log of Confirmed Cases",
       title = "Log Of Worldwide Confirmed Cases Over Time") +
  scale_y_continuous(labels = comma) +
  theme_gray()


ggplotly(scatterPlot1)
#ggsave('wwc_2ndlog.png')

#-----------------------------------------------

g_death <- covid.date %>% 
  ggplot(aes(x = ObservationDate, y = Death)) + 
  geom_line(alpha=1.0, colour = "RED") + 
  labs(x = "Date", 
       y = "deaths",
       title = "Worldwide Deaths Over Time") +
  scale_y_continuous(name="Deaths", labels = scales::comma) +
  theme_gray()

ggplotly(g_death)
#ggsave("wwd3rd.png")
#------------------------------------------------------
g_death1 <- covid.date %>% 
  ggplot(aes(x = ObservationDate, y = log(Death))) + 
  geom_line(alpha=1.0, colour = "RED") + 
  labs(x = "Date", 
       y = "Log of death Cases",
       title = "Log of Worldwide Deaths Over Time") +
  scale_y_continuous(name="Deaths", labels = scales::comma) +
  theme_gray()

ggplotly(g_death1)
#ggsave("wwd4log.png")

#-------------------------------------------------------------------------

covid.by.date <- coronaData[,.(Confirmed=sum(Confirmed),Recovered=sum(Recovered),Deaths=sum(Deaths)),.(ObservationDate)]

#Percent of NAs
print("Percent of NAs per Column")
paste0(colnames(coronaData),": ",round(colMeans(sapply(coronaData,is.na)),3)*100,"%") %>% print()

#FINDING TOP-5 COUNTRIES

covid.by.count1 <- coronaData[,.(Confirmed=sum(Confirmed),Recovered=sum(Recovered),Deaths=sum(Deaths)),.(ObservationDate,Country.Region)]
covid.by.count1[,Daily.Confirmed:=Confirmed-lag(Confirmed,1),.(Country.Region)]
covid.by.count1[,Daily.Recovered:=Recovered-lag(Recovered,1),.(Country.Region)]
covid.by.count1[,Daily.Deaths:=Deaths-lag(Deaths,1),.(Country.Region)]

covid.by.count <- covid.by.count1 %>%
  select(ObservationDate, Country.Region, Daily.Confirmed, Daily.Deaths, Daily.Recovered)  %>% 
  group_by(Country.Region) %>%
  summarize(Confirmed_Cases = sum(Daily.Confirmed,na.rm=TRUE), Death_cases = sum(Daily.Deaths,na.rm=TRUE), 
            Recovered_cases = sum(Daily.Recovered,na.rm=TRUE)) %>%
  arrange(desc(Confirmed_Cases), desc(Death_cases), desc(Recovered_cases))

coronaData_count5 <- covid.by.count[c(1:5),]

#PLOTTING TOP-5 COUNTRIES

coul <- brewer.pal(5, "Set1") 

barplot(height=coronaData_count5$Confirmed_Cases, names=coronaData_count5$Country.Region, col=coul, xlab = "countries", ylab = "covid-19 cases", main = "Top 5 countries having higher covid-19 confirmed cases")
ggsave('top5_countries(5).png')
#EXPLORING FOR TOP-5 COUNTRIES

country1 <- ggplot() + 
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[1])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Confirmed,fill="Confirmed"),alpha=0.2) +
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[1])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Recovered,fill="Recovered"),alpha=1.5) +
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[1])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Deaths,fill="Deaths"),alpha=0.7) +
  scale_fill_manual(values=c("Recovered" = "Blue","Confirmed" = "red","Deaths" = "Yellow"),
                    name="Level Type") +
  labs(title=coronaData_count5$Country.Region[1], 
       subtitle = "Cases / Recoveries / Deaths") +
  xlab("Date") + 
  ylab("Count of Cases / Deaths")

print(country1)
#ggsave('country1(6).png')

#----------------------------------------------------------------------------------------

country2 <- ggplot() + 
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[2])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Confirmed,fill="Confirmed"),alpha=0.4) +
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[2])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Recovered,fill="Recovered"),alpha=0.4) +
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[2])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Deaths,fill="Deaths"),alpha=0.4) +
  scale_fill_manual(values=c("Recovered" = "Yellow","Confirmed" = "red","Deaths" = "Green"),
                    name="Level Type") +
  labs(title=coronaData_count5$Country.Region[2], 
       subtitle = "Cases / Recoveries / Deaths") +
  xlab("Date") + 
  ylab("Count of Cases / Deaths")

print(country2)
#ggsave('country2(7).png')

country3 <- ggplot() + 
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[3])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Confirmed,fill="Confirmed"),alpha=0.4) +
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[3])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Recovered,fill="Recovered"),alpha=0.8) +
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[3])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Deaths,fill="Deaths"),alpha=0.4) +
  scale_fill_manual(values=c("Recovered" = "Cyan","Confirmed" = "red","Deaths" = "Green"),
                    name="Level Type") +
  labs(title=coronaData_count5$Country.Region[3], 
       subtitle = "Cases / Recoveries / Deaths") +
  xlab("Date") + 
  ylab("Count of Cases / Deaths")

print(country3)
#ggsave('country3(8).png')

country4 <- ggplot() + 
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[4])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Confirmed,fill="Confirmed"),alpha=0.4) +
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[4])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Recovered,fill="Recovered"),alpha=0.4) +
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[4])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Deaths,fill="Deaths"),alpha=0.4) +
  scale_fill_manual(values=c("Recovered" = "Magenta","Confirmed" = "Cyan","Deaths" = "Green"),
                    name="Level Type") +
  labs(title=coronaData_count5$Country.Region[4], 
       subtitle = "Cases / Recoveries / Deaths") +
  xlab("Date") + 
  ylab("Count of Cases / Deaths")

print(country4)
#ggsave('country4(9).png')

country5 <- ggplot() + 
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[5])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Confirmed,fill="Confirmed"),alpha=0.4) +
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[5])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Recovered,fill="Recovered"),alpha=0.4) +
  geom_area(data=(covid.by.count1 %>% 
                    filter(`Country.Region` %in% coronaData_count5$Country.Region[5])),
            #group_by(date, count)), 
            aes(x=ObservationDate,y=Deaths,fill="Deaths"),alpha=1.9) +
  scale_fill_manual(values=c("Recovered" = "Cyan","Confirmed" = "Yellow","Deaths" = "Red"),
                    name="Level Type") +
  labs(title=coronaData_count5$Country.Region[5], 
       subtitle = "Cases / Recoveries / Deaths") +
  xlab("Date") + 
  ylab("Count of Cases / Deaths")

print(country5)
#ggsave('country5(10).png')

#SIR MODELLING FOR USA
#MODELLING FOR USA 

#summary(covid.by.count1)

#df$ObservationDate = as.Date(df$ObservationDate, format = "%m/%d/%y")
timeline = covid.by.count1 %>% 
  filter(`Country.Region` %in% 'US') %>% 
  group_by(ObservationDate) %>% summarize(infecteds=sum(Confirmed), recovereds=sum(Recovered),deaths_reported=sum(Deaths))

summary(timeline)

#INFECTED VS RECOVERED 

plot(timeline$ObservationDate, timeline$infecteds, type="l", xlab = "Timeline", ylab = "Cumulative count", col = "red")
lines(timeline$ObservationDate, timeline$recovereds, col = "green")
title(main = "Timeline of cumulative count of infecteds (red) and recovereds (green)")
options(scipen=999)
#ggsave("ivsr11.png")



#-----------------------SIR MODEL-----------------------------

#In the below graphic, the two separate growth in number of reported cases (indicating spread) can be seen more clearly.

# Calculate the rate of infection

dI = diff(timeline$infecteds)
dt = as.numeric(diff(timeline$ObservationDate))


dIdt = dI / dt


# Calculate the rate of recovery
dRdt = diff(timeline$recovereds) / dt

# Visualize the new increased cases

plot(timeline$ObservationDate[-1], dIdt, type="l", xlab = "Timeline", ylab = "Reported", col = "red")
lines(timeline$ObservationDate[-1], dRdt, col="green")
title(main = "Timeline of reported new cases (red) and recoveries (green)")

#calculating wave before 4days of the system date 
wave.2 = timeline[(timeline$ObservationDate >= "2020-02-15") & (timeline$ObservationDate <= Sys.Date()-4),]


N = 3000000 
wave.2.I = wave.2$infecteds - wave.2$recovereds 
wave.2.R = wave.2$recovereds

wave.2.S = N - wave.2.I - wave.2.R

#dS/dt = - b SI/N 
#FIRST EQUATION 
#Using the first equation, we fit a linear regression to the growth data. Under the null hypothesis, there is no (linear) correlation.
x = wave.2.I * wave.2.S / N
x = x[-1]
y = diff(wave.2.S) / as.numeric(diff(wave.2$ObservationDate))
data_frame <- cbind.data.frame(x,y)
b.reg = lm(y ~ x + 0,data=data_frame)

# Remove unnecessary variables to keep space clean
#rm(x)
#rm(y)

# Show the regression summary and save the coefficient
print(summary(b.reg))
b = -as.numeric(coef(b.reg))
print("RMSE VALUE AND MAE VALUES:")
print(rmse(b.reg,data_frame))
print(mae(b.reg,data_frame))


#FOR THE SSECOND EQUATION 
# dR/dt = c*I
y1 = diff(wave.2.R) / as.numeric(diff(wave.2$ObservationDate))
x1 = wave.2.I[-1]
data_frame1 <- cbind.data.frame(x1,y1)
c.reg = lm(y1 ~ x1+ 0)


# Clean the variables 
#rm(x)
#rm(y)

print(summary(c.reg))
c = as.numeric(coef(c.reg))
print("RMSE VALUE and MAE VALUES :")
print(rmse(c.reg,data_frame1))
print(mae(c.reg,data_frame1))

#dI/dt = b SI/N - cI - This is our third equation and to find the infected population
n.sim = 150 # Number of days to prognose
position = length(wave.2$ObservationDate)
#print(position)
S = c(wave.2.S, rep(0, n.sim))
I = c(wave.2.I, rep(0, n.sim))
R = c(wave.2.R, rep(0, n.sim))
new.len = length(S)

dates = c(wave.2$ObservationDate, rep(0, n.sim))
for(m in position+1:n.sim) {
  S.change = -b*S[m-1]*I[m-1] / N
  S[m] = max(min(S[m-1] + S.change, N), 0)
  
  R.change = c*I[m-1]
  R[m] = min(R[m-1] + R.change, N)
  
  I.change = b*S[m-1]*I[m-1] / N - c*I[m-1]
  I[m] = max(I[m-1] + I.change, 0)
  
  dates[m] = dates[m-1] + 1
}

results.sir.model = data.frame(
  Dates = dates,
  Susceptibles = S,
  Infecteds = I,
  Recovereds = R
)

sir.melt = melt(results.sir.model, id='Dates') 
sir.plot = ggplot(data=sir.melt, aes(x=Dates, y=value, color=variable)) + geom_point() + labs(title="SIR Model of Covid-19 Pandemic", x="Timeline", y="Counts")
sir.plot = sir.plot + scale_color_manual(name = "", values = c('blue', 'red', 'green'))
sir.plot

#RESULTS
results.sir.model[results.sir.model$Infecteds>=max(results.sir.model$Infecteds),]

#PREDICTING FOR THE NEXT 5 DAYS 
view.df = results.sir.model[(results.sir.model$Dates>=Sys.Date()-5) & (results.sir.model$Dates<=Sys.Date()+5),]

print(view.df)

#PREDICTED VS ACTUAL 
actual.df = timeline[(timeline$ObservationDate >= "2020-02-15") & (timeline$ObservationDate <=Sys.Date()-5),]

colnames(view.df) = c("Dates", "Predicted.Susceptibles", "Predicted.Infecteds", "Predicted.Recovereds")
colnames(actual.df) = c("Dates", "Actual.Infecteds", "Actual.Recovereds","Deaths")
actual.df$Deaths<-NULL

# The Actual.Infecteds column is actually the total cases, so subtraction of recovereds is needed
actual.df$Actual.Infecteds = actual.df$Actual.Infecteds - actual.df$Actual.Recovereds

# Drop the susceptibles prediction as this is the artificial variable, for which there is no comparison
view.df = view.df[c("Dates", "Predicted.Infecteds", "Predicted.Recovereds")]
view.df = view.df[view.df$Dates > Sys.Date()-8,]

merged = merge(view.df, actual.df)
print(merged)

#--------------------EDA FOR FIRST OUTCOME ---------------------------

setorder(covid.by.count1,ObservationDate,Country.Region)

#add column of daily cases (but we add same NA values in the case of the first submission)
covid.by.count1[,Daily.Confirmed:=Confirmed-lag(Confirmed,1),.(Country.Region)]
covid.by.count1[,Daily.Recovered:=Recovered-lag(Recovered,1),.(Country.Region)]
covid.by.count1[,Daily.Deaths:=Deaths-lag(Deaths,1),.(Country.Region)]

##Make NA values zero (this make sense if you want to visualize daily cases as barplots and stuff)
covid.by.count1[is.na(covid.by.count1)] <- 0
##From first Confirmed to the first outcome. We merge the lines of first Confirmed,Recovered and Death for each Country.We also add the dates in order to find later the difference in time. Na values are placed for missing values.
date.of.case.status.by.country <- 
  merge(
    merge(covid.by.count1[Daily.Confirmed>0][order(Country.Region,ObservationDate)][,head(.SD,1),.(Country.Region)][,.(Country.Region,ConfirmedDate=ObservationDate,Confirmed)],
          covid.by.count1[Daily.Recovered>0][order(Country.Region,ObservationDate)][,head(.SD,1),.(Country.Region)][,.(Country.Region,RecoveredDate=ObservationDate,Recovered)],
          by='Country.Region',all=T),
    covid.by.count1[Daily.Deaths>0][order(Country.Region,ObservationDate)][,head(.SD,1),.(Country.Region)][,.(Country.Region,DeathDate=ObservationDate,Deaths)],
    by='Country.Region',all=T)

# Determining the first outcome (either recovery or death)
date.of.case.status.by.country[!is.na(RecoveredDate) & !is.na(DeathDate),
                               First.Outcome.Date:=as.Date(ifelse(RecoveredDate >= DeathDate,DeathDate,RecoveredDate),origin = origin)]

#Add new columns
date.of.case.status.by.country[,Recovered.Days:=as.integer(difftime(RecoveredDate,ConfirmedDate,units = 'days'))]
date.of.case.status.by.country[,Death.Days:=as.integer(difftime(DeathDate,ConfirmedDate,units = 'days'))]
date.of.case.status.by.country[,Outcome.Days:=as.integer(difftime(First.Outcome.Date,ConfirmedDate,units = 'days'))]
date.of.case.status.by.country[,Outcome.Type:=ifelse(RecoveredDate==First.Outcome.Date,'Recovered',ifelse(DeathDate==First.Outcome.Date,'Death','Others'))]

#computation in difference in time between first confirmation and outcome
#head(date.of.case.status.by.country)
date.of.case.status.by.country[,Recovered.Days:=as.integer(difftime(RecoveredDate,ConfirmedDate,units = 'days'))]
date.of.case.status.by.country[,Death.Days:=as.integer(difftime(DeathDate,ConfirmedDate,units = 'days'))]
date.of.case.status.by.country[,Outcome.Days:=as.integer(difftime(First.Outcome.Date,ConfirmedDate,units = 'days'))]
date.of.case.status.by.country[,Outcome.Type:=ifelse(RecoveredDate==First.Outcome.Date,'Recovered',ifelse(DeathDate==First.Outcome.Date,'Death','Others'))]

#--------------------------FOR DEATH CASES----------------------------

date.of.case.status.by.country = date.of.case.status.by.country[(date.of.case.status.by.country$ConfirmedDate >= "2020-02-20") & (date.of.case.status.by.country$ConfirmedDate <= "2020-04-01"),]
date.of.case.status.by.country.death = date.of.case.status.by.country[(date.of.case.status.by.country$Outcome.Type=="Death")]
date.of.case.status.by.country.death = date.of.case.status.by.country.death[(date.of.case.status.by.country.death$Country.Region!="Costa Rica") & (date.of.case.status.by.country.death$Country.Region!="Luxembourg") & (date.of.case.status.by.country.death$Country.Region!="Dominican Republic") & (date.of.case.status.by.country.death$Country.Region!="Kazakhstan") & (date.of.case.status.by.country.death$Country.Region!="Cabo Verde")]
#enlarge plot options(repr.plot.width=15, repr.plot.height=8)
options(repr.plot.width=40, repr.plot.height=15)
#Visualization among countries in the the first incindence and outcome in days.

ggplot(date.of.case.status.by.country.death[Outcome.Days>0])+
  geom_point(aes(y=Outcome.Days,x=ConfirmedDate),col="red")+
  geom_text(aes(y=Outcome.Days,x=ConfirmedDate,label=Country.Region,hjust='left'))+
  #scale_color_brewer(palette = 'Set2',name='outcome')+
  scale_x_date(date_labels = '%m/%d',limits = as.Date(c("2020-02-24","2020-03-25")))+
  labs(title='Days Till the First Outcome Was Death',
       y="Days After First Confirmed Case", x = "First Confirmed Case") +
  theme(plot.title = element_text(hjust = 0.5))

#--------------------FOR RECOVERY CASES-------------------------------

date.of.case.status.by.country = date.of.case.status.by.country[(date.of.case.status.by.country$ConfirmedDate >= "2020-02-20") & (date.of.case.status.by.country$ConfirmedDate <= "2020-03-25"),]
date.of.case.status.by.country.rec = date.of.case.status.by.country[(date.of.case.status.by.country$Outcome.Type=="Recovered")]
date.of.case.status.by.country.rec = date.of.case.status.by.country.rec[(date.of.case.status.by.country.rec$Country.Region!="Sweden") & (date.of.case.status.by.country.rec$Country.Region!="Jamaica") & (date.of.case.status.by.country.rec$Country.Region!="South Africa") & (date.of.case.status.by.country.rec$Country.Region!="Romania") & (date.of.case.status.by.country.rec$Country.Region!="Kyrgyzstan") & (date.of.case.status.by.country.rec$Country.Region!="North Macedonia") & (date.of.case.status.by.country.rec$Country.Region!="Senegal") & (date.of.case.status.by.country.rec$Country.Region!="Belarus") & (date.of.case.status.by.country.rec$Country.Region!="Liberia")]
#enlarge plot options(repr.plot.width=15, repr.plot.height=8)
options(repr.plot.width=40, repr.plot.height=15)
#Visualization among countries in the the first incindence and outcome in days.

ggplot(date.of.case.status.by.country.rec[Outcome.Days>0])+
  geom_point(aes(y=Outcome.Days,x=ConfirmedDate),col="Green")+
  geom_text(aes(y=Outcome.Days,x=ConfirmedDate,label=Country.Region,hjust='left'))+
  #scale_color_brewer(palette = 'Set2',name='outcome')+
  scale_x_date(date_labels = '%m/%d',limits = as.Date(c("2020-02-24","2020-04-01")))+
  labs(title='Days Till the First Outcome Was Recovery',
       y="Days After First Confirmed Case", x = "First Confirmed Case") +
  theme(plot.title = element_text(hjust = 0.5))


#europe <- covid.by.country %>% filter(Country.Region %in% european_countries)

#--------------------USA ANALYSIS--------------------------

us.death <- read_csv("C://Users/spati/OneDrive/Desktop/covid_19/time_series_covid_19_deaths_US.csv")
us.data <- read_csv("C://Users/spati/OneDrive/Desktop/covid_19/time_series_covid_19_confirmed_US.csv")
us.death <- us.death[,c("Province_State","4/7/20")]
us.data <- us.data[,c("Province_State","4/7/20")]
colnames(us.death)[colnames(us.death)=="4/7/20"] <- "count"
colnames(us.data)[colnames(us.data)=="4/7/20"] <- "count"


death.state <- us.death %>%
  group_by(`Province_State`) %>%
  summarise_all(list(sum)) %>% 
  rename(NAME=`Province_State`,
         deaths=count)

count.state <- us.data %>%
  group_by(`Province_State`) %>%
  summarise_all(list(sum)) %>% 
  rename(NAME=`Province_State`,
         total=count)

# Adjust Rate per 100,000
# To adjust confirmed cases  relative to population: 
# load us map for 2015 Census population data
#Take the statepop from the us mappackage: 
state.pop <- statepop
colnames(state.pop)[colnames(state.pop)=="full"] <- "NAME"
# left join the data
count.state <- count.state %>% left_join(state.pop, by=c("NAME"))
# Calculating infections per 100,000 residents rounded to 2 digits
count.state$Confirmed_Per_Capita <- round(count.state$total/count.state$pop_2015*100000,2)

#Creating various subsets to join with the state data to create a single data table with percapita cases and deaths
death.tbl <- death.state
state.tbl <- death.tbl %>% left_join(count.state, by=c("NAME"))
state.tbl$Death_Per_Capita <- round(state.tbl$deaths/state.tbl$pop_2015*100000,2)

# Find the top 10 states by number of cases
state.tbl <- state.tbl %>% arrange(desc(Confirmed_Per_Capita)) 
state.tbl.top <- top_n(ungroup(state.tbl), 10, Confirmed_Per_Capita)

#RENAMING THE COLUMNS 
colnames(state.tbl.top)[colnames(state.tbl.top)=="Confirmed_Per_Capita"] <- "Cases Per Cap"
colnames(state.tbl.top)[colnames(state.tbl.top)=="Death_Per_Capita"] <- "Deaths per Cap"
colnames(state.tbl.top)[colnames(state.tbl.top)=="NAME"] <- "States"
colnames(state.tbl.top)[colnames(state.tbl.top)=="deaths"] <- "Deaths"
colnames(state.tbl.top)[colnames(state.tbl.top)=="total"] <- "Cases"

state.tbl.top <- subset(state.tbl.top, select = c(States, Cases, `Cases Per Cap`, Deaths, `Deaths per Cap`))

state.tbl.top$Mortality <- percent((state.tbl.top$Deaths / state.tbl.top$Cases)) 

state.tbl.top

#DOWNLOADED PNG 
#Plotting the trend of the top 10 states by cases per capita, number of days since their initial 100 confirmed cases. This gives us a sense of the effectiveness of a state response in containing the virus. Here we see New York greatly accelerating their number of cases, will current efforts be enough to slow this trend?

#Which Variables are the most Important (randomForestSRC_importance)

#-----------------------MODEL(RANDOM FOREST)-------------------------


covll <- read_csv("C://Users/spati/OneDrive/Desktop/covid_19/COVID19_line_list_data.csv",col_names = T)
apply(covll,2,function(x) sum(is.na(x)))
covll <- select(covll,-c(X4,link,source,X22,X23,X24,X25,X26,X27))
covll$`reporting date` <- mdy(covll$`reporting date`)
covll$country <- as.factor(covll$country)
covll$gender <- as.factor(covll$gender)
covll$symptom_onset <- mdy(covll$symptom_onset)
covll$hosp_visit_date <- mdy(covll$hosp_visit_date)
covll$exposure_start <- mdy(covll$exposure_start)
covll$exposure_end <- mdy(covll$exposure_end)
covll$death <- mdy(covll$death)
#probably the wrong data to recovered dates ment 2019
covll$recovered <- str_replace_all(covll$recovered,pattern = "1899",replacement = "2019")
covll$recovered <- mdy(covll$recovered)


# 1. Add new variables

#create family variable
family_friends <- str_detect(string = covll$summary,pattern = ("husbend | family | daughter | children
                                             |father | wife | son | married | aunt |
                                             mother| sister | cousin | grandmother |
                                             grandfather | relatives | friend | friends | boyfriend | girlfriend | party"))
data1 <- covll %>% 
  mutate(family_friends=as.numeric(family_friends))


# Create Travel variable
travel <- str_detect(data1$summary, ("travel | traveled | cruise | Cruise | trip | journey | Journey | airport | train | plane | airplane |
                                     traveling | flight | Flight | rail | Tour | tour | ship | Ship | arrived | return | returned | business trip"))
data1 <- data1 %>% 
  mutate(travel=as.numeric(travel))
#table(data1$travel)

# Create airplane variable
airplain <- str_detect(data1$summary, (" airport | Airport| plane |Plane| airplane |Airplane| flight | Flight"))
data1 <- data1 %>% 
  mutate(airplain=as.numeric(airplain))
#table(data1$airplain)

# Create train variable
train <- str_detect(data1$summary,"train | Train | rail | Rail |trainstation|Trainstation")
data1 <- data1 %>% 
  mutate(train=as.numeric(airplain))
#table(data1$airplain)

# Create work variable
work <- str_detect(data1$summary, ("employee | Employee | Business | business | coworker | meeting"))
data1 <- data1 %>% 
  mutate(work=as.numeric(work))
#table(data1$work)

# Create byContact variable
byContact <- str_detect(data1$summary, ("contact | store | mall | shop"))
data1 <- data1 %>% 
  mutate(byContact=as.numeric(byContact))
#table(data1$byContact)

glimpse(data1)


##-------------------------------------Symptoms -------------------------------------

# cough
pattern <- ("cough")
cough <- ifelse(is.na(data1$symptom)==T,
                str_detect(data1$summary, pattern),
                str_detect(string = str_c(data1$symptom, data1$summary, sep = " "), pattern))

data1 <- mutate(data1,cough=as.numeric(cough))
#table(data1$cough)

# sputum
pattern <- ("sputum")
sputum <- ifelse(is.na(data1$symptom)==T,
                 str_detect(data1$summary, pattern),
                 str_detect(string = str_c(data1$symptom, data1$summary, sep = " "), pattern))

data1 <- mutate(data1,sputum=as.numeric(sputum))
#table(data1$sputum)

# fever
pattern <- ("fever | Fever")
fever <- ifelse(is.na(data1$symptom)==T,
                str_detect(data1$summary, pattern),
                str_detect(string = str_c(data1$symptom, data1$summary, sep = " "), pattern))

data1 <- mutate(data1,fever=as.numeric(fever))
#table(data1$fever)

# pneumonia
pattern = ("pneumonia")
pneumonia <- ifelse(is.na(data1$symptom)==T,
                    str_detect(data1$summary, pattern),
                    str_detect(string = str_c(data1$symptom, data1$summary, sep = " "), pattern))

data1 <- mutate(data1, pneumonia=as.numeric(pneumonia))
#table(data1$pneumonia)
#x <- data1 %>% filter(pneumonia==1) %>% select(summary)
#x$summary

# throat
pattern = ("throat")
throat <- ifelse(is.na(data1$symptom)==T,
                 str_detect(data1$summary, pattern),
                 str_detect(string = str_c(data1$symptom, data1$summary, sep = " "), pattern))

data1 <- mutate(data1,throat=as.numeric(throat))
#table(data1$throat)
#x <- data1 %>% filter(throat==1) %>% select(summary)
#x$summary


# respiratory
pattern = ("shortness | breath | chest | dyspnea | `shortness of breath`| malaise")
respiratory <- ifelse(is.na(data1$symptom)==T,
                      str_detect(data1$summary, pattern),
                      str_detect(string = str_c(data1$symptom, data1$summary, sep = " "), pattern))

data1 <- mutate(data1,respiratory=as.numeric(respiratory))

# diarrhea
pattern = ("diarrhea | abdominal")
diarrhea <- ifelse(is.na(data1$symptom)==T,
                   str_detect(data1$summary, pattern),
                   str_detect(string = str_c(data1$symptom, data1$summary, sep = " "), pattern))

data1 <- mutate(data1,diarrhea=as.numeric(diarrhea))
#table(data1$diarrhea)
#x <- data1 %>% filter(diarrhea==1) %>% select(summary)
#x$summary

# vomiting
pattern = ("vomiting")
vomiting <- ifelse(is.na(data1$symptom)==T,
                   str_detect(data1$summary, pattern),
                   str_detect(string = str_c(data1$symptom, data1$summary, sep = " "), pattern))

data1 <- mutate(data1,vomiting=as.numeric(vomiting))

# myalgia
pattern = ("myalgia | body | tired | `sore body` | muscle | muscles | joint | cramp | cramps ")
muscles_pain <- ifelse(is.na(data1$symptom)==T,
                       str_detect(data1$summary, pattern),
                       str_detect(string = str_c(data1$symptom, data1$summary, sep = " "), pattern))

data1 <- mutate(data1,muscles_pain=as.numeric(muscles_pain))
#table(data1$muscles_pain)
#x <- data1 %>% filter(muscles_pain==1) %>% select(summary)

# chill
pattern = ("chill | chills | cold ")
chill <- ifelse(is.na(data1$symptom)==T,
                str_detect(data1$summary, pattern),
                str_detect(string = str_c(data1$symptom, data1$summary, sep = " "), pattern))

data1 <- mutate(data1,chill=as.numeric(chill))

# headache
pattern = ("headache| head ")
headache <- ifelse(is.na(data1$symptom)==T,
                   str_detect(data1$summary, pattern),
                   str_detect(string = str_c(data1$symptom, data1$summary, sep = " "), pattern))

data1 <- mutate(data1,headache=as.numeric(headache))

# Dates------------------------------------------------------------------
# exposure start - end
data1 <- mutate(data1, daysExpStr_End=as.numeric(data1$exposure_end-data1$exposure_start))
#data1 %>% select(daysExpStr_End)
# exposure start  - symptoms onset
data1 <- mutate(data1, daysExpStr_SmptOnst=as.numeric(data1$symptom_onset-data1$exposure_start))
# exposure start - hospital visit
data1 <- mutate(data1, daysExpStr_HospVisit=as.numeric(data1$hosp_visit_date-data1$exposure_start))
# exposure start - death
data1 <- mutate(data1,daysExpStr_death = as.numeric(data1$death-data1$exposure_start))
#exposure start - recover
data1 <- mutate(data1, daysExpStr_recover = as.numeric(data1$recovered - data1$exposure_start))
#symptom onset - exposure end
data1 <- mutate(data1, daysSmptOnst_ExpEnd=as.numeric(symptom_onset-exposure_end))
#symptom onset - hospital visit
data1 <- mutate(data1, daysSmptOnst_HospVisit=as.numeric(hosp_visit_date-symptom_onset))
#symptom onset - death
data1 <- mutate(data1, daysSmptOnst_death=as.numeric(death-symptom_onset))
#symptom onset - recovered
data1 <- mutate(data1, daysSmptOnst_recovered=as.numeric(recovered-symptom_onset))
# hospitalVisit - death
data1 <- mutate(data1, dayshsplzed_death=as.numeric(death-hosp_visit_date))
# hospitalVisit - recovered
data1 <- mutate(data1, dayshsplzed_recovered=as.numeric(recovered-hosp_visit_date))


data1 <- mutate(data1,progress=ifelse(is.na(data1$death)==F,yes="death",
                                      no=ifelse(is.na(data1$recovered)==F,
                                                yes = "recovered",no="on_progress")))

data1$progress <- as_factor(data1$progress)
table(data1$progress)

#install.packages("randomForestSRC")
library(randomForestSRC)
library(mlr)
#glimpse(data1)

# lets see how many NA are on each Variable
#apply(data1,2,function(x) sum(is.na(x)))
# i will not use the day variables because there are many NAs and the model will be weak if i use them too.
dataTrain <- data1 %>% select(location, country, gender, age, If_onset_approximated, `visiting Wuhan` , `from Wuhan`,
                              family_friends,travel, airplain, train, work, byContact, cough, sputum, fever, pneumonia, throat, respiratory,
                              diarrhea, vomiting, muscles_pain, chill, headache,progress) %>% rename(visiting_Wuhan=`visiting Wuhan`,from_Wuhan=`from Wuhan`)

#apply(dataTrain,2,function(x) sum(is.na(x)))
# impute numeric variables with "regr.Rpart" method
imputeMethod <- imputeLearner("regr.rpart")
dataTrainImpute <- impute(as.data.frame(dataTrain[,-c(1,2,3)]), classes = list(numeric=imputeMethod),target = "progress")
dataTrainImpute1 <- dataTrainImpute$data
train <- cbind(dataTrain[,c(1,2,3)],dataTrainImpute1)
#apply(train,2,function(x) sum(is.na(x)))

# impute gender values
dataTrainImpute2 <- impute(train, cols = list(gender=imputeConstant("male")),target = "progress")
train <- dataTrainImpute2$data
#apply(train,2,function(x) sum(is.na(x)))

train$location <- str_replace_all(train$location,pattern = (" "),replacement = "_")
train$location <- str_replace_all(train$location, "-",replacement = "_")
train$location <- str_replace_all(train$location, ("\\(|\\)|\\,"),replacement = "_" )
train$location <- as_factor(train$location)
train <- normalizeFeatures(train,target = "progress",method = "standardize")
task <- makeClassifTask(data = train, target = "progress")
options(scipen=999)

# choosing best variables with filter method
filterVals <- generateFilterValuesData(task = task,method = "randomForestSRC_importance")      

options(repr.plot.width =25, repr.plot.height = 15)
plotFilterValues(filterVals)+
  theme_bw()+
  labs(title = "Variable Importance",
       y="measure",
       x="variables")+
  coord_flip()+
  theme(axis.text.x = element_text(colour="grey20",size=20,angle=60,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=25,angle=0,hjust=.5,vjust=1,face="plain"),
        axis.title.x = element_text(colour="grey20",size=22,angle=0,hjust=.5,vjust=1,face="plain"),
        axis.title.y = element_text(colour="grey20",size=22,angle=90,hjust=.5,vjust=.5,face="plain"),
        legend.text=element_text(size = 18),
        legend.key = element_rect(size = 18),
        legend.title = element_text(size = 19),
        strip.text= element_text(size=17),
        plot.title = element_text(size = rel(4)))


#----------------------------STARTING EDA'S--------------------------
#install.packages("ggrepel")
'''
library(ggrepel)

covid_start <- covid.by.count1 %>%
  filter(ObservationDate>='2020-02-15')

covid_start <- covid_start %>% 
  filter(Country.Region %in% (covid_start %>% 
                         filter(ObservationDate==max(ObservationDate)) %>% group_by(Country.Region) %>% summarize(cases=sum(Confirmed)) %>% 
                         top_n(10, cases))) 
%>%
  group_by(ObservationDate, Country.Region) %>%
  summarize(Confirmed = sum(Confirmed))  %>%
  ungroup() %>%
  filter(Confirmed>100) %>%
  droplevels() %>%
  group_by(Country.Region) %>%
  # summarize(cases = n()) %>%
  mutate(`days since >100 cases` = row_number(ObservationDate)) %>%
  mutate(label = if_else(`days since >100 cases` == max(`days since >100 cases`), as.character(paste0(Country.Region," ", round(Confirmed/1000,0), "k")), NA_character_)) %>%
  ggplot(aes(x = `days since >100 cases`, y = Confirmed, colour = Country.Region)) +
  geom_line(size = 2, alpha = 0.8) +
  labs(y = "", 
       x = paste0("n° of days since 100th case (as of ", max(covid_start$ObservationDate), ")"),
       title =  "Largest COVID-19 Outbreaks", 
       subtitle = paste0("n° of confirmed cases in most affected countries outside China+SK"))+
  scale_y_continuous(label=scales::comma, position = "right")+
  #scale_colour_brewer(palette = "Set2")+
  geom_label_repel(size = 7, aes(label = label),na.rm = TRUE, segment.size=0.25, nudge_x=4, direction="y") +
  theme_fivethirtyeight(base_size = 22) + 
  theme(axis.title = element_text(),
        legend.position ="none")
'''
#--------------------------------EDA STARTING -----------------------------------------------------

#MORE EXPLORATION ON COUNTRIES

coronaData_count10 <- covid.by.count[c(1:10),]
coronaData_count11 <- covid.by.count[18,]
total <- rbind(coronaData_count10, coronaData_count11)

plot1 <- covid.by.count1 %>%
  filter(Country.Region %in% most_sick) %>%
  group_by(ObservationDate, Country.Region) %>%
  summarize(Daily_Cases = sum(Confirmed)) %>%
  ggplot(aes(x = ObservationDate, y = Daily_Cases,col = Country.Region))+
  geom_line() +
  labs(title = "Daily Cases per Country", x = "Day", y = "Cases")+
  theme_minimal() 
options(scipen=999)
plot1
ggsave('dcpercount12.png')


plot2 <- covid.by.count1  %>%
  filter(Country.Region %in% most_sick, ObservationDate>"2020-02-20") %>%
  group_by(ObservationDate, Country.Region) %>%
  summarize(Daily_Cases = sum(Confirmed)) %>%
  ggplot(aes(x = ObservationDate, y = Daily_Cases,col = Country.Region))+
  geom_line() +
  labs(title = "Daily Cases per Country (After 20 Feb)", x = "Day", y = "Cases")+
  theme_minimal()
options(scipen=999)
plot2
ggsave('dcpercount13.png')
#gridExtra::grid.arrange(plot1,plot2)

#---------------------DAILY NEW CASES-----------------------


covid.by.count1 %>%
  filter(ObservationDate>='2020-02-20') %>%
  filter(Country.Region %in% (covid.by.count1 %>% 
                         filter(ObservationDate==max(ObservationDate)) %>% group_by(Country.Region) %>% summarize(cases=sum(Confirmed)) %>% 
                         top_n(10, cases))$Country.Region) %>%   group_by(Country.Region, ObservationDate) %>%
  group_by(Country.Region, ObservationDate) %>%
  summarize(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered)) %>%
  mutate(NewCases = Confirmed - lag(Confirmed, order_by = ObservationDate)) %>%
  ggplot(aes(x = ObservationDate, y = NewCases, colour = Country.Region)) +
  geom_line(size=2, alpha=0.8) +
  labs(x = "Dates", y = "Confirmed Cases", title =  "Daily New COVID-19 cases", subtitle = "In the 10 most affected countries, since Feb 20")+
  scale_x_date(date_labels = "%b %d", date_breaks = "25 days")+
  scale_y_continuous(label=scales::comma, position = "right")+
  #scale_colour_brewer(palette = "Set2")+
  theme_fivethirtyeight(base_size = 15) + 
  facet_wrap(~Country.Region, scales = "free")+
  stat_smooth(formula = "y~x", method="loess", se = FALSE, size = 1,linetype ="dashed",alpha = 0.2)+
  theme(axis.title = element_text(size = 15),
        legend.position = "bottom") +
  guides(fill=guide_legend("Country"))
ggsave('dncpercount15.png')


#-----------------------DAILY DEATH CASES-------------------------------

covid.by.count1 %>%
  filter(ObservationDate>='2020-02-20') %>%
  filter(Country.Region %in% (covid.by.count1 %>% 
                         filter(ObservationDate==max(ObservationDate)) %>% group_by(Country.Region) %>% summarize(cases=sum(Deaths)) %>% 
                         top_n(10, cases))$Country.Region) %>%   group_by(Country.Region, ObservationDate) %>%
  group_by(Country.Region, ObservationDate) %>%
  summarize(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered)) %>%
  mutate(NewDeaths = Deaths - lag(Deaths, order_by = ObservationDate)) %>%
  ggplot(aes(x = ObservationDate, y = NewDeaths, colour = Country.Region)) +
  geom_line(size=2, alpha=0.8) +
  labs(x = "Dates", y = "No of Deaths", title =  "Daily New Deaths", subtitle = "In 10 most affected countries, since Feb 20")+
  scale_x_date(date_labels = "%b %d", date_breaks = "25 days")+
  scale_y_continuous(label=scales::comma, position = "right")+
  #scale_colour_brewer(palette = "Set2")+
  theme_fivethirtyeight(base_size = 15) + 
  facet_wrap(~Country.Region, scales = "free")+
  stat_smooth(formula = "y~x", method="loess", se = FALSE, size = 1,linetype ="dashed",alpha = 0.2)+
  theme(axis.title = element_text(),
        legend.position = "bottom") +
  guides(fill=guide_legend("Country"))

#-------------------------DAILY RECOVERED CASES-----------------------

covid.by.count1 %>%
  filter(ObservationDate>='2020-02-20') %>%
  filter(Country.Region %in% (covid.by.count1%>% 
                         filter(ObservationDate==max(ObservationDate)) %>% group_by(Country.Region) %>% summarize(cases=sum(Recovered)) %>% 
                         top_n(10, cases))$Country.Region) %>%   group_by(Country.Region, ObservationDate) %>%
  group_by(Country.Region, ObservationDate) %>%
  summarize(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered)) %>%
  mutate(NewRecovered = Recovered - lag(Recovered, order_by = ObservationDate)) %>%
  ggplot(aes(x = ObservationDate, y = NewRecovered, colour = Country.Region)) +
  geom_line(size=2, alpha=0.8) +
  labs(x = "Dates", y = "Recovered", title =  "Daily Recovered", subtitle = "In 10 most affected countries, since Feb 20")+
  scale_x_date(date_labels = "%b %d", date_breaks = "25 days")+
  scale_y_continuous(label=scales::comma, position = "right")+
  #scale_colour_brewer(palette = "Set2")+
  theme_fivethirtyeight(base_size = 15) + 
  facet_wrap(~Country.Region, scales = "free")+
  stat_smooth(formula = "y~x", method="loess", se = FALSE, size = 1,linetype ="dashed",alpha = 0.2)+
  theme(axis.title = element_text(),
        legend.position = "bottom")


#--------------------------------------------------------------------------------------------



















