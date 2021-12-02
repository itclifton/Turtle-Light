setwd("~/Desktop/Research/Side Projects/Turtle Light 2021/Kzoo Maps")
## Packages ----
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lme4)
st.err = function(x) {sd(x)/sqrt(length(x))}

## Data Management ----
Alvin<-read.csv("Alvin.csv")
Alvin$ID<-"Alvin"
Alvin$sex<-"Male"
April<-read.csv("April.csv")
April$ID<-"April"
April$sex<-"Female"
Betty<-read.csv("Betty.csv")
Betty$ID<-"Betty"
Betty$sex<-"Female"
Bugs<-read.csv("Bugs.csv")
Bugs$ID<-"Bugs"
Bugs$sex<-"Male"
Carmen<-read.csv("Carmen.csv")
Carmen$ID<-"Carmen"
Carmen$sex<-"Female"
Chip<-read.csv("Chip.csv")
Chip$ID<-"Chip"
Chip$sex<-"Male"
Dale<-read.csv("Dale.csv")
Dale$ID<-"Dale"
Dale$sex<-"Male"
Dot<-read.csv("Dot.csv")
Dot$ID<-"Dot"
Dot$sex<-"Female"
Ernie<-read.csv("Ernie.csv") # Ernie wasn't recovered- This is more Felix
Ernie$ID<-"Felix"
Ernie$sex<-"Male"
Felix<-read.csv("Felix.csv")
Felix$ID<-"Felix"
Felix$sex<-"Male"
Fiona<-read.csv("Fiona.csv")
Fiona$ID<-"Fiona"
Fiona$sex<-"Female"
Granny<-read.csv("Granny.csv")
Granny$ID<-"Granny"
Granny$sex<-"Female"
Grouch<-read.csv("Grouch.csv")
Grouch$ID<-"Grouch"
Grouch$sex<-"Male"
Hubert<-read.csv("Hubert.csv")
Hubert$ID<-"Hubert"
Hubert$sex<-"Male"
Marlo<-read.csv("Marlo.csv")
Marlo$ID<-"Marlo"
Marlo$sex<-"Female"
Satan<-read.csv("Satan.csv")
Satan$ID<-"Satan"
Satan$sex<-"Male"
Ziggy<-read.csv("Ziggy.csv")
Ziggy$ID<-"Ziggy"
Ziggy$sex<-"Male"

Turtles<-rbind(Alvin, April, Betty, Bugs,
               Carmen, Chip, Dale, Dot,
               Ernie, Felix, Fiona,
               Granny, Grouch, Hubert, Marlo,
               Satan, Ziggy)
Turtles$ID<-as.factor(Turtles$ID)
Turtles<-Turtles[,c(2,5,7,13,14)]

Turtles$datetime<-as.POSIXct(strptime(Turtles$Adjusted.Local.Time, format("%m/%d/%y %H:%M")))
Turtles$WetDry<-fct_recode(Turtles$Wet.Dry, "0"="wet", "1"="dry")
Turtles<-Turtles[,c(4,5,6,7,2)]
Turtles$WetDry<-as.numeric(as.character(Turtles$WetDry))
Turtles$Week<-as.numeric(strftime(Turtles$datetime,format="%W"))


## Preliminary Light Analysis ----
# Light Only - I'm not sure what duration means.
Turtles.lux<-Turtles[,c(1,2,3,5,6)]
Turtles.lux<-na.omit(Turtles)
# Remove nighttime based on mean date sunrise (6:06) and sunset (21:22)
Turtles.Day<-subset(Turtles.lux, format(datetime, "%H:%M:%S")<"21:22:00" & format(datetime, "%H:%M:%S")>"06:06:00")

# Individual daily summary statistics
Ind.Mean<-aggregate(Light~sex+Week+ID+format(datetime, "%m-%d"), mean, data=Turtles.Day)
Ind.Sum<-aggregate(Light~sex+Week+ID+format(datetime, "%m-%d"), sum, data=Turtles.Day)
Ind.Median<-aggregate(Light~sex+Week+ID+format(datetime, "%m-%d"), median, data=Turtles.Day)
Ind.Summary<-cbind(Ind.Mean, Ind.Sum$Light, Ind.Median$Light)
colnames(Ind.Summary)<-c("sex","Week","ID","Day","Mean","Sum","Median")
Ind.Summary$Day<-as.Date(Ind.Summary$Day, format="%m-%d")

# Exploratory plotting- Sexes
aggregate(Mean~sex+format(Day, "%m-%d"), length, data=Ind.Summary) # Number of individuals is weird at 6/15
aggregate(Mean~sex+format(Day, "%m"), mean, data=Ind.Summary)
TukeyHSD(aov(Mean~sex+format(Day, "%m"), data=Ind.Summary))

Monthly.Sex.Mean<-aggregate(Mean~sex+format(Day, "%m"), mean, data=Ind.Summary)
Monthly.Sex.se<-aggregate(Mean~sex+format(Day, "%m"), st.err, data=Ind.Summary)
Monthly.Sex<-cbind(Monthly.Sex.Mean, Monthly.Sex.se$Mean)
colnames(Monthly.Sex)<-c("sex","Month","Mean","SE")
Monthly.Sex<-subset(Monthly.Sex, Month!="04")
#Day.Sex.Mean$Month<-as.Date(Day.Sex.Mean$Month, format="%m")
ggplot(data=Monthly.Sex, aes(x=Month, y=Mean, group=sex))+
  geom_point(aes(colour=sex), size=3, position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(width=0.5))+
  ylab("Mean Daily Light")

Weekly.Sex.Mean<-aggregate(Mean~sex+Week, mean, data=Ind.Summary)
Weekly.Sex.se<-aggregate(Mean~sex+Week, st.err, data=Ind.Summary)
Weekly.Sex<-cbind(Weekly.Sex.Mean, Weekly.Sex.se$Mean)
colnames(Weekly.Sex)<-c("sex","Week","Mean","SE")
Weekly.Sex<-subset(Weekly.Sex, Week!="17")
#Day.Sex.Mean$Month<-as.Date(Day.Sex.Mean$Month, format="%m")
ggplot(data=Weekly.Sex, aes(x=Week, y=Mean, group=sex))+
  geom_point(aes(colour=sex), size=3, position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.5, position=position_dodge(width=0.5))+
  ylab("Mean Daily Light")
summary(lmer(Mean~sex+Week+(1|ID), data=Ind.Summary))

Daily.Sex.Mean<-aggregate(Mean~sex+format(Day, "%m-%d"), mean, data=Ind.Summary)
Daily.Sex.se<-aggregate(Mean~sex+format(Day, "%m-%d"), st.err, data=Ind.Summary)
Daily.Sex<-cbind(Daily.Sex.Mean, Daily.Sex.se$Mean)
colnames(Daily.Sex)<-c("sex","Day","Mean","SE")
Daily.Sex$Day<-as.Date(Daily.Sex$Day, format="%m-%d")
Daily.Sex<-subset(Daily.Sex, format(Day, "%Y-%m-%d")>"2021-05-03" & format(Day, "%Y-%m-%d")<"2021-08-20") # Removes days with fewer than 2 individuals
ggplot(data=Daily.Sex, aes(x=Day, y=Mean, group=sex))+
  geom_line(aes(colour=sex))+
  ylab("Mean Daily Light")

Day.Sex.Sum<-aggregate(Light~sex+format(datetime, "%m-%d"), sum, data=Turtles.Day)
colnames(Day.Sex.Sum)[2]<-"Day"
Day.Sex.Sum$Day<-as.Date(Day.Sex.Sum$Day, format="%m-%d")
ggplot(x=Day, y=Light, group=sex, data=Day.Sex.Sum)+
  geom_line(aes(x=Day, y=Light, color=sex))+
  ylab("Sum Daily Light")

Day.Sex.median<-aggregate(Light~sex+format(datetime, "%m-%d"), median, data=Turtles.Day)
colnames(Day.Sex.median)[2]<-"Day"
Day.Sex.median$Day<-as.Date(Day.Sex.median$Day, format="%m-%d")
ggplot(x=Day, y=Light, group=sex, data=Day.Sex.median)+
  geom_line(aes(x=Day, y=Light, color=sex))+
  ylab("Median Daily Light")


## Preliminary Wet/Dry Analysis ----



