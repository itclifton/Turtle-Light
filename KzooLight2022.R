setwd("~/Desktop/Research/Side Projects/Turtle Light 2021/Turtle-Light")
## Packages ----
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
st.err = function(x) {sd(x)/sqrt(length(x))}
path<-"/Users/ianclifton/Desktop/Research/Side Projects/Turtle Light 2021/Figures"

## Data Management ----
Alvin<-read.csv("AlvinDone.csv")
Alvin$ID<-"Alvin"
Alvin$sex<-"Male"
Alvin$datetime<-as.POSIXct(strptime(Alvin$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Alvin<-subset(Alvin, format(datetime, "%Y-%m-%d")<"2021-09-10" & format(datetime, "%Y-%m-%d")>"2021-04-30")

April<-read.csv("AprilDone.csv")
April$ID<-"April"
April$sex<-"Female"
April$datetime<-as.POSIXct(strptime(April$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
April<-subset(April, format(datetime, "%Y-%m-%d")<"2022-06-28" & format(datetime, "%Y-%m-%d")>"2021-04-30")

Betty<-read.csv("BettyDone.csv")
Betty$ID<-"Betty"
Betty$sex<-"Female"
Betty$datetime<-as.POSIXct(strptime(Betty$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Betty<-subset(Betty, format(datetime, "%Y-%m-%d")<"2021-09-10" & format(datetime, "%Y-%m-%d")>"2021-04-30")

Bugs<-read.csv("BugsDone.csv")
Bugs$ID<-"Bugs"
Bugs$sex<-"Male"
Bugs$datetime<-as.POSIXct(strptime(Bugs$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Bugs<-subset(Bugs, format(datetime, "%Y-%m-%d")<"2021-08-24" & format(datetime, "%Y-%m-%d")>"2021-05-01")

Carmen<-read.csv("CarmenDone.csv")
Carmen$ID<-"Carmen"
Carmen$sex<-"Female"
Carmen$datetime<-as.POSIXct(strptime(Carmen$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Carmen<-subset(Carmen, format(datetime, "%Y-%m-%d")<"2021-09-10" & format(datetime, "%Y-%m-%d")>"2021-04-30")

Chip<-read.csv("ChipDone.csv")
Chip$ID<-"Chip"
Chip$sex<-"Male"
Chip$datetime<-as.POSIXct(strptime(Chip$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Chip<-subset(Chip, format(datetime, "%Y-%m-%d")<"2021-09-10" & format(datetime, "%Y-%m-%d")>"2021-05-18")

Dale<-read.csv("DaleDone.csv")
Dale$ID<-"Dale"
Dale$sex<-"Male"
Dale$datetime<-as.POSIXct(strptime(Dale$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Dale<-subset(Dale, format(datetime, "%Y-%m-%d")<"2021-09-10" & format(datetime, "%Y-%m-%d")>"2021-05-04")

Dot<-read.csv("DotDone.csv")
Dot$ID<-"Dot"
Dot$sex<-"Female"
Dot$datetime<-as.POSIXct(strptime(Dot$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Dot<-subset(Dot, format(datetime, "%Y-%m-%d")<"2021-09-10" & format(datetime, "%Y-%m-%d")>"2021-04-30")

Elsa<-read.csv("ElsaDone.csv")
Elsa$ID<-"Elsa"
Elsa$sex<-"Female"
Elsa$datetime<-as.POSIXct(strptime(Elsa$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Elsa<-subset(Elsa, format(datetime, "%Y-%m-%d")<"2022-06-28" & format(datetime, "%Y-%m-%d")>"2021-04-30")

Felix<-read.csv("FelixDone.csv")
Felix$ID<-"Felix"
Felix$sex<-"Male"
Felix$datetime<-as.POSIXct(strptime(Felix$Adjusted.Local.Time, format("%m/%d/%Y %H:%M")))
Felix<-subset(Felix, format(datetime, "%Y-%m-%d")<"2021-07-17" & format(datetime, "%Y-%m-%d")>"2021-05-04")

Fiona<-read.csv("FionaDone.csv")
Fiona$ID<-"Fiona"
Fiona$sex<-"Female"
Fiona$datetime<-as.POSIXct(strptime(Fiona$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Fiona<-subset(Fiona, format(datetime, "%Y-%m-%d")<"2021-09-10" & format(datetime, "%Y-%m-%d")>"2021-04-30")

Granny<-read.csv("GrannyMissing.csv")
Granny$ID<-"Granny"
Granny$sex<-"Female"
Granny$datetime<-as.POSIXct(strptime(Granny$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Granny<-subset(Granny, format(datetime, "%Y-%m-%d")<"2021-06-14" & format(datetime, "%Y-%m-%d")>"2021-04-30")

Grouch<-read.csv("GrouchDone.csv")
Grouch$ID<-"Grouch"
Grouch$sex<-"Male"
Grouch$datetime<-as.POSIXct(strptime(Grouch$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Grouch<-subset(Grouch, format(datetime, "%Y-%m-%d")<"2021-07-31" & format(datetime, "%Y-%m-%d")>"2021-05-20")

Hubert<-read.csv("HubertDone.csv")
Hubert$ID<-"Hubert"
Hubert$sex<-"Male"
Hubert$datetime<-as.POSIXct(strptime(Hubert$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Hubert<-subset(Hubert, format(datetime, "%Y-%m-%d")<"2022-06-28" & format(datetime, "%Y-%m-%d")>"2021-05-22")

Marlo<-read.csv("MarloDone.csv")
Marlo$ID<-"Marlo"
Marlo$sex<-"Female"
Marlo$datetime<-as.POSIXct(strptime(Marlo$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Marlo<-subset(Marlo, format(datetime, "%Y-%m-%d")<"2022-06-28" & format(datetime, "%Y-%m-%d")>"2021-04-30")

Satan<-read.csv("SatanBroken.csv")
Satan$ID<-"Satan"
Satan$sex<-"Male"
Satan$datetime<-as.POSIXct(strptime(Satan$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Satan<-subset(Satan, format(datetime, "%Y-%m-%d")<"2021-08-24" & format(datetime, "%Y-%m-%d")>"2021-05-22")

Ziggy<-read.csv("ZiggyDone.csv")
Ziggy$ID<-"Ziggy"
Ziggy$sex<-"Male"
Ziggy$datetime<-as.POSIXct(strptime(Ziggy$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Ziggy<-subset(Ziggy, format(datetime, "%Y-%m-%d")<"2022-06-14" & format(datetime, "%Y-%m-%d")>"2021-05-04")

Turtles<-rbind(Alvin, April, Betty, Bugs,
               Carmen, Chip, Dale, Dot,
               Elsa, Felix, Fiona,
               Granny, Grouch, Hubert, Marlo,
               Satan, Ziggy)
Turtles$ID<-as.factor(Turtles$ID)
Turtles$sex<-as.factor(Turtles$sex)
Turtles<-Turtles[,c(2,5,7,13,14,15)]
Turtles$WetDry<-fct_recode(Turtles$Wet.Dry, "0"="wet", "1"="dry")






