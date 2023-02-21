## Packages ----
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(suncalc)
library(lubridate)
library(cowplot)
library(ggpubr)
library(Hmisc)
library(corrplot)
library(MuMIn)
library(devtools)
library(ggbiplot)
st.err = function(x) {sd(x)/sqrt(length(x))}
path<-"/Users/ianclifton/Desktop/Research- PhD to Present/Side Projects/Turtle Light 2021/Figures"

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
Carmen<-subset(Carmen, format(datetime, "%Y-%m-%d")<"2021-08-19" & format(datetime, "%Y-%m-%d")>"2021-04-30")

Chip<-read.csv("ChipDone.csv")
Chip$ID<-"Chip"
Chip$sex<-"Male"
Chip$datetime<-as.POSIXct(strptime(Chip$Adjusted.Local.Time, format("%m/%d/%y %H:%M")))
Chip<-subset(Chip, format(datetime, "%Y-%m-%d")<"2021-09-10" & format(datetime, "%Y-%m-%d")>"2021-05-18")

Dale<-read.csv("DaleDone.csv")
Dale$ID<-"Dale"
Dale$sex<-"Male"
Dale$datetime<-as.POSIXct(strptime(Dale$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Dale<-subset(Dale, format(datetime, "%Y-%m-%d")<"2021-07-02" & format(datetime, "%Y-%m-%d")>"2021-05-04")

Dot<-read.csv("DotDone.csv")
Dot$ID<-"Dot"
Dot$sex<-"Female"
Dot$datetime<-as.POSIXct(strptime(Dot$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Dot<-subset(Dot, format(datetime, "%Y-%m-%d")<"2021-09-10" & format(datetime, "%Y-%m-%d")>"2021-04-30")

Elsa<-read.csv("ElsaDone.csv")
Elsa$ID<-"Elsa"
Elsa$sex<-"Female"
Elsa$datetime<-as.POSIXct(strptime(Elsa$Adjusted.Local.Time, format("%m/%d/%y %H:%M")))
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

Satan1<-read.csv("SatanBroken.csv")
Satan2<-read.csv("Satan2.csv")
Satan<-rbind(Satan1,Satan2)
Satan$ID<-"Satan"
Satan$sex<-"Male"
Satan$datetime<-as.POSIXct(strptime(Satan$Adjusted.Local.Time, format("%Y-%m-%d %H:%M")))
Satan<-subset(Satan, format(datetime, "%Y-%m-%d")<"2022-05-25" & format(datetime, "%Y-%m-%d")>"2021-05-22")

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

## Filter by sunrise and sunset
start_date <- ymd("2021-05-01")
end_date <- ymd("2022-6-28")
dates<-seq(start_date, end_date,"days")
df.dates<-data.frame(dates)
# creates a sunrise and sunset column for each date from lat long
dat2 <- distinct(df.dates, dates) %>%
  with(., getSunlightTimes(date = dates,
                           lat = 42.2, lon = -85.1, tz = 'America/New_York',
                           keep = c('sunrise', 'sunset')))
# created a straight date column that would match date column of dat2 df
Turtles$date <- as.Date(Turtles$datetime)
# then filter 
Turtles.Day<-distinct(df.dates, dates) %>%
  with(., getSunlightTimes(date = dates,
                           lat = 42.2, lon = -85.1, tz = 'America/New_York',
                           keep = c('sunrise', 'sunset')))%>% 
  left_join(Turtles, dat2, by = "date") %>%
  filter(sunrise <= datetime, datetime <= sunset)
# Remove the observations that only summarize wet/dry duration
Turtles.Day<-na.omit(Turtles.Day)

## 11/9 meeting with Josh ----
# I still need to remove observations without a light measurement.
# Correlations between environmental cues and basking throughout the year
  # Particularly in overwintering data
# Compare seasonal basking between sexes
  # Only use 2021 active season data for seasonal basking differences
  # Link up with reproductive data
  # Lmer for weeks with eemeans to look at posthoc


## Daily individual summaries without filtering ----
# Sum Light
d.s.l<-aggregate(Light~sex+ID+format(datetime, "%Y-%m-%d"), sum, data=Turtles.Day)
colnames(d.s.l)<-c("sex","ID","datetime","Sum.Light")
# Mean Light
d.m.l<-aggregate(Light~sex+ID+format(datetime, "%Y-%m-%d"), mean, data=Turtles.Day)
colnames(d.m.l)<-c("sex","ID","datetime","Mean.Light")
# Median Light
d.me.l<-aggregate(Light~sex+ID+format(datetime, "%Y-%m-%d"), median, data=Turtles.Day)
colnames(d.me.l)<-c("sex","ID","datetime","Median.Light")
# Total Dry Count
Turtles.Day$WetDry<-as.numeric(as.character(Turtles.Day$WetDry))
d.s.d<-aggregate(WetDry~sex+ID+format(datetime, "%Y-%m-%d"), sum, data=Turtles.Day)
colnames(d.s.d)<-c("sex","ID","datetime","Sum.Dry")
# Mean Dry Count
d.m.d<-aggregate(WetDry~sex+ID+format(datetime, "%Y-%m-%d"), mean, data=Turtles.Day)
colnames(d.m.d)<-c("sex","ID","datetime","Mean.Dry")
# Median Dry Count
d.me.d<-aggregate(WetDry~sex+ID+format(datetime, "%Y-%m-%d"), median, data=Turtles.Day)
colnames(d.me.d)<-c("sex","ID","datetime","Median.Dry")
# Proportion of Day Dry
d.l.d<-aggregate(WetDry~sex+ID+format(datetime, "%Y-%m-%d"), length, data=Turtles.Day)
colnames(d.l.d)<-c("sex","ID","datetime","Length.WetDry")
# Merge All Current Summary Stats
Daily.Summary<-cbind(d.s.l, d.m.l$Mean.Light, d.me.l$Median.Light,
                     d.s.d$Sum.Dry, d.m.d$Mean.Dry, d.me.d$Median.Dry,
                     d.l.d$Length.WetDry)
colnames(Daily.Summary)<-c("Sex","ID","Date","Sum.Light","Mean.Light","Median.Light",
                           "Sum.Dry","Mean.Dry","Median.Dry","Lenth.WetDry")
# Proportion of day dry
Daily.Summary$Proportion.Dry<-(Daily.Summary$Sum.Dry/Daily.Summary$Lenth.WetDry)
# Add a column with number of daylight minutes to estimate minutes dry
Daily.Summary$Daylight.Minutes<-(Daily.Summary$Lenth.WetDry*5)
# It should range from 545 minutes to 835
# Remove one day from Ziggy because time was missed after a download
Daily.Summary<-subset(Daily.Summary, Daylight.Minutes!=470)
# Convert the Datetime to a usable format again.
Daily.Summary$Date<-as.POSIXct(strptime(Daily.Summary$Date, format("%Y-%m-%d")))
# Add a month column to help keep things clear
Daily.Summary$Month<-as.factor(strftime(Daily.Summary$Date,format="%B, %Y"))

## Merge in environmental data
Env.data<-read.csv("UltimatMapTurtleDataLogger.csv")
# Cut duplicate columns out
Env.data<-Env.data[,-c(2,4,6,7,8,9,10)]
# Usable date format
Env.data$Date<-as.POSIXct(strptime(Env.data$Date, format("%m/%d/%y")))
# Merge by ID and Date
Daily.Summary.Env<-merge(Daily.Summary, Env.data, by=c("ID","Date"))
# Write file to send to Josh
#write.csv(Daily.Summary.Env, "DailySummaryEnv.csv", row.names=F)

## Filter data by dry values to more directly assess basking- Only necessary for light ----
Turtles.Dry<-subset(Turtles.Day, Wet.Dry=="dry")
# Sum Light
dry.d.s.l<-aggregate(Light~sex+ID+format(datetime, "%Y-%m-%d"), sum, data=Turtles.Dry)
colnames(dry.d.s.l)<-c("sex","ID","datetime","Sum.Light")
# Mean Light
dry.d.m.l<-aggregate(Light~sex+ID+format(datetime, "%Y-%m-%d"), mean, data=Turtles.Dry)
colnames(dry.d.m.l)<-c("sex","ID","datetime","Mean.Light")
# Median Light
dry.d.me.l<-aggregate(Light~sex+ID+format(datetime, "%Y-%m-%d"), median, data=Turtles.Dry)
colnames(dry.d.me.l)<-c("sex","ID","datetime","Median.Light")

Dry.Daily.Summary<-cbind(dry.d.s.l, dry.d.m.l$Mean.Light, dry.d.me.l$Median.Light)
colnames(Dry.Daily.Summary)<-c("Sex","ID","Date","Sum.Light","Mean.Light","Median.Light")
# Convert the Datetime to a usable format again.
Dry.Daily.Summary$Date<-as.POSIXct(strptime(Dry.Daily.Summary$Date, format("%Y-%m-%d")))
# Add a month column to help keep things clear
Dry.Daily.Summary$Month<-as.factor(strftime(Dry.Daily.Summary$Date,format="%B, %Y"))

# Merge dry daily summary data with environmental data
Dry.Daily.Summary.Env<-merge(Env.data, Dry.Daily.Summary, by=c("ID","Date"))
# Write file to send to Josh
#write.csv(Daily.Summary.Env, "DryDailySummaryEnv.csv", row.names=F)

## Exploratory analyses and figures ----
## Unfiltered (by wet/dry state) light data
## Seasonal differences based on weekly means
Daily.Summary.Env$Week.Year<-as.factor(strftime(Daily.Summary.Env$Date,format="%W,%y"))
# Add a month column to help keep things clear
Daily.Summary.Env$Month<-as.factor(strftime(Daily.Summary.Env$Date,format="%B,%Y"))
# Put factors in chronological order- There's probably a much more straightforward way to do this
Daily.Summary.Env$Week.Year<-factor(Daily.Summary.Env$Week.Year,
                                    levels=c('17,21','18,21','19,21','20,21',
                                             '21,21','22,21','23,21','24,21',
                                             '25,21','26,21','27,21','28,21',
                                             '29,21','30,21','31,21','32,21',
                                             '33,21','34,21','35,21','36,21',
                                             '37,21','38,21','39,21','40,21',
                                             '41,21','42,21','43,21','44,21',
                                             '45,21','46,21','47,21','48,21',
                                             '49,21','50,21','51,21','52,21',
                                             '00,22','01,22','02,22','03,22',
                                             '04,22','05,22','06,22','07,22',
                                             '08,22','09,22','10,22','11,22',
                                             '12,22','13,22','14,22','15,22',
                                             '16,22','17,22','18,22','19,22',
                                             '20,22','21,22','22,22','23,22',
                                             '24,22','25,22','26,22'))
Daily.Summary.Env$Month<-factor(Daily.Summary.Env$Month,
                                    levels=c('May,2021','June,2021','July,2021','August,2021',
                                             'September,2021','October,2021','November,2021','December,2021',
                                             'January,2022','February,2022','March,2022','April,2022',
                                             'May,2022','June,2022'))
# Exclude dates outside 2021 active period
Daily.Summary.Env.2021<-subset(Daily.Summary.Env, format(Date, "%Y-%m-%d")<"2021-09-10")
Daily.Summary.Env.2021<-droplevels(Daily.Summary.Env.2021)
## Get weekly means and errors by sex
# Weekly mean by sex
Weekly.Summary.m<-aggregate(cbind(
  Sum.Light,Mean.Light,Median.Light,Proportion.Dry,Daylight.Minutes)~Sex+Week.Year, mean, data=Daily.Summary.Env.2021)
Weekly.Summary.se<-aggregate(cbind(
  Sum.Light,Mean.Light,Median.Light,Proportion.Dry,Daylight.Minutes)~Sex+Week.Year, st.err, data=Daily.Summary.Env.2021)
Weekly.Summary<-cbind(Weekly.Summary.m,Weekly.Summary.se$Sum.Light,Weekly.Summary.se$Mean.Light,Weekly.Summary.se$Median.Light,
                      Weekly.Summary.se$Proportion.Dry,Weekly.Summary.se$Daylight.Minutes)
colnames(Weekly.Summary)<-c('Sex','Week.Year','Sum.Light','Mean.Light','Median.Light',
                            'Proportion.Dry','Daylight.Minutes','Sum.Light.se','Mean.Light.se',
                            'Median.Light.se','Proportion.Dry.se','Daylight.Minutes.se')

# Mean light per week- unfiltered
m1<-lmer(Mean.Light~Sex*Week.Year+(1|ID), data=Daily.Summary.Env.2021)
summary(m1)
anova(m1)
m1.em<-emmeans(m1, list(pairwise~Sex*Week.Year), adjust = "tukey")
m1.coef<-as.data.frame(m1.em$`emmeans of Sex, Week.Year`)

m1.1<-lmer(Mean.Light~Week.Year+(1|ID), data=Daily.Summary.Env.2021)
summary(m1.1)
anova(m1.1)
m1.1.em<-emmeans(m1.1, list(pairwise~Week.Year), adjust = "tukey")
m1.1.coef<-as.data.frame(m1.1.em$`emmeans of Week.Year`)

m1.1.coef$Sex<-"Pooled"
m1.1.coef<-m1.1.coef[,c(7,1:6)]
m1.coefs<-rbind(m1.coef,m1.1.coef)
p1<-ggplot(data=m1.coefs, aes(x=Week.Year, y=emmean, group=Sex))+
  scale_x_discrete()+
  annotate("rect", xmin=.75, xmax=6, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=3, y=0, fontface="bold", label="May", size=8)+
  annotate("text", x=8, y=0, fontface="bold", label="Jun", size=8)+
  annotate("rect", xmin=10, xmax=14, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=12, y=0, fontface="bold", label="Jul", size=8)+
  annotate("text", x=16.5, y=0, fontface="bold", label="Aug", size=8)+
  annotate("rect", xmin=19, xmax=20.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=20, y=0, fontface="bold", label="Sep", size=8)+
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.5, position=position_dodge(width=0.4))+
  geom_line(aes(linetype=Sex, colour=Sex), position=position_dodge(w=0.4), size=1.5)+
  geom_point(aes(shape=Sex, colour=Sex), size=7, position=position_dodge(width=0.4))+
  scale_color_manual(values=c('#ABABAB','#5E5E5E', '#000000'))+
  theme_classic()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=30,face="bold"),
        legend.text=element_text(size=26, face="bold"), legend.title=element_text(size=30, face="bold"))+
  theme(legend.position = c(.8, .8))+
  ylab("Weekly Mean Light (lux)")+
  xlab("Week")
#ggsave("MeanLighUnF.jpg", plot=p1, width=18, height=10, path=path)

# Median light per week- unfiltered
m2<-lmer(Median.Light~Sex*Week.Year+(1|ID), data=Daily.Summary.Env.2021)
summary(m2)
anova(m2)
m2.em<-emmeans(m2, list(pairwise~Sex*Week.Year), adjust = "tukey")
m2.coef<-as.data.frame(m2.em$`emmeans of Sex, Week.Year`)

m2.1<-lmer(Median.Light~Week.Year+(1|ID), data=Daily.Summary.Env.2021)
summary(m2.1)
anova(m2.1)
m2.1.em<-emmeans(m2.1, list(pairwise~Week.Year), adjust = "tukey")
m2.1.coef<-as.data.frame(m2.1.em$`emmeans of Week.Year`)

m2.1.coef$Sex<-"Pooled"
m2.1.coef<-m2.1.coef[,c(7,1:6)]
m2.coefs<-rbind(m2.coef,m2.1.coef)
p2<-ggplot(data=m2.coefs, aes(x=Week.Year, y=emmean, group=Sex))+
  scale_x_discrete()+
  annotate("rect", xmin=.75, xmax=6, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=3, y=-50, fontface="bold", label="May", size=8)+
  annotate("text", x=8, y=-50, fontface="bold", label="Jun", size=8)+
  annotate("rect", xmin=10, xmax=14, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=12, y=0, fontface="bold", label="Jul", size=8)+
  annotate("text", x=16.5, y=0, fontface="bold", label="Aug", size=8)+
  annotate("rect", xmin=19, xmax=20.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=20, y=0, fontface="bold", label="Sep", size=8)+
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.5, position=position_dodge(width=0.4))+
  geom_line(aes(linetype=Sex, colour=Sex), position=position_dodge(w=0.4), size=1.5)+
  geom_point(aes(shape=Sex, colour=Sex), size=7, position=position_dodge(width=0.4))+
  scale_color_manual(values=c('#ABABAB','#5E5E5E', '#000000'))+
  theme_classic()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=30,face="bold"),
        legend.text=element_text(size=26, face="bold"), legend.title=element_text(size=30, face="bold"))+
  theme(legend.position = c(.8, .8))+
  ylab("Weekly Median Light (lux)")+
  xlab("Week")
#ggsave("MedianLighUnF.jpg", plot=p2, width=18, height=10, path=path)

# Proportion of time dry per week- unfiltered
m3<-lmer(Proportion.Dry~Sex*Week.Year+(1|ID), data=Daily.Summary.Env.2021)
summary(m3)
anova(m3)
m3.em<-emmeans(m3, list(pairwise~Sex*Week.Year), adjust = "tukey")
m3.coef<-as.data.frame(m3.em$`emmeans of Sex, Week.Year`)

m3.1<-lmer(Proportion.Dry~Week.Year+(1|ID), data=Daily.Summary.Env.2021)
summary(m3.1)
anova(m3.1)
m3.1.em<-emmeans(m3.1, list(pairwise~Week.Year), adjust = "tukey")
m3.1.coef<-as.data.frame(m3.1.em$`emmeans of Week.Year`)

m3.1.coef$Sex<-"Pooled"
m3.1.coef<-m3.1.coef[,c(7,1:6)]
m3.coefs<-rbind(m3.coef,m3.1.coef)
p3<-ggplot(data=m3.coefs, aes(x=Week.Year, y=emmean, group=Sex))+
  scale_x_discrete()+
  annotate("rect", xmin=.75, xmax=6, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=3, y=0, fontface="bold", label="May", size=8)+
  annotate("text", x=8, y=0, fontface="bold", label="Jun", size=8)+
  annotate("rect", xmin=10, xmax=14, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=12, y=0, fontface="bold", label="Jul", size=8)+
  annotate("text", x=16.5, y=0, fontface="bold", label="Aug", size=8)+
  annotate("rect", xmin=19, xmax=20.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=20, y=0, fontface="bold", label="Sep", size=8)+
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.5, position=position_dodge(width=0.4))+
  geom_line(aes(linetype=Sex, colour=Sex), position=position_dodge(w=0.4), size=1.5)+
  geom_point(aes(shape=Sex, colour=Sex), size=7, position=position_dodge(width=0.4))+
  scale_color_manual(values=c('#ABABAB','#5E5E5E', '#000000'))+
  theme_classic()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=30,face="bold"),
        legend.text=element_text(size=26, face="bold"), legend.title=element_text(size=30, face="bold"))+
  theme(legend.position = c(.8, .8))+
  ylab("Mean Proportion of Day Dry")+
  xlab("Week")
#ggsave("PropDryUnF.jpg", plot=p3, width=18, height=10, path=path)

## Filtered by wet/dry state light data
## Seasonal differences based on weekly means
Dry.Daily.Summary.Env$Week.Year<-as.factor(strftime(Dry.Daily.Summary.Env$Date,format="%W,%y"))
# Add a month column to help keep things clear
Dry.Daily.Summary.Env$Month<-as.factor(strftime(Dry.Daily.Summary.Env$Date,format="%B,%Y"))
# Put factors in chronological order- There's probably a much more straightforward way to do this
Dry.Daily.Summary.Env$Week.Year<-factor(Dry.Daily.Summary.Env$Week.Year,
                                    levels=c('17,21','18,21','19,21','20,21',
                                             '21,21','22,21','23,21','24,21',
                                             '25,21','26,21','27,21','28,21',
                                             '29,21','30,21','31,21','32,21',
                                             '33,21','34,21','35,21','36,21',
                                             '37,21','38,21','39,21','40,21',
                                             '41,21','42,21','43,21','44,21',
                                             '45,21','46,21','47,21','48,21',
                                             '49,21','50,21','51,21','52,21',
                                             '00,22','01,22','02,22','03,22',
                                             '04,22','05,22','06,22','07,22',
                                             '08,22','09,22','10,22','11,22',
                                             '12,22','13,22','14,22','15,22',
                                             '16,22','17,22','18,22','19,22',
                                             '20,22','21,22','22,22','23,22',
                                             '24,22','25,22','26,22'))
Dry.Daily.Summary.Env$Month<-factor(Dry.Daily.Summary.Env$Month,
                                levels=c('May,2021','June,2021','July,2021','August,2021',
                                         'September,2021','October,2021','November,2021','December,2021',
                                         'January,2022','February,2022','March,2022','April,2022',
                                         'May,2022','June,2022'))
# Exclude dates outside 2021 active period
Dry.Daily.Summary.Env.2021<-subset(Dry.Daily.Summary.Env, format(Date, "%Y-%m-%d")<"2021-09-10")
## Get weekly means and errors by sex
# Weekly mean by sex
Dry.Weekly.Summary.m<-aggregate(cbind(
  Sum.Light,Mean.Light,Median.Light)~Sex+Week.Year, mean, data=Dry.Daily.Summary.Env.2021)
Dry.Weekly.Summary.se<-aggregate(cbind(
  Sum.Light,Mean.Light,Median.Light)~Sex+Week.Year, st.err, data=Dry.Daily.Summary.Env.2021)
Dry.Weekly.Summary<-cbind(Dry.Weekly.Summary.m,Dry.Weekly.Summary.se$Sum.Light,Dry.Weekly.Summary.se$Mean.Light,Dry.Weekly.Summary.se$Median.Light)
colnames(Dry.Weekly.Summary)<-c('Sex','Week.Year','Sum.Light','Mean.Light','Median.Light',
                            'Sum.Light.se','Mean.Light.se','Median.Light.se')

# Mean light per week- filtered
m4<-lmer(Mean.Light~Sex*Week.Year+(1|ID), data=Dry.Daily.Summary.Env.2021)
summary(m4)
anova(m4)
m4.em<-emmeans(m4, list(pairwise~Sex*Week.Year), adjust = "tukey")
m4.coef<-as.data.frame(m4.em$`emmeans of Sex, Week.Year`)

m4.1<-lmer(Mean.Light~Week.Year+(1|ID), data=Dry.Daily.Summary.Env.2021)
summary(m4.1)
anova(m4.1)
m4.1.em<-emmeans(m4.1, list(pairwise~Week.Year), adjust = "tukey")
m4.1.coef<-as.data.frame(m4.1.em$`emmeans of Week.Year`)

m4.1.coef$Sex<-"Pooled"
m4.1.coef<-m4.1.coef[,c(7,1:6)]
m4.coefs<-rbind(m4.coef,m4.1.coef)
p4<-ggplot(data=m4.coefs, aes(x=Week.Year, y=emmean, group=Sex))+
  scale_x_discrete()+
  annotate("rect", xmin=.75, xmax=6, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=3, y=0, fontface="bold", label="May", size=8)+
  annotate("text", x=8, y=0, fontface="bold", label="Jun", size=8)+
  annotate("rect", xmin=10, xmax=14, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=12, y=0, fontface="bold", label="Jul", size=8)+
  annotate("text", x=16.5, y=0, fontface="bold", label="Aug", size=8)+
  annotate("rect", xmin=19, xmax=20.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=20, y=0, fontface="bold", label="Sep", size=8)+
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.5, position=position_dodge(width=0.4))+
  geom_line(aes(linetype=Sex, colour=Sex), position=position_dodge(w=0.4), size=1.5)+
  geom_point(aes(shape=Sex, colour=Sex), size=7, position=position_dodge(width=0.4))+
  scale_color_manual(values=c('#ABABAB','#5E5E5E', '#000000'))+
  theme_classic()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=30,face="bold"),
        legend.text=element_text(size=26, face="bold"), legend.title=element_text(size=30, face="bold"))+
  theme(legend.position = c(.8, .8))+
  ylab("Weekly Mean Light (lux)")+
  xlab("Week")
#ggsave("MeanLighF.jpg", plot=p4, width=18, height=10, path=path)

# Median light per week- filtered
m5<-lmer(Median.Light~Sex*Week.Year+(1|ID), data=Dry.Daily.Summary.Env.2021)
summary(m5)
anova(m5)
m5.em<-emmeans(m5, list(pairwise~Sex*Week.Year), adjust = "tukey")
m5.coef<-as.data.frame(m5.em$`emmeans of Sex, Week.Year`)

m5.1<-lmer(Median.Light~Week.Year+(1|ID), data=Dry.Daily.Summary.Env.2021)
summary(m5.1)
anova(m5.1)
m5.1.em<-emmeans(m5.1, list(pairwise~Week.Year), adjust = "tukey")
m5.1.coef<-as.data.frame(m5.1.em$`emmeans of Week.Year`)

m5.1.coef$Sex<-"Pooled"
m5.1.coef<-m5.1.coef[,c(7,1:6)]
m5.coefs<-rbind(m5.coef,m5.1.coef)
p5<-ggplot(data=m5.coefs, aes(x=Week.Year, y=emmean, group=Sex))+
  scale_x_discrete()+
  annotate("rect", xmin=.75, xmax=6, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=3, y=0, fontface="bold", label="May", size=8)+
  annotate("text", x=8, y=0, fontface="bold", label="Jun", size=8)+
  annotate("rect", xmin=10, xmax=14, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=12, y=0, fontface="bold", label="Jul", size=8)+
  annotate("text", x=16.5, y=0, fontface="bold", label="Aug", size=8)+
  annotate("rect", xmin=19, xmax=20.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=20, y=0, fontface="bold", label="Sep", size=8)+
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.5, position=position_dodge(width=0.4))+
  geom_line(aes(linetype=Sex, colour=Sex), position=position_dodge(w=0.4), size=1.5)+
  geom_point(aes(shape=Sex, colour=Sex), size=7, position=position_dodge(width=0.4))+
  scale_color_manual(values=c('#ABABAB','#5E5E5E', '#000000'))+
  theme_classic()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=30,face="bold"),
        legend.text=element_text(size=26, face="bold"), legend.title=element_text(size=30, face="bold"))+
  theme(legend.position = c(.8, .8))+
  ylab("Weekly Median Light (lux)")+
  xlab("Week")
#ggsave("MedianLighF.jpg", plot=p4, width=18, height=10, path=path)

# Fig 1- Combined full and dry and a second panel with proportion of time dry
# Mean unfiltered
m1.coef.com<-m1.coef[,1:4]
colnames(m1.coef.com)<-c("Sex","Week","UnMean","UnMean.se")
m1.coef.com$UnMean<-m1.coef.com$UnMean/1000
m1.coef.com$UnMean.se<-m1.coef.com$UnMean.se/1000
# Median unfiltered
m2.coef.com<-m2.coef[,3:4]
colnames(m2.coef.com)<-c("UnMedian","UnMedian.se")
m2.coef.com$UnMedian<-m2.coef.com$UnMedian/1000
m2.coef.com$UnMedian.se<-m2.coef.com$UnMedian.se/1000
# Mean dry
m4.coef.com<-m4.coef[,3:4]
colnames(m4.coef.com)<-c("DryMean","DryMean.se")
m4.coef.com$DryMean<-m4.coef.com$DryMean/1000
m4.coef.com$DryMean.se<-m4.coef.com$DryMean.se/1000
# Median dry
m5.coef.com<-m5.coef[,3:4]
colnames(m5.coef.com)<-c("DryMedian","DryMedian.se")
m5.coef.com$DryMedian<-m5.coef.com$DryMedian/1000
m5.coef.com$DryMedian.se<-m5.coef.com$DryMedian.se/1000
# Proportion Dry
m3.coef.com<-m3.coef[,3:4]
colnames(m3.coef.com)<-c("PropDry","PropDry.se")

# Combinded
light.coef.com<-cbind(m1.coef.com,m2.coef.com,m4.coef.com,m5.coef.com,m3.coef.com)
light.coef.com<-separate(light.coef.com, 'Week', paste("Week", 1:2, sep=""), sep=",", extra="drop")
light.coef.com<-light.coef.com[,-3]
colnames(light.coef.com)[2]<-"Week"

p6<-ggplot(data=light.coef.com, aes(x=as.numeric(Week), y=UnMean, group=Sex))+
  theme_classic()+
  annotate("rect", xmin=16, xmax=22, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=19, y=48, fontface="bold", label="May", size=8)+
  annotate("text", x=24, y=48, fontface="bold", label="Jun", size=8)+
  annotate("rect", xmin=26, xmax=30, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=28, y=48, fontface="bold", label="Jul", size=8)+
  annotate("text", x=32.5, y=48, fontface="bold", label="Aug", size=8)+
  annotate("rect", xmin=35, xmax=37, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=36, y=48, fontface="bold", label="Sep", size=8)+
  geom_errorbar(aes(ymin=UnMean-UnMean.se, ymax=UnMean+DryMean.se), width=.5, position=position_dodge(width=0.4))+
  geom_line(aes(linetype=Sex, colour=Sex), position=position_dodge(w=0.4), size=1.25)+
  geom_point(aes(shape=Sex, colour=Sex), size=5, position=position_dodge(width=0.4))+
  scale_color_manual(values=c('#ABABAB','#5E5E5E'))+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"))+
  theme(legend.position="None")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), breaks=seq(0,50,10))+
  scale_x_continuous(limits = c(16, 37), breaks=seq(17,36,2))+
  ylab("Mean Light (klx)")+
  annotate("text", x=16, y=48, fontface="bold", label="A", size=8, hjust=1)+
  xlab("")

p7<-ggplot(data=light.coef.com, aes(x=as.numeric(Week), y=DryMean, group=Sex))+
  theme_classic()+
  annotate("rect", xmin=16, xmax=22, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=19, y=77, fontface="bold", label="May", size=8)+
  annotate("text", x=24, y=77, fontface="bold", label="Jun", size=8)+
  annotate("rect", xmin=26, xmax=30, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=28, y=77, fontface="bold", label="Jul", size=8)+
  annotate("text", x=32.5, y=77, fontface="bold", label="Aug", size=8)+
  annotate("rect", xmin=35, xmax=37, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=36, y=77, fontface="bold", label="Sep", size=8)+
  geom_errorbar(aes(ymin=DryMean-DryMean.se, ymax=DryMean+DryMean.se), width=.5, position=position_dodge(width=0.4))+
  geom_line(aes(linetype=Sex, colour=Sex), position=position_dodge(w=0.4), size=1.25)+
  geom_point(aes(shape=Sex, colour=Sex), size=5, position=position_dodge(width=0.4))+
  scale_color_manual(values=c('#ABABAB','#5E5E5E'))+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.2, .22))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80), breaks=seq(0,80,20))+
  scale_x_continuous(limits = c(16, 37), breaks=seq(17,36,2))+
  ylab("Mean Light (klx)")+
  annotate("text", x=16, y=77, fontface="bold", label="C", size=8, hjust=1)+
  xlab("Week")

p8<-ggplot(data=light.coef.com, aes(x=as.numeric(Week), y=UnMedian, group=Sex))+
  theme_classic()+
  annotate("rect", xmin=16, xmax=22, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=19, y=48, fontface="bold", label="May", size=8)+
  annotate("text", x=24, y=48, fontface="bold", label="Jun", size=8)+
  annotate("rect", xmin=26, xmax=30, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=28, y=48, fontface="bold", label="Jul", size=8)+
  annotate("text", x=32.5, y=48, fontface="bold", label="Aug", size=8)+
  annotate("rect", xmin=35, xmax=37, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=36, y=48, fontface="bold", label="Sep", size=8)+
  geom_errorbar(aes(ymin=UnMedian-UnMedian.se, ymax=UnMedian+UnMedian.se), width=.5, position=position_dodge(width=0.4))+
  geom_line(aes(linetype=Sex, colour=Sex), position=position_dodge(w=0.4), size=1.25)+
  geom_point(aes(shape=Sex, colour=Sex), size=5, position=position_dodge(width=0.4))+
  scale_color_manual(values=c('#ABABAB','#5E5E5E'))+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"))+
  theme(legend.position="None")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), breaks=seq(0,50,10))+
  scale_x_continuous(limits = c(16, 37), breaks=seq(17,36,2))+
  ylab("Median Light (klx)")+
  annotate("text", x=16, y=48, fontface="bold", label="B", size=8, hjust=1)+
  xlab("")

p9<-ggplot(data=light.coef.com, aes(x=as.numeric(Week), y=DryMedian, group=Sex))+
  theme_classic()+
  annotate("rect", xmin=16, xmax=22, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=19, y=77, fontface="bold", label="May", size=8)+
  annotate("text", x=24, y=77, fontface="bold", label="Jun", size=8)+
  annotate("rect", xmin=26, xmax=30, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=28, y=77, fontface="bold", label="Jul", size=8)+
  annotate("text", x=32.5, y=77, fontface="bold", label="Aug", size=8)+
  annotate("rect", xmin=35, xmax=37, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=36, y=77, fontface="bold", label="Sep", size=8)+
  geom_errorbar(aes(ymin=DryMedian-DryMedian.se, ymax=DryMedian+DryMedian.se), width=.5, position=position_dodge(width=0.4))+
  geom_line(aes(linetype=Sex, colour=Sex), position=position_dodge(w=0.4), size=1.25)+
  geom_point(aes(shape=Sex, colour=Sex), size=5, position=position_dodge(width=0.4))+
  scale_color_manual(values=c('#ABABAB','#5E5E5E'))+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"))+
  theme(legend.position="None")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80), breaks=seq(0,80,20))+
  scale_x_continuous(limits = c(16, 37), breaks=seq(17,36,2))+
  ylab("Median Light (klx)")+
  annotate("text", x=16, y=77, fontface="bold", label="D", size=8, hjust=1)+
  xlab("Week")

Fig1<-plot_grid(p6, p8,
                p7, p9,
                ncol = 2, scale=0.99)+
  draw_label("Dry Only", size=28, fontface="bold", x=-0.03, y=0.25, vjust= 1.5, hjust=0, angle=90)+
  draw_label("Full", size=28, fontface="bold", x=-0.03, y=0.75, vjust= 1.5, hjust=0, angle=90)+
  theme(plot.margin = margin(6, 6, 6, 35, "pt"))
#ggsave("Fig1.jpg", plot=Fig1, width=19, height=9, path=path)

# Figure 2- Proportion of time dry

p10<-ggplot(data=light.coef.com, aes(x=as.numeric(Week), y=PropDry, group=Sex))+
  theme_classic()+
  annotate("rect", xmin=16, xmax=22, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=19, y=.95, fontface="bold", label="May", size=8)+
  annotate("text", x=24, y=.95, fontface="bold", label="Jun", size=8)+
  annotate("rect", xmin=26, xmax=30, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=28, y=.95, fontface="bold", label="Jul", size=8)+
  annotate("text", x=32.5, y=.95, fontface="bold", label="Aug", size=8)+
  annotate("rect", xmin=35, xmax=37, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+
  annotate("text", x=36, y=.95, fontface="bold", label="Sep", size=8)+
  geom_errorbar(aes(ymin=PropDry-PropDry.se, ymax=PropDry+PropDry.se), width=.5, position=position_dodge(width=0.4))+
  geom_line(aes(linetype=Sex, colour=Sex), position=position_dodge(w=0.4), size=1.25)+
  geom_point(aes(shape=Sex, colour=Sex), size=5, position=position_dodge(width=0.4))+
  scale_color_manual(values=c('#ABABAB','#5E5E5E'))+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=seq(0,1,.25))+
  scale_x_continuous(limits = c(16, 37), breaks=seq(17,36,2))+
  ylab("Proportion of Day Dry")+
  xlab("Week")
#ggsave("Fig2.jpg", plot=p10, width=10, height=6, path=path)

## Look at each individual summarized for each day ----
Daily.Summary.Env$Sum.Light<-Daily.Summary.Env$Sum.Light/1000
Daily.Summary.Env$Mean.Light<-Daily.Summary.Env$Mean.Light/1000
Daily.Summary.Env$Median.Light<-Daily.Summary.Env$Median.Light/1000

levels(Daily.Summary.Env$ID)

#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Alvin"))
p11<-ggplot(data=subset(Daily.Summary.Env, ID=="Alvin"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="April"))
# Left out a full year
p12<-ggplot(data=subset(Daily.Summary.Env, ID=="April"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Betty"))
p13<-ggplot(data=subset(Daily.Summary.Env, ID=="Betty"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Bugs"))
p14<-ggplot(data=subset(Daily.Summary.Env, ID=="Bugs"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Carmen"))
p15<-ggplot(data=subset(Daily.Summary.Env, ID=="Carmen"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Chip"))
p16<-ggplot(data=subset(Daily.Summary.Env, ID=="Chip"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Dale"))
p17<-ggplot(data=subset(Daily.Summary.Env, ID=="Dale"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Dot"))
p18<-ggplot(data=subset(Daily.Summary.Env, ID=="Dot"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Elsa"))
# Left out a full year
p19<-ggplot(data=subset(Daily.Summary.Env, ID=="Elsa"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Felix"))
p20<-ggplot(data=subset(Daily.Summary.Env, ID=="Felix"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Fiona"))
p21<-ggplot(data=subset(Daily.Summary.Env, ID=="Fiona"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Granny"))
p22<-ggplot(data=subset(Daily.Summary.Env, ID=="Granny"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Grouch"))
p23<-ggplot(data=subset(Daily.Summary.Env, ID=="Grouch"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Hubert"))
# Left out a full year
p24<-ggplot(data=subset(Daily.Summary.Env, ID=="Hubert"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Marlo"))
# Left out a full year
p25<-ggplot(data=subset(Daily.Summary.Env, ID=="Marlo"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Satan"))
p26<-ggplot(data=subset(Daily.Summary.Env, ID=="Satan"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")
#plot(Mean.Light~Date, data=subset(Daily.Summary.Env, ID=="Ziggy"))
# Left out a full year
p27<-ggplot(data=subset(Daily.Summary.Env, ID=="Ziggy"), aes(x=Date, y=Mean.Light))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  ylab("Mean Light (klx)")+
  xlab("Date")

plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Alvin"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="April"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Betty"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Bugs"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Carmen"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Chip"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Dale"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Dot"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Elsa"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Felix"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Fiona"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Granny"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Grouch"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Hubert"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Marlo"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Satan"))
plot(Proportion.Dry~Date, data=subset(Daily.Summary.Env, ID=="Ziggy"))


Daily.Yearlong<-subset(Daily.Summary.Env, ID=="April" | ID=="Elsa" | ID=="Hubert" | ID=="Marlo" | ID=="Ziggy")
test<-subset(Daily.Yearlong, Proportion.Dry==0)
Daily.Yearlong.Med<-aggregate(Proportion.Dry~Date, median, data=Daily.Yearlong)


Year.Light<-ggplot(data=Daily.Yearlong, aes(x=Date, y=Mean.Light, colour=ID))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,10))+
  scale_x_datetime(date_breaks="1 month", date_labels="%b")+
  ylab("Mean Light (klx)")+
  xlab("Date")

Year.Dry<-ggplot(data=Daily.Yearlong, aes(x=Date, y=Proportion.Dry, colour=ID))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = "bottom", plot.margin = margin(25, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=seq(0,1,.1))+
  scale_x_datetime(date_breaks="1 month", date_labels="%b")+
  ylab("Proportion of Day Dry")+
  xlab("Date")

Year.Dry.Med<-ggplot(data=Daily.Yearlong.Med, aes(x=Date, y=Proportion.Dry))+
  theme_classic()+
  geom_point(size=3, color="#9e9e9e")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=seq(0,1,.1))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b %y")+
  geom_vline(xintercept = as.POSIXct(as.Date("2021-10-21")), color = "red", lwd = 1.5, linetype=2)+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-04-04")), color = "red", lwd = 1.5, linetype=2)+
  ylab("Proportion of Day Dry")+
  xlab("Date")
#ggsave("Fig3.jpg", plot=Year.Dry.Med, width=10, height=6, path=path)

ggplot(data=Daily.Yearlong, aes(x=Date, y=Water.Mean))+
  theme_classic()+
  geom_point(size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = "bottom", plot.margin = margin(25, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30), breaks=seq(0,30,5))+
  scale_x_datetime(date_breaks="1 month", date_labels="%b")+
  ylab("Minimum Water Temperature (°C)")+
  xlab("Date")

## Environmental correlations ----
## Get the daily mean for all turtles and compare that against environmental measures
Pooled.Summary<-aggregate(cbind(Sum.Light,Mean.Light,Median.Light,Proportion.Dry,
                                Water.Min,Water.Max,Water.Mean,Avg.Wind,CloudCover,Precip,Air.Max.C,Air.Min.C,Air.Mean.C,RiverHeight.ft,Discharge.ft.s)~
                            Date, mean, data=Daily.Summary.Env.2021)
Pooled.Summary$Sum.Light<-Pooled.Summary$Sum.Light/1000
Pooled.Summary$Mean.Light<-Pooled.Summary$Mean.Light/1000
Pooled.Summary$Median.Light<-Pooled.Summary$Median.Light/1000

# Environmental Correlations
EnvCorr<-Pooled.Summary[, c(6:16)]
CorMat<-cor(EnvCorr)
round(CorMat, 2)
CorMat2<-rcorr(as.matrix(EnvCorr))
corrplot(CorMat2$r, type="upper", order="hclust", 
         p.mat=CorMat2$P, sig.level=0.05, insig="blank")

EnvCorr.pca<-prcomp(EnvCorr)
summary(EnvCorr.pca)
ggbiplot(EnvCorr.pca)
## Mean Light
m6<-lm(Mean.Light~Water.Min, data=Pooled.Summary)
summary(m6)
anova(m6)
m6.coef<-coef(m6)
plot(Mean.Light~Water.Min, data=Pooled.Summary)
abline(a=m6.coef[1],b=m6.coef[2], col='red')

m7<-lm(Mean.Light~Water.Max, data=Pooled.Summary)
summary(m7)
anova(m7)
m7.coef<-coef(m7)
plot(Mean.Light~Water.Max, data=Pooled.Summary)
abline(a=m7.coef[1],b=m7.coef[2], col='red')

m8<-lm(Mean.Light~Water.Mean, data=Pooled.Summary)
summary(m8)
anova(m8)
m8.coef<-coef(m8)
plot(Mean.Light~Water.Mean, data=Pooled.Summary)
abline(a=m8.coef[1],b=m8.coef[2], col='red')

m9<-lm(Mean.Light~Avg.Wind, data=Pooled.Summary)
summary(m9)
anova(m9)
m9.coef<-coef(m9)
plot(Mean.Light~Avg.Wind, data=Pooled.Summary)
abline(a=m9.coef[1],b=m9.coef[2], col='red')

m10<-lm(Mean.Light~CloudCover, data=Pooled.Summary)
summary(m10)
anova(m10)
m10.coef<-coef(m10)
plot(Mean.Light~CloudCover, data=Pooled.Summary)
abline(a=m10.coef[1],b=m10.coef[2], col='red')

m11<-lm(Mean.Light~Precip, data=Pooled.Summary)
summary(m11)
anova(m11)
m11.coef<-coef(m11)
plot(Mean.Light~Precip, data=Pooled.Summary)
abline(a=m11.coef[1],b=m11.coef[2], col='red')

m12<-lm(Mean.Light~Air.Max.C, data=Pooled.Summary)
summary(m12)
anova(m12)
m12.coef<-coef(m12)
plot(Mean.Light~Air.Max.C, data=Pooled.Summary)
abline(a=m12.coef[1],b=m12.coef[2], col='red')

m13<-lm(Mean.Light~Air.Min.C, data=Pooled.Summary)
summary(m13)
anova(m13)
m13.coef<-coef(m13)
plot(Mean.Light~Air.Min.C, data=Pooled.Summary)
abline(a=m13.coef[1],b=m13.coef[2], col='red')

m14<-lm(Mean.Light~Air.Mean.C, data=Pooled.Summary)
summary(m14)
anova(m14)
m14.coef<-coef(m14)
plot(Mean.Light~Air.Mean.C, data=Pooled.Summary)
abline(a=m14.coef[1],b=m14.coef[2], col='red')

m15<-lm(Mean.Light~RiverHeight.ft, data=Pooled.Summary)
summary(m15)
anova(m15)
m15.coef<-coef(m15)
plot(Mean.Light~RiverHeight.ft, data=Pooled.Summary)
abline(a=m15.coef[1],b=m15.coef[2], col='red')

m16<-lm(Mean.Light~Discharge.ft.s, data=Pooled.Summary)
summary(m16)
anova(m16)
m16.coef<-coef(m16)
plot(Mean.Light~Discharge.ft.s, data=Pooled.Summary)
abline(a=m16.coef[1],b=m16.coef[2], col='red')

# AIC Models- Mean Light
aic1<-lm(Mean.Light~Water.Min+Water.Max+Water.Mean+Avg.Wind+CloudCover+Precip
        +Air.Max.C+Air.Min.C+Air.Mean.C+RiverHeight.ft+Discharge.ft.s,
        data=Pooled.Summary)
summary(aic1)
anova(aic1)
AIC(aic1)

aic2<-lm(Mean.Light~Water.Min+Water.Max+Water.Mean+
         +Air.Max.C+Air.Min.C+Air.Mean.C+Discharge.ft.s,
         data=Pooled.Summary)
summary(aic2)
anova(aic2)
AIC(aic2)

## Proportion of Day Dry
m17<-lm(Proportion.Dry~Water.Min, data=Pooled.Summary)
summary(m17)
anova(m17)
m17.coef<-coef(m17)
plot(Proportion.Dry~Water.Min, data=Pooled.Summary)
abline(a=m17.coef[1],b=m17.coef[2], col='red')

m18<-lm(Proportion.Dry~Water.Max, data=Pooled.Summary)
summary(m18)
anova(m18)
m18.coef<-coef(m18)
plot(Proportion.Dry~Water.Max, data=Pooled.Summary)
abline(a=m18.coef[1],b=m18.coef[2], col='red')

m19<-lm(Proportion.Dry~Water.Mean, data=Pooled.Summary)
summary(m19)
anova(m19)
m19.coef<-coef(m19)
plot(Proportion.Dry~Water.Mean, data=Pooled.Summary)
abline(a=m19.coef[1],b=m19.coef[2], col='red')

m20<-lm(Proportion.Dry~Avg.Wind, data=Pooled.Summary)
summary(m20)
anova(m20)
m20.coef<-coef(m20)
plot(Proportion.Dry~Avg.Wind, data=Pooled.Summary)
abline(a=m20.coef[1],b=m20.coef[2], col='red')

m21<-lm(Proportion.Dry~CloudCover, data=Pooled.Summary)
summary(m21)
anova(m21)
m21.coef<-coef(m21)
plot(Proportion.Dry~CloudCover, data=Pooled.Summary)
abline(a=m21.coef[1],b=m21.coef[2], col='red')

m22<-lm(Proportion.Dry~Precip, data=Pooled.Summary)
summary(m22)
anova(m22)
m22.coef<-coef(m22)
plot(Proportion.Dry~Precip, data=Pooled.Summary)
abline(a=m22.coef[1],b=m22.coef[2], col='red')

m23<-lm(Proportion.Dry~Air.Max.C, data=Pooled.Summary)
summary(m23)
anova(m23)
m23.coef<-coef(m23)
plot(Proportion.Dry~Air.Max.C, data=Pooled.Summary)
abline(a=m23.coef[1],b=m23.coef[2], col='red')

m24<-lm(Proportion.Dry~Air.Min.C, data=Pooled.Summary)
summary(m24)
anova(m24)
m24.coef<-coef(m24)
plot(Proportion.Dry~Air.Min.C, data=Pooled.Summary)
abline(a=m24.coef[1],b=m24.coef[2], col='red')

m25<-lm(Proportion.Dry~Air.Mean.C, data=Pooled.Summary)
summary(m25)
anova(m25)
m25.coef<-coef(m25)
plot(Proportion.Dry~Air.Mean.C, data=Pooled.Summary)
abline(a=m25.coef[1],b=m25.coef[2], col='red')

m26<-lm(Proportion.Dry~RiverHeight.ft, data=Pooled.Summary)
summary(m26)
anova(m26)
m26.coef<-coef(m26)
plot(Proportion.Dry~RiverHeight.ft, data=Pooled.Summary)
abline(a=m26.coef[1],b=m26.coef[2], col='red')

m27<-lm(Proportion.Dry~Discharge.ft.s, data=Pooled.Summary)
summary(m27)
anova(m27)
m27.coef<-coef(m27)
plot(Proportion.Dry~Discharge.ft.s, data=Pooled.Summary)
abline(a=m27.coef[1],b=m27.coef[2], col='red')

# AIC models- Proportion Dry
aic3<-lm(Proportion.Dry~Water.Min+Water.Max+Water.Mean+Avg.Wind+CloudCover+Precip
         +Air.Max.C+Air.Min.C+Air.Mean.C+RiverHeight.ft+Discharge.ft.s,
         data=Pooled.Summary)
summary(aic3)
anova(aic3)
AIC(aic3)

aic4<-lm(Proportion.Dry~Water.Min+Water.Max+Water.Mean+
           +Air.Max.C+Air.Min.C+Air.Mean.C+Discharge.ft.s,
         data=Pooled.Summary)
summary(aic4)
anova(aic4)
AIC(aic4)

p28<-ggplot(data=Pooled.Summary, aes(x=Date, y=Proportion.Dry))+
  theme_classic()+
  geom_point(size=2)+
  #geom_point(aes(x=Date, y=Water.Max), color="blue", size=2)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(legend.position = c(.75, .7), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=seq(0,1,.25))+
  ylab("Mean Proportion of Day Dry")+
  xlab("Date")

