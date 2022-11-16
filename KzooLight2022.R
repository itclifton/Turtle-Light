## Packages ----
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(suncalc)
library(lubridate)
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
Dale<-subset(Dale, format(datetime, "%Y-%m-%d")<"2021-09-10" & format(datetime, "%Y-%m-%d")>"2021-05-04")

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

# Medianlight per week- filtered
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

## Environmental correlations
m6<-lmer(Proportion.Dry~Discharge.ft.s+Julien+(1|ID), data=Daily.Summary.Env.2021)
summary(m6)
anova(m6)
fixef(m6)
plot(Proportion.Dry~Discharge.ft.s, data=Daily.Summary.Env.2021)
abline(a=2.947812e-01,b=(-1.535e-03*9.353e-05), col='red')






                         