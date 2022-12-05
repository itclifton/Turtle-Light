setwd("~/Desktop/Research- PhD to Present/Side Projects/Turtle Light 2021/Turtle-Light/Kzoo Maps")
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
Satan1<-read.csv("Satan2.csv")
Satan<-rbind(Satan,Satan1)
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
# Turtles$WetDry<-as.numeric(as.character(Turtles$WetDry))
Turtles$Week<-as.numeric(strftime(Turtles$datetime,format="%W"))


## Preliminary Light Analysis ----
# Light Only - I'm not sure what duration means.
Turtles.lux<-Turtles[,c(1,2,3,5,6)]
Turtles.lux<-na.omit(Turtles.lux)
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
colnames(Weekly.Sex)<-c("Sex","Week","Mean","SE")
Weekly.Sex<-subset(Weekly.Sex, Week!="17" & Week!="34")
#Day.Sex.Mean$Month<-as.Date(Day.Sex.Mean$Month, format="%m")
p1<-ggplot(data=Weekly.Sex, aes(x=Week, y=Mean, group=Sex))+
  annotate("rect", xmin=22.6, xmax=24.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="blue")+
  annotate("rect", xmin=27.3, xmax=29.7, ymin=-Inf, ymax=Inf, alpha=0.2, fill="red")+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.5, position=position_dodge(width=0.2))+
  geom_point(aes(colour=Sex), size=7, position=position_dodge(width=0.2))+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=30,face="bold"),
        legend.text=element_text(size=20, face="bold"), legend.title=element_text(size=26, face="bold"))+
  scale_x_continuous(breaks=seq(from=18, to = 33, by = 1))+
  geom_line(aes(colour=Sex), position=position_dodge(w=0.2), size=2)+
  theme(legend.position="bottom")+
  ylab("Mean Daily Light (lx)")
#ggsave(path=path, filename="WeeklyLight.png", height=14, width=20, plot=p1)
# Data analysis
Ind.Summary1<-subset(Ind.Summary, Week!="17" & Week!="34")
Ind.Summary1$Week<-as.factor(Ind.Summary1$Week)
lm1<-lmer(Mean~sex+(1|Week)+(1|ID), data=Ind.Summary1)
summary(lm1)
em.m1<-emmeans(lm1, c("Week"))
summary(em.m1)
contrast(em.m1, 'tukey')
anova(lm1, type=3)
# End of season divergence
Ind.Summary2<-subset(Ind.Summary, Week>27 & Week<33)
lm2<-lmer((Mean/1000)~sex*Week+(1|sex:ID), data=Ind.Summary2)
summary(lm2)


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




Turtles.dur<-rbind(Alvin, April, Betty, Bugs,
                   Carmen, Chip, Dale, Dot,
                   Ernie, Felix, Fiona,
                   Granny, Grouch, Hubert, Marlo,
                   Satan, Ziggy)
Turtles.dur$ID<-as.factor(Turtles.dur$ID)
Turtles.dur$datetime<-as.POSIXct(strptime(Turtles.dur$Adjusted.Local.Time, format("%m/%d/%y %H:%M")))
Turtles.dur<-subset(Turtles.dur, Light!="NA")

Turtles.dur.Daily<-aggregate(Light~Wet.Dry+sex+ID+format(datetime, "%m-%d"), length, data=Turtles.dur)
Turtles.Wet<-subset(Turtles.dur.Daily, Wet.Dry=="wet")
Turtles.Dry<-subset(Turtles.dur.Daily, Wet.Dry=="dry")
Turtles.Dry$Prop<-Turtles.Dry$Light/288
Turtles.Dry$datetime<-as.Date(Turtles.Dry$`format(datetime, "%m-%d")`, format("%m-%d"))
Turtles.Dry$Week<-as.numeric(strftime(Turtles.Dry$datetime,format="%W"))

Prop.Sex.Mean<-aggregate(Prop~sex+Week, mean, data=Turtles.Dry)
Prop.Sex.se<-aggregate(Prop~sex+Week, st.err, data=Turtles.Dry)
Prop.Sex<-cbind(Prop.Sex.Mean, Prop.Sex.se$Prop)
colnames(Prop.Sex)<-c("Sex","Week","Mean","SE")
Prop.Sex<-subset(Prop.Sex, Week!="17" & Week !="34")

ggplot(data=Prop.Sex, aes(x=Week, y=Mean, group=Sex))+
  annotate("rect", xmin=22.6, xmax=24.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="blue")+
  annotate("rect", xmin=27.3, xmax=29.7, ymin=-Inf, ymax=Inf, alpha=0.2, fill="red")+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.5, position=position_dodge(width=0.2))+
  geom_point(aes(colour=Sex), size=7, position=position_dodge(width=0.2))+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=16, face="bold"), legend.title=element_text(size=16, face="bold"))+
  geom_line(aes(colour=Sex), position=position_dodge(w=0.2), size=2)+
  theme(legend.position="bottom")+
  ylab("Mean Daily Proportion Dry")

# Daytime only
Turtles.Dur.Daytime<-subset(Turtles.dur, format(datetime, "%H:%M:%S")<"21:22:00" & format(datetime, "%H:%M:%S")>"06:06:00")
Turtles.dur.Daytime.Daily<-aggregate(Light~Wet.Dry+sex+ID+format(datetime, "%m-%d"), length, data=Turtles.Dur.Daytime)
Turtles.dur.Daytime.Daily.mean<-aggregate(Light~Wet.Dry+sex+ID+format(datetime, "%m-%d"), mean, data=Turtles.Dur.Daytime)
colnames(Turtles.dur.Daytime.Daily.mean)[5]<-"lx"
Turtles.dur.Daytime.Daily<-cbind(Turtles.dur.Daytime.Daily, Turtles.dur.Daytime.Daily.mean$lx)
colnames(Turtles.dur.Daytime.Daily)[6]<-"lx"

Turtles.Wet.Daytime<-subset(Turtles.dur.Daytime.Daily, Wet.Dry=="wet")
Turtles.Dry.Daytime<-subset(Turtles.dur.Daytime.Daily, Wet.Dry=="dry")
Turtles.Dry.Daytime$Prop<-Turtles.Dry.Daytime$Light/183
Turtles.Dry.Daytime$datetime<-as.Date(Turtles.Dry.Daytime$`format(datetime, "%m-%d")`, format("%m-%d"))
Turtles.Dry.Daytime$Week<-as.numeric(strftime(Turtles.Dry.Daytime$datetime,format="%W"))

Prop.Sex.Mean.Day<-aggregate(Prop~sex+Week, mean, data=Turtles.Dry.Daytime)
Prop.Sex.se.Day<-aggregate(Prop~sex+Week, st.err, data=Turtles.Dry.Daytime)
Prop.Sex<-cbind(Prop.Sex.Mean.Day, Prop.Sex.se.Day$Prop)
colnames(Prop.Sex)<-c("Sex","Week","Mean","SE")
Prop.Sex<-subset(Prop.Sex, Week!="17" & Week!="34")

p2<-ggplot(data=Prop.Sex, aes(x=Week, y=Mean, group=Sex))+
  annotate("rect", xmin=22.6, xmax=24.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="blue")+
  annotate("rect", xmin=27.3, xmax=29.7, ymin=-Inf, ymax=Inf, alpha=0.2, fill="red")+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.5, position=position_dodge(width=0.2))+
  geom_point(aes(colour=Sex), size=7, position=position_dodge(width=0.2))+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=30,face="bold"),
        legend.text=element_text(size=20, face="bold"), legend.title=element_text(size=26, face="bold"))+
  scale_x_continuous(breaks=seq(from=18, to = 33, by = 1))+
  geom_line(aes(colour=Sex), position=position_dodge(w=0.2), size=2)+
  theme(legend.position="bottom")+
  ylab("Mean Proportion of Time Spent Dry")
#ggsave(path=path, filename="WeeklyDry.png", height=14, width=20, plot=p2)

Turtles.Dry.Daytime1<-subset(Turtles.Dry.Daytime, Week!="17" & Week!="34")
lm3<-lmer(Prop~sex+(1|ID)+(1|Week), data=Turtles.Dry.Daytime1)
summary(lm3)
anova(lm3, type=3)

## Only mean light levels when turtles are dry
Dry.light<-aggregate(lx~sex+Week, mean, data=Turtles.Dry.Daytime)
Dry.light.se<-aggregate(lx~sex+Week, st.err, data=Turtles.Dry.Daytime)
Dry.light<-cbind(Dry.light, Dry.light.se$lx)
colnames(Dry.light)<-c("Sex","Week","Mean","SE")
Dry.light<-subset(Dry.light, Week!="17" & Week!="34")

p3<-ggplot(data=Dry.light, aes(x=Week, y=Mean, group=Sex))+
  annotate("rect", xmin=22.6, xmax=24.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="blue")+
  annotate("rect", xmin=27.3, xmax=29.7, ymin=-Inf, ymax=Inf, alpha=0.2, fill="red")+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.5, position=position_dodge(width=0.2))+
  geom_point(aes(colour=Sex), size=7, position=position_dodge(width=0.2))+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=30,face="bold"),
        legend.text=element_text(size=20, face="bold"), legend.title=element_text(size=26, face="bold"))+
  scale_x_continuous(breaks=seq(from=18, to = 33, by = 1))+
  geom_line(aes(colour=Sex), position=position_dodge(w=0.2), size=2)+
  theme(legend.position="bottom")+
  ylab("Mean Light While Dry (lx)")
#ggsave(path=path, filename="DryLight.png", height=14, width=20, plot=p3)

Turtles.Dry.Daytime1<-subset(Turtles.Dry.Daytime, Week!=17 & Week!=34)
# Post nesting
Turtles.Dry.Daytime2<-subset(Turtles.Dry.Daytime,  Week>24 & Week!=34)
# Pre nesting
Turtles.Dry.Daytime3<-subset(Turtles.Dry.Daytime,  Week<25 & Week!=17)

Turtles.Dry.Daytime1$Week<-as.factor(Turtles.Dry.Daytime1$Week)
Turtles.Dry.Daytime2$Week<-as.factor(Turtles.Dry.Daytime2$Week)
Turtles.Dry.Daytime3$Week<-as.factor(Turtles.Dry.Daytime3$Week)
lm4<-lmer(lx~sex+(1|ID)+(1|Week), data=Turtles.Dry.Daytime2)
summary(lm4)
anova(lm4, type=3)
