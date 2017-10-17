library("ggplot2")
library("DTK")

Timecourse_4_weeks_varied_temp <- read.csv("C:/Users/cul07b/Dropbox/PhD - Darren/Safflower Cultivars/Physiology - 4 weeks varied temp/Timecourse_4_weeks_varied_temp.csv",stringsAsFactors=TRUE)

Timecourse_4_weeks_varied_temp$Cultivar <- factor(Timecourse_4_weeks_varied_temp$Cultivar, levels=c("S317","C311"), labels=c("Spring","Winter"))

BOX <- ggplot(aes(y = as.integer(Days.to.flowering), x = factor(Temp), fill = Cultivar), data = Timecourse_4_weeks_varied_temp)
BOX <- BOX + geom_boxplot(width = 0.5)
BOX <- BOX + ylab("Days to Flowering") + xlab("Temperature (C)")
BOX <- BOX + scale_y_continuous(breaks=seq(70, 105, 5),limits = c(70,105), expand = c(0,0))
BOX <- BOX + scale_fill_manual(values=c("Yellow", "#3399FF"))
BOX <- BOX + theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),panel.grid.major.y = element_line(color = "black",linetype = "solid"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),legend.title=element_text(size=14,face="bold"),legend.text=element_text(size=12),panel.background = element_blank())
BOX

summary(Timecourse_4_weeks_varied_temp[Timecourse_4_weeks_varied_temp$Cultivar == "Winter",]$Days.to.flowering)
mean(Timecourse_4_weeks_varied_temp[Timecourse_4_weeks_varied_temp$Cultivar == "Winter",]$Days.to.flowering)
sd(Timecourse_4_weeks_varied_temp$Days.to.flowering)

t.test((Timecourse_4_weeks_varied_temp[(Timecourse_4_weeks_varied_temp$Cultivar == "Spring") & (Timecourse_4_weeks_varied_temp$Temp != 16),]$Days.to.flowering),(Timecourse_4_weeks_varied_temp[(Timecourse_4_weeks_varied_temp$Cultivar == "Spring") & (Timecourse_4_weeks_varied_temp$Temp == 16),]$Days.to.flowering))

Timecourse_4_weeks_varied_temp[(Timecourse_4_weeks_varied_temp$Cultivar == "Spring") & (Timecourse_4_weeks_varied_temp$Temp != 16),]

fit <- aov(Timecourse_4_weeks_varied_temp[(Timecourse_4_weeks_varied_temp$Cultivar == "Spring"),]$Days.to.flowering
         ~ factor(Timecourse_4_weeks_varied_temp[(Timecourse_4_weeks_varied_temp$Cultivar == "Spring"),]$Temp))

fit <- aov(Timecourse_4_weeks_varied_temp[(Timecourse_4_weeks_varied_temp$Cultivar == "Winter"),]$Days.to.flowering
           ~ factor(Timecourse_4_weeks_varied_temp[(Timecourse_4_weeks_varied_temp$Cultivar == "Winter"),]$Temp))

summary(fit)
TukeyHSD(fit)
#T3fit <- DTK.test(Timecourse_4_weeks_varied_temp[(Timecourse_4_weeks_varied_temp$Cultivar == "Winter"),]$Days.to.flowering,
#  factor(Timecourse_4_weeks_varied_temp[(Timecourse_4_weeks_varied_temp$Cultivar == "Winter"),]$Temp), 0.05)

#DTK.plot(T3fit)


                               x<-(na.omit(Timecourse_4_weeks_varied_temp[(Timecourse_4_weeks_varied_temp$Cultivar == "Winter") & (Timecourse_4_weeks_varied_temp$Temp == 0),]$Days.to.flowering))
y<-(na.omit(Timecourse_4_weeks_varied_temp[(Timecourse_4_weeks_varied_temp$Cultivar == "Winter") & (Timecourse_4_weeks_varied_temp$Temp == 8),]$Days.to.flowering))


t.test(x,y)

R.Version()
