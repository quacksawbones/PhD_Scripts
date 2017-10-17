library("ggplot2")

Vernalisation.Time.Course <- read.csv("C:/Users/cul07b/Dropbox/PhD - Darren/Safflower Cultivars/Vernalisation Time Course.csv")
Vernalisation.Time.Course.Outliers <- read.csv("C:/Users/cul07b/Dropbox/PhD - Darren/Safflower Cultivars/Vernalisation Time Course - Outliers.csv")

#Vernalisation.Time.Course <- read.csv("C:/Users/cul07b/Dropbox/PhD - Darren/Safflower Cultivars/Vernalisation Time Course.csv")


Vernalisation.Time.Course$Cultivar <- factor(Vernalisation.Time.Course$Cultivar, levels=c("S317","311.3.3"), labels=c("Spring","Winter"))
Vernalisation.Time.Course.Outliers$Cultivar <- factor(Vernalisation.Time.Course.Outliers$Cultivar, levels=c("S317","311.3.3"), labels=c("Spring","Winter"))


summary(Vernalisation.Time.Course[Vernalisation.Time.Course$Cultivar == "Spring",]$Days.to.Flowering..Cabinet.)
sd(Vernalisation.Time.Course[Vernalisation.Time.Course$Cultivar == "Spring",]$Days.to.Flowering..Cabinet.)

summary(Vernalisation.Time.Course[Vernalisation.Time.Course$Cultivar == "Winter",]$Days.to.Flowering..Cabinet.)
sd(Vernalisation.Time.Course[Vernalisation.Time.Course$Cultivar == "Winter",]$Days.to.Flowering..Cabinet.)


t.test(Vernalisation.Time.Course[Vernalisation.Time.Course$Cultivar == "Spring",]$Days.to.Flowering..Cabinet.,Vernalisation.Time.Course[Vernalisation.Time.Course$Cultivar == "Winter",]$Days.to.Flowering..Cabinet.)

coef(lm(Vernalisation.Time.Course[Vernalisation.Time.Course$Cultivar == "Spring",]$Days.to.Flowering..Cabinet. ~ Vernalisation.Time.Course[Vernalisation.Time.Course$Cultivar == "Spring",]$Days.Vernalised))

sat <- Vernalisation.Time.Course[Vernalisation.Time.Course$Cultivar == "Winter" & Vernalisation.Time.Course$Days.Vernalised > 0 & Vernalisation.Time.Course$Days.Vernalised <= 12,]
nonsat <- Vernalisation.Time.Course[Vernalisation.Time.Course$Cultivar == "Winter" & Vernalisation.Time.Course$Days.Vernalised > 16 & Vernalisation.Time.Course$Days.to.Flowering..Cabinet. <= 65,]


coef(lm(nonsat$Days.to.Flowering..Cabinet. ~ nonsat$Days.Vernalised))
coef(lm(sat$Days.to.Flowering..Cabinet. ~ sat$Days.Vernalised))


#XY Scatter plot
XY <- ggplot(aes(y = as.numeric(Days.to.Flowering..Cabinet.), x = Days.Vernalised), data = Vernalisation.Time.Course)
XY <- XY + geom_point(aes(shape = Cultivar),size = 5,colour = "red",data=Vernalisation.Time.Course.Outliers) + geom_point(aes(color = Cultivar,shape = Cultivar), size = 4,data=Vernalisation.Time.Course.Outliers)
XY <- XY + geom_point(aes(shape = Cultivar),size = 5,colour = "black") + scale_colour_manual(values=c("yellow","#3399FF")) + scale_fill_manual(values=c("yellow","#3399FF")) + geom_point(aes(color = Cultivar,shape = Cultivar), size = 4)
XY <- XY + geom_smooth(aes(group=Cultivar),color="black",se=FALSE,size=1.5) + geom_smooth(aes(group=Cultivar, color=Cultivar),se=FALSE,size=1)
XY <- XY + ylab("Days to Flowering") + xlab("Days Vernalised")
XY <- XY + scale_x_continuous(breaks=seq(0, 36, 2), limits = c(0,36), expand = c(0,0))
XY <- XY + scale_y_continuous(breaks=seq(40, 84, 5))
XY <- XY + theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),legend.text=element_text(size=12),panel.grid.major.y = element_line(color = "black",linetype = "solid"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),legend.title=element_text(size=14,face="bold"),panel.background = element_blank())
#XY <- XY + geom_abline(intercept = 53.8024949, slope = -0.2273836, color = "orange", size = 1)
#XY <- XY + geom_abline(intercept =  92.434579, slope = -3.649533, color = "#3399FF", size = 1)
#XY <- XY + geom_abline(intercept =  47.08163265, slope = -0.02040816, color = "#3399FF", size = 1)
XY


BOX <- ggplot(aes(y = as.numeric(Days.to.Flowering..Cabinet.), x = Cultivar, fill = Cultivar), data = Vernalisation.Time.Course)
BOX <- BOX + geom_boxplot()
BOX <- BOX + ylab("Days to Flowering") + xlab("Cultivar")
BOX <- BOX + scale_fill_manual(values=c("orange", "#3399FF"), guide=FALSE)
BOX <- BOX + theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=9),panel.grid.major.y = element_line(color = "black",linetype = "solid"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),legend.title=element_text(size=14,face="bold"),panel.background = element_blank())
BOX
