library("ggplot2")
library("FSA") #NB: This is for the Kruskall Wallace test for non-normal distributions
library("reshape")

#Import Data - Desktop
Safflower_cultivar_harvest_Q0 <- read.delim("E:/Data/Dropbox/PhD/Safflower Cultivars/Safflower_cultivar_harvest_Q0.txt", dec=",", stringsAsFactors=FALSE)
Safflower_cultivar_harvest_Q1 <- read.delim("E:/Data/Dropbox/PhD/Safflower Cultivars/Safflower_cultivar_harvest_Q1_planted_20131104.txt", dec=",", stringsAsFactors=FALSE)


#Import Data - laptop
Safflower_cultivar_harvest_Q0 <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Safflower Cultivars/Safflower_cultivar_harvest_Q0.txt", dec=",", stringsAsFactors=FALSE)
Safflower_cultivar_harvest_Q1 <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Safflower Cultivars/Safflower_cultivar_harvest_Q1_planted_20131104.txt", dec=",", stringsAsFactors=FALSE)



#convert string of dates to a list - Q0 only
Safflower_cultivar_harvest_Q0$Flowering.Dates <- strsplit(Safflower_cultivar_harvest_Q0$Flowering.Dates,",")
Safflower_cultivar_harvest_Q0$Flowering.Dates <- as.Date(strsplit(Safflower_cultivar_harvest_Q0$Flowering.Dates,","),"%d/%m/%Y")


Safflower_cultivar_harvest_Q0$harvested <- as.Date(Safflower_cultivar_harvest_Q0$harvested,"%d/%m/%Y")
Safflower_cultivar_harvest_Q0$sown <- as.Date(Safflower_cultivar_harvest_Q0$sown,"%d/%m/%Y")

#NB: This will count the number of elements in each row

dfCQ0 <- data.frame(cultivar=factor(Safflower_cultivar_harvest_Q0$cultivar), Vernalised=factor(Safflower_cultivar_harvest_Q0$ver.), Days.to.Flowering=as.numeric(Safflower_cultivar_harvest_Q0$Days.to.Flowering))
dfCQ0$CnV <- interaction(dfCQ0$cultivar, dfCQ0$Vernalised)


dfC <- data.frame(cultivar=factor(Safflower_cultivar_harvest_Q1$Cultivar,), Vernalised=factor(Safflower_cultivar_harvest_Q1$Vernalised), Days.to.Flowering=as.numeric(Safflower_cultivar_harvest_Q1$Days.to.Flowering), Primary.Stem.Leaf.Number=as.numeric(Safflower_cultivar_harvest_Q1$Primary.Stem.Leaf.Number), Plant.Height=as.numeric(Safflower_cultivar_harvest_Q1$Plant.Height))
dfC$CnV <- interaction(dfC$cultivar, dfC$Vernalised)


#only use this when you need to filter out and just use the 154311 cultivar alongside S317
dfC <- dfC[dfC$cultivar %in% c("S317", "154311"),]
#dfC <- dfC[dfC$cultivar %in% c("154311"),]
dfCQ0 <- dfCQ0[dfCQ0$cultivar %in% c("S317.7", "154311"),]
dfCQ0$cultivar <- factor(dfCQ0$cultivar, levels=c("S317.7","154311"), labels=c("Spring","Winter"))
#dfCQ0 <- dfCQ0[complete.cases(dfCQ0),]


#Q0 - Days to Flowering
Q0 <- ggplot(aes(y = as.numeric(Days.to.Flowering), x = Vernalised, fill = cultivar), data = dfCQ0)
Q0 <- Q0 + geom_boxplot()
Q0 <- Q0 + scale_y_continuous(limits=c(50,80),expand=c(0,0))
Q0 <- Q0 + scale_fill_manual("Cultivar",values=c("yellow", "#3399FF"))
Q0 <- Q0 + scale_x_discrete()
Q0 <- Q0 + ylab("Days to Flowering")
#Q0 <- Q0 + annotate(geom="text", x=3, y=30, label="Scatter plot",color="red")
Q0 <- Q0 + theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),panel.grid.major.y = element_line(color = "black",linetype = "solid"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),legend.title=element_text(size=14,face="bold"),legend.text=element_text(size=12),panel.background = element_blank())
Q0



#https://stackoverflow.com/questions/23330279/ggplot2-annotate-labelling-geom-boxplot-with-position-dodge


count(dfCQ0, "Cultivar")
#Q1
Q1 <- ggplot(aes(y = as.numeric(Days.to.Flowering), x = cultivar, fill = Vernalised), data = dfC)
Q1 <- Q1 + geom_boxplot()
Q1 <- Q1 + scale_y_continuous(limits=c(50,90))
Q1 <- Q1 + scale_fill_manual(values=c("yellow", "#3399FF"))
Q1 <- Q1 + scale_x_discrete(breaks=c("154311", "S317"), labels=c("Winter", "Spring"))
Q1 <- Q1 + ylab("Days to Flowering") + xlab("Cultivar")
Q1 <- Q1 + theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),panel.grid.major.y = element_line(color = "black",linetype = "solid"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),legend.title=element_text(size=14,face="bold"),legend.text=element_text(size=12),panel.background = element_blank())
Q1


R1 <- ggplot(aes(y = as.numeric(Primary.Stem.Leaf.Number), x = cultivar, fill = Vernalised), data = dfC)
R1 <- R1 + geom_boxplot()
R1 <- R1 + scale_y_continuous(limits=c(10,50))
R1 <- R1 + scale_fill_manual(values=c("yellow", "#3399FF"))
R1 <- R1 + scale_x_discrete(breaks=c("154311", "S317"), labels=c("Winter", "Spring"))
R1 <- R1 + ylab("Nubmer of Leaves (Stem)") + xlab("Cultivar")
R1 <- R1 + theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),panel.grid.major.y = element_line(color = "black",linetype = "solid"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),legend.title=element_text(size=14,face="bold"),legend.text=element_text(size=12),panel.background = element_blank())
R1

S1 <- ggplot(aes(y = as.numeric(Plant.Height), x = cultivar, fill = Vernalised), data = dfC)
S1 <- S1 + geom_boxplot()
S1 <- S1 + scale_y_continuous(limits=c(350,1000))
S1 <- S1 + scale_fill_manual(values=c("yellow", "#3399FF"))
S1 <- S1 + scale_x_discrete(breaks=c("154311", "S317"), labels=c("Winter", "Spring"))
S1 <- S1 + ylab("Plant Height (mm)") + xlab("Cultivar")
S1 <- S1 + theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),panel.grid.major.y = element_line(color = "black",linetype = "solid"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),legend.title=element_text(size=14,face="bold"),legend.text=element_text(size=12),panel.background = element_blank())
S1



dfC_melted <- melt(dfC, id=c("cultivar","Vernalised","CnV"))

dfC_melted <- dfC_melted[dfC_melted$cultivar != "S317",]

levels(dfC_melted$variable)[levels(dfC_melted$variable)=="Days.to.Flowering"] <- "Days to Flowering"
levels(dfC_melted$variable)[levels(dfC_melted$variable)=="Primary.Stem.Leaf.Number"] <- "Leaf Number (Primary Stem)"
levels(dfC_melted$variable)[levels(dfC_melted$variable)=="Plant.Height"] <- "Plant Height (mm)"


X1 <- ggplot(aes(y = value, x = "Winter Safflower", fill = Vernalised), data = dfC_melted)
X1 <- X1 + geom_boxplot()
#X1 <- X1 + scale_y_continuous(limits=c(350,1000))
X1 <- X1 + scale_fill_manual(values=c("yellow", "#3399FF"))
#X1 <- X1 + scale_x_discrete(breaks=c("154311", "S317"), labels=c("Winter", "Spring"))
X1 <- X1 + xlab("Winter Safflower")
X1 <- X1 + theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),panel.grid.major.y = element_line(color = "black",linetype = "solid"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),legend.title=element_text(size=14,face="bold"),legend.text=element_text(size=12), panel.background = element_blank())
X1 <- X1 + facet_grid(variable ~ .,scales = "free_y") + theme(axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),panel.margin = unit(1, "lines"))
X1




test <- dfC[dfC$CnV == "S317.Y",]


#normality
shapiro.test(dfCQ0[dfCQ0$CnV == "154311.yes",]$Days.to.Flowering)
shapiro.test(dfCQ0[dfCQ0$CnV == "154311.no",]$Days.to.Flowering)
shapiro.test(dfCQ0[dfCQ0$CnV == "S317.7.yes",]$Days.to.Flowering)
shapiro.test(dfCQ0[dfCQ0$CnV == "S317.7.no",]$Days.to.Flowering)

#Assumption of equal variance (vernalisation ones fail)
sd(dfCQ0[dfCQ0$CnV == "154311.yes",]$Days.to.Flowering,na.rm=TRUE)
sd(dfCQ0[dfCQ0$CnV == "154311.no",]$Days.to.Flowering,na.rm=TRUE)
sd(dfCQ0[dfCQ0$CnV == "S317.7.yes",]$Days.to.Flowering,na.rm=TRUE)
sd(dfCQ0[dfCQ0$CnV == "S317.7.no",]$Days.to.Flowering,na.rm=TRUE)

#Two mean t-test (Flowering, Q0)
t.test(dfCQ0[dfCQ0$CnV == "S317.7.yes",]$Days.to.Flowering,dfCQ0[dfCQ0$CnV == "S317.7.no",]$Days.to.Flowering)
t.test(dfCQ0[dfCQ0$CnV == "154311.yes",]$Days.to.Flowering,dfCQ0[dfCQ0$CnV == "154311.no",]$Days.to.Flowering)
t.test(dfCQ0[dfCQ0$CnV == "S317.7.yes",]$Days.to.Flowering,dfCQ0[dfCQ0$CnV == "154311.yes",]$Days.to.Flowering)
t.test(dfCQ0[dfCQ0$CnV == "S317.7.no",]$Days.to.Flowering,dfCQ0[dfCQ0$CnV == "154311.no",]$Days.to.Flowering)

#Two mean t-test (Winter Safflower, Q1)
t.test(dfC[dfC$CnV == "154311.yes",]$Days.to.Flowering,dfC[dfC$CnV == "154311.no",]$Days.to.Flowering)
t.test(dfC[dfC$CnV == "154311.yes",]$Primary.Stem.Leaf.Number,dfC[dfC$CnV == "154311.no",]$Primary.Stem.Leaf.Number)
t.test(dfC[dfC$CnV == "154311.yes",]$Plant.Height,dfC[dfC$CnV == "154311.no",]$Plant.Height)



#unvernalised C311 everything else
t.test(dfCQ0[dfCQ0$CnV == "154311.no",]$Days.to.Flowering,dfCQ0[dfCQ0$CnV == "154311.yes" | dfCQ0$CnV == "S317.7.no" | dfCQ0$CnV == "S317.7.yes",]$Days.to.Flowering)

#vernalised C311 vs S317
t.test(dfCQ0[dfCQ0$CnV == "154311.yes",]$Days.to.Flowering,dfCQ0[dfCQ0$CnV == "S317.7.no" | dfCQ0$CnV == "S317.7.yes",]$Days.to.Flowering)


#ANOVA - Kruskall- Wallace
kw_vernalisation <- kruskal.test(dfCQ0$Days.to.Flowering~dfCQ0$CnV)
dunn.test(dfCQ0$Days.to.Flowering~dfCQ0$CnV)
dunnTest(dfCQ0$Days.to.Flowering~dfCQ0$CnV)

 #Everything below here is relating to manipulation of the flowering date of Q0
length(Safflower_cultivar_harvest_Q0$Flowering.Dates)

for (i in 1:length(Safflower_cultivar_harvest_Q0$Flowering.Dates)) {
  Safflower_cultivar_harvest_Q0$Flowering.Dates2 <- rapply(Safflower_cultivar_harvest_Q0$Flowering.Dates[i],as.Date("%d/%m/%Y"))
}

x <- c(1:length(Safflower_cultivar_harvest_Q0$Flowering.Dates))

Safflower_cultivar_harvest_Q0$Flowering.Dates2 = NULL

for (i in 1:length(Safflower_cultivar_harvest_Q0$Flowering.Dates)) {
  x <- difftime(as.Date(Safflower_cultivar_harvest_Q0$harvested[i],"%d/%m/%Y"),as.Date(Safflower_cultivar_harvest_Q0$Flowering.Dates[i][rapply(Safflower_cultivar_harvest_Q0$Flowering.Dates,length)])
}




Safflower_cultivar_harvest_Q0$Flowering.Dates[[1]][[1]]

Safflower_cultivar_harvest_Q0$harvested - Safflower_cultivar_harvest_Q0$Flowering.Dates[[,1:lapply(Safflower_cultivar_harvest_Q0$Flowering.Dates,length)]

rapply(Safflower_cultivar_harvest_Q0$Flowering.Dates,length)




# function for number of observations 
give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

# function for mean labels
mean.n <- function(x){
  return(c(y = median(x)*0.97, label = round(mean(x),2))) 
  # experiment with the multiplier to find the perfect position
}


