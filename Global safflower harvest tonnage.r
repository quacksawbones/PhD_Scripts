library("ggplot2")

#Laptop
safflowerTonnage_old<- read.csv("C:/Users/cul07b/Dropbox/PhD - Darren/Global safflower harvest tonnage_2012.csv", stringsAsFactors=FALSE)
safflowerTonnage<- read.csv("C:/Users/cul07b/Dropbox/PhD - Darren/Global safflower harvest tonnage_2014.csv", stringsAsFactors=FALSE)

#Desktop
#Old - safflowerTonnage<- read.csv("E:/Data/Dropbox/PhD - Darren/Global safflower harvest tonnage.csv", stringsAsFactors=FALSE)


safflowerTonnage$Country <- factor(safflowerTonnage$Country, levels = safflowerTonnage$Country[order(safflowerTonnage$X2014,decreasing = TRUE)])
p2 <- ggplot(data = safflowerTonnage)
p2 <- p2 + aes(x = Country, y = X2014)
p2 <- p2 + geom_bar(stat = "identity",fill=ifelse(safflowerTonnage$Country=="AUS", "dark red", "orange"))
p2 <- p2 + ylab("Safflower Seed Production (ton)") 
p2 <- p2 + scale_y_continuous(limit = c(0,200000), breaks = seq(0, 200000, by=25000))
p2 <- p2 + theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=9),panel.grid.major.y = element_line(color = "black",linetype = "solid"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),legend.title=element_text(size=14,face="bold"),panel.background = element_blank())
p2
