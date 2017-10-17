library("ggplot2")
library("scales")

Temps <- read.csv("C:/Users/cul07b/Dropbox/PhD - Darren/2014 Birdcage Temps.csv",stringsAsFactors=FALSE)


Temps$Date <- as.Date(Temps$Date,"%d/%m/%Y")

#Temps$Date <- factor(Temps$Date, ordered=TRUE)




plot <- ggplot(Temps) + geom_ribbon(aes(x=Date,ymin=Min,ymax=Max),color="orange", fill="orange")
plot <- plot + scale_x_date(labels = date_format("%b %Y"),breaks = date_breaks("month")) + scale_y_continuous(expand = c(0,0.2), limit=c(-10,40), breaks=seq(-10, 40, 5),labels=seq(-10, 40, 5))
plot <- plot + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90,vjust = 0.5), legend.position="none",panel.grid.major.x = element_blank(),axis.text.x = element_text(size=9),panel.grid.major.y = element_line(color = "black",linetype = "solid"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),panel.background = element_blank()) + ylab("Temperature (C)")
plot
