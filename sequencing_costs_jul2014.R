library("ggplot2")
library("scales")

sequencing_costs_jul2014 <- read.delim("C:/Users/cul07b/Dropbox/PhD/Documents/sequencing_costs_jul2014.txt", stringsAsFactors=FALSE)
sequencing_costs_jul2014 <- read.delim("E:/Data/Dropbox/PhD/Documents/sequencing_costs_jul2014.txt", stringsAsFactors=FALSE)

sequencing_costs_jul2014$Cost.per.Mb <- as.numeric(substring(sequencing_costs_jul2014$Cost.per.Mb, 2))
sequencing_costs_jul2014$Date <- factor(sequencing_costs_jul2014$Date, levels = sequencing_costs_jul2014$Date, ordered = TRUE)

data <- ggplot(sequencing_costs_jul2014)
data + geom_point(stat = "identity", aes(x=Date, y=Cost.per.Mb, color = "orange", size = 3)) + ylab("Cost per Megabase ($)") + scale_y_continuous(trans=log_trans(),expand = c(0,0.2), limit=c(0.01,10000), breaks=c(0.01,0.1,1,10,100,1000,10000),labels=c(0.01,0.1,1,10,100,1000,10000)) + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90,vjust = 0.5), legend.position="none",panel.grid.major.x = element_line(color = "gray",linetype = "dotted"),axis.text.x = element_text(size=9),panel.grid.major.y = element_line(color = "black",linetype = "dashed"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),panel.background = element_blank())
