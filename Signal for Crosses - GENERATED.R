library("ggplot2")
Signal <- read.table("C:/Users/cul07b/Dropbox/Signal for Crosses - GENERATED.txt", quote="\"")
Signal <- read.table("E:/Data/Dropbox/Signal for Crosses - GENERATED.txt", quote="\"")

data <- ggplot(Signal, aes(x=as.numeric(rownames(Signal)),y=V1))
data + geom_line(size=1) + ylab("Frequency") + xlab("(Genome Location)") + scale_y_continuous(limit=c(0,1), breaks=seq(0,1,0.1)) + theme(axis.title.y = element_text(vjust = 1),panel.grid.major.y = element_line(color = "black",linetype = "dashed"),panel.grid.minor.y = element_blank(),axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title=element_text(size=14,face="bold"),panel.background = element_blank())
