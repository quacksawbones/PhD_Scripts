library(ggplot2)

#Desktop
RNA.Results <- read.csv("E:/Data/Dropbox/PhD - Darren/RNA Extraction/RNA Extraction Method Results.csv")

#Laptop
RNA.Results <- read.csv("C:/Users/cul07b/Dropbox/PhD - Darren/RNA Extraction/RNA Extraction Method Results.csv")

RNA.Results$Stage <- factor(RNA.Results$Stage, levels = rev(levels(RNA.Results$Stage)))


#What you are plotting
p <- ggplot(RNA.Results, aes(X260.280, X260.230))

#Type of chart and colour palette
p <- p + geom_point(aes(shape = factor(Method),color = factor(Stage),size = (ng.uL))) + scale_size(range = c(2,10),labels = c(100,500,1000,1500)) + scale_colour_manual(values = c("Orange","Purple"))

#X/Y details
p <- p  + scale_x_continuous(limits = c(1.6,2.4), breaks = seq(1.6, 2.4, .2)) + scale_shape_manual(values = c(15,16,18,9,7,13))

#X axis labels and particulars
p <- p + xlab("Absorbance (260/280)") + theme(axis.text.x = element_text(size=12), axis.title=element_text(size=12))

#Y axis labels and particulars
p <- p + ylab("Absorbance (260/230)") + theme(axis.text.y = element_text(size=12))

#Legends
p <- p + labs(color = "DNAse Treatment", size = "Yield (ng/uL)", shape = "Extraction Method") + theme(legend.title=element_text(size=12))

#Legends - change size of legend elements
p <- p + guides(shape = guide_legend(override.aes = list(size=6), order = 1), color = guide_legend(override.aes = list(size=6),order = 2))

#Panels
p <- p + theme(legend.text=element_text(size=12),panel.grid.major = element_line(color = "black",linetype = "dashed"),panel.grid.minor.y = element_blank(), panel.background = element_blank())

#Lines
p <- p + geom_hline(yintercept = c(2,2.2), colour="red") + geom_vline(xintercept=2, colour="red")

#load
p


