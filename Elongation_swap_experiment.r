library("ggplot2")
library("reshape2")

Elongation_swap_experiment <- read.csv("/data/temp/Safflower Vernalisation Physiology/Elongation_swap_experiment.txt")

Elongation_melt <- melt(Elongation_swap_experiment, id=c("Cultivar","Days.in.Final.Cabinet","Pot"))

Elongation_melt$variable <- gsub(".Day.Cabinet"," Day Cabinet",Elongation_melt$variable)

p <- ggplot(Elongation_melt,aes(Days.in.Final.Cabinet,value))

p <- p + geom_jitter(width = 0, height = 0.05, shape=21, size=2, aes(fill=Cultivar)) + scale_fill_manual(values = c("#3399FF","Yellow"))


p <- p + scale_y_continuous(limits = c(-0.1,1.1), breaks = seq(0, 1),labels = c("No","Yes"))

p <- p + scale_x_continuous(limits = c(0,40), breaks = seq(0, 40,10))

#X axis labels and particulars
p <- p + xlab("Days in Final Cabinet") + theme(axis.text.x = element_text(size=12), axis.title=element_text(size=12))

#Y axis labels and particulars
p <- p + ylab("Elongated?") + theme(axis.text.y = element_text(size=12))

#Panels
p <- p + theme(legend.text=element_text(size=12),panel.grid.major.x = element_line(color = "black",size = 0.25),panel.grid.major.y = element_blank(), panel.background = element_blank())

#Facet
p <- p + facet_grid(. ~ variable)
p


