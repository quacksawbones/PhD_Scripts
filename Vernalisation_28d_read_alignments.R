library(ggplot2)
library(reshape)
library(RColorBrewer)

#Desktop
#tissue_localisation <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/De Novo transcriptome/de_characterised_tissue_localisation.txt")

#Laptop
reads <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/DE - 28 days 4C/Vernalisation_28d_read_alignments.txt")


melted_reads <- melt(reads,id=c("Condition","Replicate"))

#tissue_alignments$Tissue <- factor(tissue_alignments$Tissue, levels = tissue_alignments$Tissue)

#Reverse order of Spectral colours
colours=brewer.pal(name="Spectral", n=nlevels(melted_reads$variable))
names(colours)=rev(levels(melted_reads$variable))

#What you are plotting
q <- ggplot(melted_reads,aes(Replicate,value,fill=variable))

#Type of chart and colour palette
q <- q + geom_bar(stat = "identity",width=1,color="black") + scale_fill_manual(values = colours)

#X/Y Axis details
q <- q + scale_y_continuous(expand = c(0,0), breaks = seq(5000000,25000000,5000000),limits=c(0,25000000),labels=seq(5,25,5))

#X Axis labels and particulars
q <- q  + xlab("Cultivars (4 replicates)") + theme(axis.title.x = element_text(size = 12), axis.text.x = element_blank())

#Y Axis labels and particulars
q <- q + ylab("Reads (Million)") + theme(axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12))

#Legends and Panels
q <- q + labs(fill = "Alignment Type") + theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(color="black"), axis.ticks.x = element_blank(),panel.background = element_blank())

#Reverse legend order
q <- q + guides(fill = guide_legend(reverse = TRUE))

#Facet details
q <- q + facet_grid(. ~ Condition,scale="free_x") + theme(strip.text = element_text(size = 12)) 

#load!
q
