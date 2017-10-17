library(ggplot2)
library(reshape)
library(RColorBrewer)

#Desktop
#tissue_localisation <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/De Novo transcriptome/de_characterised_tissue_localisation.txt")

#Laptop
reads <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/DE - Timecourse/timecourse_reads.txt")


melted_reads <- melt(reads,id=c("Cultivar","Days","Rep"))

#tissue_alignments$Tissue <- factor(tissue_alignments$Tissue, levels = tissue_alignments$Tissue)

#Reverse order of Spectral colours
colours=brewer.pal(name="Spectral", n=nlevels(melted_reads$variable))
names(colours)=rev(levels(melted_reads$variable))

#What you are plotting
q <- ggplot(melted_reads,aes(Rep,value,fill=variable))

#Type of chart and colour palette
q <- q + geom_bar(stat = "identity",width=1,color="black") + scale_fill_manual(values=colours)# + scale_fill_brewer(palette = "Spectral")

#X/Y Axis details
q <- q + scale_y_continuous(expand = c(0,0), breaks = seq(10000000,60000000,10000000),limits=c(0,60000000),labels=seq(10,60,10))

#X axis labels and particulars
q <- q + xlab("Days vernalised (3 replicates)") + theme(axis.title.x = element_text(size=12),axis.ticks.x = element_blank(), axis.text.x = element_blank())

#Y  axis labels and particulars
q <- q + ylab("Reads (Million)") + labs(fill = "Alignment Type") + theme(axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))

#Legend and Panels
q <- q + theme(panel.margin.y = unit(.5, "lines"),panel.margin.x = unit(0, "lines"), panel.grid.minor = element_blank(), panel.grid.major.y = element_line(color="black"), panel.background = element_rect(fill=NA,color="black"))

#Reverse legend order
q <- q + guides(fill = guide_legend(reverse = TRUE))

#Facet details
q <- q + facet_grid(Cultivar ~ Days,scale="free_x") + theme(strip.text = element_text(size = 12))

#load!
q

