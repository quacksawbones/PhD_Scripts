library(ggplot2)
library(reshape)
library(RColorBrewer)


#Desktop
#tissue_localisation <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/De Novo transcriptome/de_characterised_tissue_localisation.txt")

#Laptop
tissue_localisation <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/De Novo transcriptome/de_characterised_tissue_localisation.txt")

tissue_localisation$Tissue <- factor(tissue_localisation$Tissue, levels = tissue_localisation$Tissue)

levels(tissue_localisation$Tissue) <- gsub(" ", "\n", levels(tissue_localisation$Tissue))

levels(tissue_localisation$Tissue) <- gsub("_", " ", levels(tissue_localisation$Tissue))


#What you are plotting
p <- ggplot(tissue_localisation, aes(Tissue,RPKM,fill = Gene))

#Type of chart and colour palette
p <- p + geom_bar(stat = "identity",position="dodge") + scale_fill_brewer(palette = "Spectral")

#X/Y details
p <- p + scale_y_continuous(expand = c(0,0))

#X axis labels and particulars
p <- p + theme(axis.text.x = element_text(angle = 90,size=10,vjust=.4),axis.title.x=element_text(size=12))

#Y axis labels and particulars
p <- p + ylab("RPKM") + theme(axis.title.y=element_text(size=12))

#Legends and Panels
p <- p + theme(legend.position="none",panel.grid.minor.x = element_line(color="black"), panel.grid.minor = element_blank(),panel.background = element_blank())

#Extra lines
p <- p + geom_vline(xintercept=seq(0.5,16.5),color="gray") + geom_hline(yintercept=0)

# Facets
p <- p + facet_wrap(~Gene, scales = "free_y",ncol=1)

#load!
p




tissue_alignments <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/De Novo transcriptome/tissue_read_alignments.txt")

tissue_alignments$Tissue <- factor(tissue_alignments$Tissue, levels = rev(tissue_alignments$Tissue))

levels(tissue_alignments$Tissue) <- gsub(" ", "\n", levels(tissue_alignments$Tissue))

levels(tissue_alignments$Tissue) <- gsub("_", " ", levels(tissue_alignments$Tissue))

melted_reads <- melt(tissue_alignments,id="Tissue")

#tissue_alignments$Tissue <- factor(tissue_alignments$Tissue, levels = tissue_alignments$Tissue)

#Reverse order of Spectral colours
colours=brewer.pal(name="Spectral", n=nlevels(melted_reads$variable))
names(colours)=rev(levels(melted_reads$variable))


#What you are plotting
q <- ggplot(melted_reads,aes(Tissue,value,fill=variable))

#Type of chart and colour palette
q <- q + geom_bar(stat = "identity",width=1,color="black") + scale_fill_manual(values = colours)

#X/Y details
q <- q + scale_y_continuous(expand = c(0,0), breaks = seq(00000000,55000000,10000000),limits=c(0,55000000),labels=seq(0,55,10))

#X axis labels and particulars
q <- q + theme(axis.text.x = element_text(size=12,vjust=.4),axis.title.x=element_text(size=12))

#Y axis labels and particulars
q <- q + ylab("Reads (Million)") + theme(axis.title.y=element_text(size=12))

#Legends and Panels
q <- q + theme(panel.grid.minor = element_blank(),panel.background = element_blank()) + geom_hline(yintercept=seq(10000000,55000000,10000000),linetype="dashed")

#Reverse legend order
q <- q + labs(fill = "Alignment Type") + guides(fill = guide_legend(reverse = TRUE))

#Flip the chart
q <- q + coord_flip()

#load!
q
