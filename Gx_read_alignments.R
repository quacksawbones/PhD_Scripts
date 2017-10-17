library(ggplot2)
library(reshape)
library(RColorBrewer)

#Desktop
#tissue_localisation <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/De Novo transcriptome/de_characterised_tissue_localisation.txt")

#Laptop
reads <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/De Novo Genome/safflower_gx_biokanga_allin_3.1.1/Gx_read_alignments_no_reads_or_chimeric.txt")


melted_reads <- melt(reads,id=c("Insert","Library"))
melted_reads$Insert <- factor(melted_reads$Insert, levels = melted_reads$Insert)

#tissue_alignments$Tissue <- factor(tissue_alignments$Tissue, levels = tissue_alignments$Tissue)

#Reverse order of Spectral colours
colours=brewer.pal(name="Spectral", n=nlevels(melted_reads$variable))
names(colours)=rev(levels(melted_reads$variable))

#What you are plotting
q <- ggplot(melted_reads,aes(Insert,value,fill=variable))

#Type of chart and colour palette
q <- q + geom_bar(stat = "identity",width=1,color="black") + scale_fill_manual(values=colours)# + scale_fill_brewer(palette = "Spectral")

#X/Y Axis details
q <- q + scale_y_continuous(expand = c(0,0), breaks = seq(100000000,400000000,50000000),limits=c(0,400000000),labels=seq(100000000,400000000,50000000))

#X axis labels and particulars
q <- q + xlab("Library\n(Fragment Lengths - Left: 180 bp -> 180 bp, Right: 100 bp -> 500 bp)") + theme(axis.title.x = element_text(size=12), axis.text.x = element_blank(),axis.ticks.x = element_blank())

#Y  axis labels and particulars
q <- q + ylab("Reads") + labs(fill = "Alignment Type") + theme(axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))

#Legend and Panels
q <- q + theme(panel.margin.y = unit(.5, "lines"),panel.margin.x = unit(0, "lines"), panel.grid.minor = element_blank(), panel.grid.major.y = element_line(color="black"), panel.background = element_blank())

#Reverse legend order
q <- q + guides(fill = guide_legend(reverse = TRUE))

#Facet details
q <- q + facet_grid(. ~ Library,scale="free_x") + theme(strip.text = element_blank())

#load!
q

