#library
library("DESeq2")
library("BiocParallel")
library("ggplot2")
library("reshape2")
register(MulticoreParam(4))


#import data as matrix (one per time point and cultivar, all replicates in a single matrix)
`C311_00` <- read.delim("/slowA/cul07b/vernalisation_time_course/3.8.1/DESeq/C311_00_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`C311_05` <- read.delim("/slowA/cul07b/vernalisation_time_course/3.8.1/DESeq/C311_05_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`C311_10` <- read.delim("/slowA/cul07b/vernalisation_time_course/3.8.1/DESeq/C311_10_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`C311_15` <- read.delim("/slowA/cul07b/vernalisation_time_course/3.8.1/DESeq/C311_15_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`C311_20` <- read.delim("/slowA/cul07b/vernalisation_time_course/3.8.1/DESeq/C311_20_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)

`S317_00` <- read.delim("/slowA/cul07b/vernalisation_time_course/3.8.1/DESeq/S317_00_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`S317_05` <- read.delim("/slowA/cul07b/vernalisation_time_course/3.8.1/DESeq/S317_05_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`S317_10` <- read.delim("/slowA/cul07b/vernalisation_time_course/3.8.1/DESeq/S317_10_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`S317_15` <- read.delim("/slowA/cul07b/vernalisation_time_course/3.8.1/DESeq/S317_15_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`S317_20` <- read.delim("/slowA/cul07b/vernalisation_time_course/3.8.1/DESeq/S317_20_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)


#import data as matrix (one per time point and cultivar, all replicates in a single matrix)
`C311_00` <- read.delim("G:/PhD/Bentham/vernalisation_time_course/DESeq/C311_00_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`C311_05` <- read.delim("G:/PhD/Bentham/vernalisation_time_course/DESeq/C311_05_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`C311_10` <- read.delim("G:/PhD/Bentham/vernalisation_time_course/DESeq/C311_10_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`C311_15` <- read.delim("G:/PhD/Bentham/vernalisation_time_course/DESeq/C311_15_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`C311_20` <- read.delim("G:/PhD/Bentham/vernalisation_time_course/DESeq/C311_20_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)

`S317_00` <- read.delim("G:/PhD/Bentham/vernalisation_time_course/DESeq/S317_00_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`S317_05` <- read.delim("G:/PhD/Bentham/vernalisation_time_course/DESeq/S317_05_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`S317_10` <- read.delim("G:/PhD/Bentham/vernalisation_time_course/DESeq/S317_10_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`S317_15` <- read.delim("G:/PhD/Bentham/vernalisation_time_course/DESeq/S317_15_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)
`S317_20` <- read.delim("G:/PhD/Bentham/vernalisation_time_course/DESeq/S317_20_vernalisation_time_course_align_vs_S317.txt", header = TRUE, row.names=1)


#import timecourse conditions for the experiment.
`condition_timecourse` <- read.delim("G:/PhD/Bentham/vernalisation_time_course/DESeq/condition_timecourse.txt", header = TRUE)
`condition_timecourse` <- read.delim("/slowA/cul07b/vernalisation_time_course/3.8.1/DESeq/condition_timecourse.txt", header = TRUE)

#Combine the condition and count data
#NB: This will give a warning message that there are numerics and did you want them to be factors. This should be fine as is.
#dds_timecourse <- DESeqDataSetFromMatrix(countData = cbind(S317_00, S317_05, S317_10, S317_15, S317_20,C311_00, C311_05, C311_10, C311_15, C311_20), colData = condition_timecourse, design = ~ cultivar + time + cultivar:time)

dds_timecourse <- DESeqDataSetFromMatrix(countData = cbind(C311_00, C311_05, C311_10, C311_15, C311_20, S317_00, S317_05, S317_10, S317_15, S317_20), colData = condition_timecourse, design = ~ cultivar + time + cultivar:time)

#Compute the time course by cultivar and time (Likelihood Ratio Test = LRT), remove "cultivar specific differences" over time
dds_timecourse_TC <- DESeq(dds_timecourse, test="LRT", reduced = ~ cultivar + time, parallel=TRUE)

#Create a table for the results
res_dds_timecourse_TC <- results(dds_timecourse_TC, parallel=TRUE)


#
res_dds_timecourse_TC$symbol <- mcols(dds_timecourse_TC)$symbol
#Confirm the analysis has results, order by (adjusted) p-value
head(res_dds_timecourse_TC[order(res_dds_timecourse_TC$pvalue),])
head(res_dds_timecourse_TC[order(res_dds_timecourse_TC$padj),])

#This extracts any values that are significantly differentially expressed (adjusted), alpha = 0.01. Removes NA results
res_dds_timecourse_TC_alpha <- na.omit(res_dds_timecourse_TC[order(res_dds_timecourse_TC$padj),])
res_dds_timecourse_TC_alpha[res_dds_timecourse_TC_alpha$padj < 0.05,]

#Pull out differential expression data based on a SPECIFIC transcript name
res_dds_timecourse_TC_alpha[row.names(res_dds_timecourse_TC_alpha) == "<TRANSRIPT NAME>",]

#NB: This section allows you to plot specific transcripts and their counts over time in a single image.
#data <- plotCounts(dds_timecourse_TC, <TRANSCRIPT>, intgroup=c("cultivar","time"), returnData=TRUE)
FLC <- plotCounts(dds_timecourse_TC, "CarTin_tx_s317_comp33367_c7_seq4", intgroup=c("cultivar","time"), returnData=TRUE)
FT <- plotCounts(dds_timecourse_TC, "CarTin_tx_s317_comp32761_c0_seq1", intgroup=c("cultivar","time"), returnData=TRUE)
AP1 <- plotCounts(dds_timecourse_TC, "CarTin_tx_s317_comp26769_c0_seq1", intgroup=c("cultivar","time"), returnData=TRUE)
VRN1 <- plotCounts(dds_timecourse_TC, "CarTin_tx_s317_comp33519_c0_seq70", intgroup=c("cultivar","time"), returnData=TRUE)

Actin <- plotCounts(dds_timecourse_TC, "CarTin_tx_s317_comp36134_c0_seq1", intgroup=c("cultivar","time"), returnData=TRUE)
GAPDH <- plotCounts(dds_timecourse_TC, "CarTin_tx_s317_comp34418_c0_seq1", intgroup=c("cultivar","time"), returnData=TRUE)


#NB: This plots the four "vernalisation" genes, Actin and GAPDH on the one graph. DON'T USE if you want ALL significant graphs!
DE_Genes_names <- c("CarTin_tx_s317_comp26769_c0_seq1","CarTin_tx_s317_comp33367_c7_seq4","CarTin_tx_s317_comp32761_c0_seq1","CarTin_tx_s317_comp33519_c0_seq70","CarTin_tx_s317_comp36134_c0_seq1","CarTin_tx_s317_comp34418_c0_seq1")
sigplots <- lapply(DE_Genes_names, function(x) {plotCounts(dds_timecourse_TC, x, intgroup=c("cultivar","time"), returnData=TRUE)})
sigplots <- Map(cbind, sigplots, transcript = DE_Genes_names)

melted_sigplots <- melt(sigplots,id=c("count","cultivar","time","transcript"))
melted_sigplots$L1<- NULL
melted_sigplots$cultivar <- gsub("C311", "Winter", melted_sigplots$cultivar)
melted_sigplots$cultivar <- gsub("S317", "Spring", melted_sigplots$cultivar)
melted_sigplots$padj <- res_dds_timecourse_TC_alpha[melted_sigplots$transcript,]$padj

melted_sigplots$transcript <- paste(melted_sigplots$transcript, ", p-value = ", round(res_dds_timecourse_TC_alpha[melted_sigplots$transcript,]$padj,3), sep = "")
melted_sigplots$transcript <- gsub("CarTin_tx_s317_comp26769_c0_seq1", "AP1-like", melted_sigplots$transcript)
melted_sigplots$transcript <- gsub("CarTin_tx_s317_comp33367_c7_seq4", "FLC-like", melted_sigplots$transcript)
melted_sigplots$transcript <- gsub("CarTin_tx_s317_comp32761_c0_seq1", "FT-like", melted_sigplots$transcript)
melted_sigplots$transcript <- gsub("CarTin_tx_s317_comp33519_c0_seq70", "VRN1-like", melted_sigplots$transcript)
melted_sigplots$transcript <- gsub("CarTin_tx_s317_comp36134_c0_seq1", "Actin", melted_sigplots$transcript)
melted_sigplots$transcript <- gsub("CarTin_tx_s317_comp34418_c0_seq1", "GAPDH", melted_sigplots$transcript)

melted_sigplots$transcript <- factor(melted_sigplots$transcript, levels = melted_sigplots$transcript)


#NB: Change the significance value to your cut-off i.e. alpha = 0.05|1. DO NOT use if you want specific genes as outline above
sigplots <- lapply(row.names(res_dds_timecourse_TC_alpha[res_dds_timecourse_TC_alpha$padj < 0.05,]), function(x) {plotCounts(dds_timecourse_TC, x, intgroup=c("cultivar","time"), returnData=TRUE)})
sigplots <- Map(cbind, sigplots, transcript = row.names(res_dds_timecourse_TC_alpha[res_dds_timecourse_TC_alpha$padj < 0.05,]))

melted_sigplots <- melt(sigplots,id=c("count","cultivar","time","transcript"))
melted_sigplots$L1<- NULL
melted_sigplots$cultivar <- gsub("C311", "Winter", melted_sigplots$cultivar)
melted_sigplots$cultivar <- gsub("S317", "Spring", melted_sigplots$cultivar)
melted_sigplots$padj <- res_dds_timecourse_TC_alpha[melted_sigplots$transcript,]$padj

#NB: You MUST add the adjusted p-values to the sequence names BEFORE substituing "common" names, or it doesn't work
melted_sigplots$transcript <- paste(melted_sigplots$transcript, ", p-value = ", round(res_dds_timecourse_TC_alpha[melted_sigplots$transcript,]$padj,3), sep = "")
melted_sigplots$transcript <- gsub("CarTin_tx_s317_comp26769_c0_seq1", "AP1-like", melted_sigplots$transcript)
melted_sigplots$transcript <- gsub("CarTin_tx_s317_comp33367_c7_seq4", "FLC-like", melted_sigplots$transcript)
melted_sigplots$transcript <- gsub("CarTin_tx_s317_comp32761_c0_seq1", "FT-like", melted_sigplots$transcript)
melted_sigplots$transcript <- gsub("CarTin_tx_s317_comp33519_c0_seq70", "VRN1-like", melted_sigplots$transcript)
melted_sigplots$transcript <- factor(melted_sigplots$transcript, levels = melted_sigplots$transcript)


options(scipen= 100000)


#What you are plotting
p <- ggplot(melted_sigplots, aes(x=time, y=count, color=cultivar, group=cultivar))

#Line of best fit and colour palette
p <- p + stat_smooth(se=FALSE,method="loess",size=1,color="Black") + stat_smooth(se=FALSE,method="loess",size=.5) + scale_colour_manual(values=c("Yellow","#3399FF"))

#Type of chart
p <- p+ geom_point(size=1.5,color="Black") + geom_point(size=1)

#X/Y Axis details First is for "The Big One", second is specific, named transcripts
p <- p + scale_y_log10()
#p <- p + scale_y_log10(breaks = c(1,10,100,1000,10000,100000),limits=c(0.45,100000),labels=c(1,10,100,1000,10000,100000))


#X Axis labels and particulars
p <- p  + xlab("Time Vernalised (days)") + theme(axis.title.x = element_text(size = 12,margin=margin(20,0,0,0)), axis.text.x = element_text(size = 12))

#Y Axis labels and particulars
p <- p + ylab("Aligned Read Counts (normalised)") + theme(axis.title.y = element_text(size = 12,margin=margin(0,20,0,0)), axis.text.y = element_text(size = 12))

#Legends and Panels
p <- p + labs(color = "Cultivar") + theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(color="black"),panel.background = element_blank())

#facets - First is for "The Big One", second is specific, named transcripts
p <- p + facet_wrap(~transcript,ncol=6,scales="free_y") + theme(strip.text.x = element_text(size = 8))
#p <- p + facet_wrap(~transcript,scales="free",ncol=3) + theme(panel.margin = unit(1, "lines"))

#load!
p



sessionInfo()