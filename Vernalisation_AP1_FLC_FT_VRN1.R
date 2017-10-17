library("ggplot2")
library("reshape")



#NB: This one just has the genes that are expressed, does NOT show the zero expression ones
#laptop
DE_both_alt <- read.delim("E:/Data/Dropbox/PhD - Darren/Sequences/DE - 28 days 4C/Vernalisation_methods_AP1_FLC_FT_VRN1_alt.txt")
RNASeq_counts <- read.delim("E:/Data/Dropbox/PhD - Darren/Sequences/DE - 28 days 4C/vernalised_genes_DE_WSCR03_vs_WSCR03_v_normalised_counts.txt")
RTPCR_raw <- read.delim("E:/Data/Dropbox/PhD - Darren/Sequences/DE - 28 days 4C/vernalised_genes_DE_WSCR03_vs_WSCR03_v_RTPCR.txt")


RNASeq_counts_melted <- melt(RNASeq_counts,id=c("Transcript"))

levels(RNASeq_counts_melted$variable)[levels(RNASeq_counts_melted$variable)=="winter.1"] <- "winter"
levels(RNASeq_counts_melted$variable)[levels(RNASeq_counts_melted$variable)=="winter.2"] <- "winter"
levels(RNASeq_counts_melted$variable)[levels(RNASeq_counts_melted$variable)=="winter.3"] <- "winter"

levels(RNASeq_counts_melted$variable)[levels(RNASeq_counts_melted$variable)=="winter_v.1"] <- "winter_v"
levels(RNASeq_counts_melted$variable)[levels(RNASeq_counts_melted$variable)=="winter_v.2"] <- "winter_v"
levels(RNASeq_counts_melted$variable)[levels(RNASeq_counts_melted$variable)=="winter_v.3"] <- "winter_v"

levels(RNASeq_counts_melted$variable)[levels(RNASeq_counts_melted$variable)=="winter"] <- "-"
levels(RNASeq_counts_melted$variable)[levels(RNASeq_counts_melted$variable)=="winter_v"] <- "+"

# RNASeq_counts_melted <- melt(RNASeq_counts,id=c("Transcript"))
# 
# levels(RNASeq_counts_melted$variable)[levels(RNASeq_counts_melted$variable)=="winter.1"] <- "winter"
# levels(RNASeq_counts_melted$variable)[levels(RNASeq_counts_melted$variable)=="winter.2"] <- "winter"
# levels(RNASeq_counts_melted$variable)[levels(RNASeq_counts_melted$variable)=="winter.3"] <- "winter"
# 
# levels(RNASeq_counts_melted$variable)[levels(RNASeq_counts_melted$variable)=="winter_v.1"] <- "winter_v"
# levels(RNASeq_counts_melted$variable)[levels(RNASeq_counts_melted$variable)=="winter_v.2"] <- "winter_v"
# levels(RNASeq_counts_melted$variable)[levels(RNASeq_counts_melted$variable)=="winter_v.3"] <- "winter_v"


t.test(subset(RNASeq_counts_melted,Transcript == "CtAP1-LIKE")$value ~ subset(RNASeq_counts_melted,Transcript == "CtAP1-LIKE")$variable)
t.test(subset(RNASeq_counts_melted,Transcript == "CtMADS1")$value ~ subset(RNASeq_counts_melted,Transcript == "CtMADS1")$variable)
t.test(subset(RNASeq_counts_melted,Transcript == "CtFT-LIKE")$value ~ subset(RNASeq_counts_melted,Transcript == "CtFT-LIKE")$variable)
t.test(subset(RNASeq_counts_melted,Transcript == "CtVRN1-LIKE")$value ~ subset(RNASeq_counts_melted,Transcript == "CtVRN1-LIKE")$variable)



#This one is for the RNASeq data
p <- ggplot(data=RNASeq_counts_melted, aes(x=variable,y=value))

#Type of chart and colour palette
p <- p + geom_jitter(width = 0.3, height = 0.0,size=3,color="#3399FF")
#Error Bars
#X/Y details
#p <- p + scale_y_continuous(expand = c(0,0),breaks = seq(0,6,1),limits=c(0,6.5),labels=seq(0,6,1))

#X axis labels and particulars
p <- p + xlab("Winter Safflower") + theme(axis.text.x = element_text(size=14),axis.title.x=element_text(size=12),axis.ticks.x=element_blank())

#Y axis labels and particulars
p <- p + ylab("Read Counts (Normalised)") + theme(axis.title.y=element_text(size=12,margin=margin(0,20,0,0)), axis.text.y = element_text(size=12))

#Legends and Panels
p <- p + theme(panel.grid.major.y = element_line(color="black"), panel.grid.minor = element_blank(), panel.background = element_blank())

#Legend attributes
p <- p  + theme(legend.text=element_text(size=12), legend.title = element_text(size=12))

#Add both images to a signle chart
p <- p + facet_wrap(~Transcript,scales = "free",nrow=1) + theme(strip.text = element_text(size = 12),panel.margin = unit(1, "lines"))

#load!
p



levels(RTPCR_raw$Vernalised)[levels(RTPCR_raw$Vernalised)=="No"] <- "-"
levels(RTPCR_raw$Vernalised)[levels(RTPCR_raw$Vernalised)=="Yes"] <- "+"


t.test(subset(RTPCR_raw,Gene == "CtAP1-LIKE")$Expression ~ subset(RTPCR_raw,Gene == "CtAP1-LIKE")$Vernalised)
t.test(subset(RTPCR_raw,Gene == "CtMADS1")$Expression ~ subset(RTPCR_raw,Gene == "CtMADS1")$Vernalised)
t.test(subset(RTPCR_raw,Gene == "CtFT-LIKE")$Expression ~ subset(RTPCR_raw,Gene == "CtFT-LIKE")$Vernalised)
t.test(subset(RTPCR_raw,Gene == "CtVRN1-LIKE")$Expression ~ subset(RTPCR_raw,Gene == "CtVRN1-LIKE")$Vernalised)



#This one is for the RT-PCR data
q <- ggplot(data=RTPCR_raw, aes(x=Vernalised,y=Expression))

#Type of chart and colour palette
q <- q + geom_jitter(width = 0.3, height = 0.0, size=3,color="#3399FF")

#Error Bars
#X/Y details
#p <- p + scale_y_continuous(expand = c(0,0),breaks = seq(0,6,1),limits=c(0,6.5),labels=seq(0,6,1))

#X axis labels and particulars
q <- q + xlab("Winter Safflower") + theme(axis.text.x = element_text(size=14),axis.title.x=element_text(size=12),axis.ticks.x=element_blank())

#Y axis labels and particulars
q <- q + ylab("Relative Expression (to CtActin1-LIKE)") + theme(axis.title.y=element_text(size=12,margin=margin(0,20,0,0)), axis.text.y = element_text(size=12))

#Legends and Panels
q <- q + theme(panel.grid.major.y = element_line(color="black"), panel.grid.minor = element_blank(), panel.background = element_blank())

#Legend attributes
q <- q  + theme(legend.text=element_text(size=12), legend.title = element_text(size=12))

#Add both images to a signle chart
q <- q + facet_wrap(~Gene,scales = "free",nrow=1) + theme(strip.text = element_text(size = 12),panel.margin = unit(1, "lines"))

#load!
q






#NB: Everything below here is the old bar graph style that craig didn't like.
#What you are plotting
p <- ggplot(data=DE_both_alt, aes(x=gene,y=value,fill = gene),order = as.character(gene))

#Type of chart and colour palette
p <- p + geom_bar(stat = "identity",position="dodge") + scale_fill_brewer(palette = "Spectral")

#Error Bars
p <- p + geom_errorbar(aes(ymin=value-se,ymax=value+se,x=gene),width=0.1,color="black",)

#X/Y details
#p <- p + scale_y_continuous(expand = c(0,0),breaks = seq(0,6,1),limits=c(0,6.5),labels=seq(0,6,1))

#X axis labels and particulars
p <- p + xlab("") + theme(axis.text.x = element_blank(),axis.title.x=element_blank(),axis.ticks.x = element_blank())

#Y axis labels and particulars
p <- p + ylab("Relative Expression Change") + theme(axis.title.y=element_text(size=12,margin=margin(0,20,0,0)), axis.text.y = element_text(size=12))

#Legends and Panels
p <- p + theme(panel.grid.major.y = element_line(color="black"), panel.grid.minor = element_blank(), panel.background = element_blank())

#Legend attributes
p <- p + labs(fill = "Transcript") + theme(legend.text=element_text(size=12), legend.title = element_text(size=12))

#Add both images to a signle chart
p <- p + facet_grid(method ~ .,scales = "free_y", nrow=1) + theme(strip.text = element_text(size = 12))


#load!
p




#NB: This one includes the zero expression values
#laptop
DE_both_alt <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/DE - 28 days 4C/Vernalisation_methods_AP1_FLC_FT_VRN1_alt.txt")

#What you are plotting
q <- ggplot(data=DE_both_alt, aes(x=vernalised,y=value,fill = gene),order = as.character(gene))

#Type of chart and colour palette
q <- q + geom_bar(stat = "identity",position="dodge") + scale_fill_brewer(palette = "Spectral")

#Error Bars
q <- q + geom_errorbar(aes(ymin=value-se,ymax=value+se,x=vernalised),width=0.1,color="black",)

#X/Y details
#p <- p + scale_y_continuous(expand = c(0,0),breaks = seq(0,6,1),limits=c(0,6.5),labels=seq(0,6,1))

#X axis labels and particulars
q <- q + xlab("Vernalised") + theme(axis.text.x = element_text(size=12),axis.title.x=element_text(size=12))

#Y axis labels and particulars
q <- q + ylab("Relative Expression Change") + theme(axis.title.y=element_text(size=12,margin=margin(0,20,0,0)), axis.text.y = element_text(size=12))

#Legends and Panels
q <- q + theme(panel.grid.major.y = element_line(color="black"), panel.grid.minor = element_blank(), panel.background = element_blank())

#Legend attributes
q <- q + labs(fill = "Transcript") + theme(legend.position = "none") #theme(legend.text=element_text(size=12), legend.title = element_text(size=12))

#Add both images to a signle chart
q <- q + facet_grid(method ~ gene,scales = "free_y") + theme(strip.text = element_text(size = 12))


#load!
q







#NB: Everything from this point on is depreciated!

#Laptop
DE_both <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/DE - 28 days 4C/Vernalisation_methods_AP1_FLC_FT_VRN1.txt")
melted_DE_both <- melt(DE_both,id="method")
melted_DE_both$variable <- gsub("[.]", "-", melted_DE_both$variable)

#What you are plotting
q <- ggplot(subset(melted_DE_both, method %in% "qPCR"), aes(method,value,fill = variable),order = as.character(variable))

#Type of chart and colour palette
q <- q + geom_bar(stat = "identity",position="dodge") + scale_fill_brewer(palette = "Spectral")

#X/Y details
q <- q + scale_y_continuous(expand = c(0,0),breaks = seq(0,0.6,0.1),limits=c(0,0.6),labels=seq(0,0.6,0.1))

#X axis labels and particulars
q <- q + xlab("") + theme(axis.text.x = element_text(size=12),axis.title.x=element_text(size=12))

#Y axis labels and particulars
q <- q + ylab("Relative Expression Change") + theme(axis.title.y=element_text(size=12,margin=margin(0,20,0,0)))

#Legends and Panels
q <- q + theme(panel.grid.major.y = element_line(color="black"), panel.grid.minor = element_blank(), panel.background = element_blank())

#Reverse legend order
q <- q + labs(fill = "Transcript")

#load!
q



#What you are plotting
r <- ggplot(subset(melted_DE_both, method %in% "RNASeq"), aes(method,value,fill = variable),order = as.character(variable))

#Type of chart and colour palette
r <- r + geom_bar(stat = "identity",position="dodge") + scale_fill_brewer(palette = "Spectral")

#X/Y details
r <- r + scale_y_continuous(expand = c(0,0),breaks = seq(0,6,1),limits=c(0,6),labels=seq(0,6,1))

#X axis labels and particulars
r <- r + xlab("") + theme(axis.text.x = element_text(size=12),axis.title.x=element_text(size=12))

#Y axis labels and particulars
r <- r + ylab("Relative Expression Change (log2)") + theme(axis.title.y=element_text(size=12,margin=margin(0,20,0,0)))

#Legends and Panels
r <- r + theme(panel.grid.major.y = element_line(color="black"), panel.grid.minor = element_blank(), panel.background = element_blank())

#Reverse legend order
r <- r + labs(fill = "Transcript")

#load!
r
