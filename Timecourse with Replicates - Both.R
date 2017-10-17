library("ggplot2")
library("reshape")


TC_RNASeq <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/DE - Timecourse/Timecourse with Replicates - RNASeq_mod.txt")
TC_RNASeq <- read.delim("E:/Data/Dropbox/PhD - Darren/Sequences/DE - Timecourse/Timecourse with Replicates - RNASeq_mod_new.txt",header = TRUE,fileEncoding="UTF-16")


colnames(TC_RNASeq)[3] <- "CtFT-Like"
colnames(TC_RNASeq)[4] <- "CtMADS1"

#T-tests for each transcript at each time point
t.test(subset(TC_RNASeq,Time == 0)$`CtFT-Like` ~ subset(TC_RNASeq,Time == 0)$Cultivar)
t.test(subset(TC_RNASeq,Time == 5)$`CtFT-Like` ~ subset(TC_RNASeq,Time == 5)$Cultivar)
t.test(subset(TC_RNASeq,Time == 10)$`CtFT-Like` ~ subset(TC_RNASeq,Time == 10)$Cultivar)
t.test(subset(TC_RNASeq,Time == 15)$`CtFT-Like` ~ subset(TC_RNASeq,Time == 15)$Cultivar)
t.test(subset(TC_RNASeq,Time == 20)$`CtFT-Like` ~ subset(TC_RNASeq,Time == 20)$Cultivar)

t.test(subset(TC_RNASeq,Time == 0)$`CtMADS1` ~ subset(TC_RNASeq,Time == 0)$Cultivar)
t.test(subset(TC_RNASeq,Time == 5)$`CtMADS1` ~ subset(TC_RNASeq,Time == 5)$Cultivar)
t.test(subset(TC_RNASeq,Time == 10)$`CtMADS1` ~ subset(TC_RNASeq,Time == 10)$Cultivar)
t.test(subset(TC_RNASeq,Time == 15)$`CtMADS1` ~ subset(TC_RNASeq,Time == 15)$Cultivar)
t.test(subset(TC_RNASeq,Time == 20)$`CtMADS1` ~ subset(TC_RNASeq,Time == 20)$Cultivar)


TC_RNASeq_melted <- melt(TC_RNASeq,id=c("Cultivar","Time"))


p <- ggplot(data=TC_RNASeq_melted, aes(x=Time,y=value,color=Cultivar))

p <- p + geom_point(color="black",size=4) + geom_point(size=3) + scale_color_manual(values=c("yellow", "#3399FF"))

p <- p + ylab("Read Counts (Normalised)") + xlab("Days Vernalised")

p <- p + theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),legend.text=element_text(size=12),panel.grid.major.y = element_line(color = "black",linetype = "solid"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),legend.title=element_text(size=14,face="bold"),panel.background = element_blank())

p <- p + facet_wrap( ~ variable,scales = "free") + theme(strip.text = element_text(size = 12))

p

t.test(TC_RNASeq$`CtFT-Like`[TC_RNASeq$Time == 20],TC_RNASeq$Cultivar[TC_RNASeq$Time == 20])



TC_RTPCR <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/DE - Timecourse/Timecourse with Replicates - RTPCR_mod_with_SE.txt")
TC_RTPCR <- read.delim("E:/Data/Dropbox/PhD - Darren/Sequences/DE - Timecourse/Timecourse with Replicates - RTPCR_mod_with_SE.txt")

TC_RTPCR$Gene <- gsub('_', '-', TC_RTPCR$Gene)


#T-tests for each transcript at each time point, RT-PCR
t.test(subset(TC_RTPCR,Gene == "CtFT-Like")$`Expression`[TC_RTPCR$Time == 0] ~ subset(TC_RTPCR,Gene == "CtFT-Like")$Cultivar[TC_RTPCR$Time == 0])
t.test(subset(TC_RTPCR,Gene == "CtFT-Like")$`Expression`[TC_RTPCR$Time == 5] ~ subset(TC_RTPCR,Gene == "CtFT-Like")$Cultivar[TC_RTPCR$Time == 5])
t.test(subset(TC_RTPCR,Gene == "CtFT-Like")$`Expression`[TC_RTPCR$Time == 10] ~ subset(TC_RTPCR,Gene == "CtFT-Like")$Cultivar[TC_RTPCR$Time == 10])
t.test(subset(TC_RTPCR,Gene == "CtFT-Like")$`Expression`[TC_RTPCR$Time == 15] ~ subset(TC_RTPCR,Gene == "CtFT-Like")$Cultivar[TC_RTPCR$Time == 15])
t.test(subset(TC_RTPCR,Gene == "CtFT-Like")$`Expression`[TC_RTPCR$Time == 20] ~ subset(TC_RTPCR,Gene == "CtFT-Like")$Cultivar[TC_RTPCR$Time == 20])

t.test(subset(TC_RTPCR,Gene == "CtMADS1")$`Expression`[TC_RTPCR$Time == 0] ~ subset(TC_RTPCR,Gene == "CtMADS1")$Cultivar[TC_RTPCR$Time == 0])
t.test(subset(TC_RTPCR,Gene == "CtMADS1")$`Expression`[TC_RTPCR$Time == 5] ~ subset(TC_RTPCR,Gene == "CtMADS1")$Cultivar[TC_RTPCR$Time == 5])
t.test(subset(TC_RTPCR,Gene == "CtMADS1")$`Expression`[TC_RTPCR$Time == 10] ~ subset(TC_RTPCR,Gene == "CtMADS1")$Cultivar[TC_RTPCR$Time == 10])
t.test(subset(TC_RTPCR,Gene == "CtMADS1")$`Expression`[TC_RTPCR$Time == 15] ~ subset(TC_RTPCR,Gene == "CtMADS1")$Cultivar[TC_RTPCR$Time == 15])
t.test(subset(TC_RTPCR,Gene == "CtMADS1")$`Expression`[TC_RTPCR$Time == 20] ~ subset(TC_RTPCR,Gene == "CtMADS1")$Cultivar[TC_RTPCR$Time == 20])


r <- ggplot(data=TC_RTPCR, aes(x=Time,y=Expression,color=Cultivar))

r <- r + geom_errorbar(aes(ymax = Expression + Error, ymin=Expression - Error),color="black",width=2) + geom_point(color="black",size=4) + geom_point(size=3) + scale_color_manual(values=c("yellow", "#3399FF"))

r <- r + ylab("Relative Expresstion (to CtActin-Like)") + xlab("Days Vernalised")

r <- r + theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),legend.text=element_text(size=12),panel.grid.major.y = element_line(color = "black",linetype = "solid"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),legend.title=element_text(size=14,face="bold"),panel.background = element_blank())

r <- r + facet_wrap( ~ Gene,scales = "free") + theme(strip.text = element_text(size = 12))

r


#NB: This one just has the genes that are expressed, does NOT show the zero expression ones
#laptop
DE_both_alt <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/DE - Timecourse/Timecourse with Replicates - Both.txt")

#What you are plotting (first just shows qPCR, second shows both)
#p <- ggplot(data=DE_both_alt[DE_both_alt$method == "qPCR"], aes(x=time,y=exp,fill = cultivar))
p <- ggplot(data=DE_both_alt, aes(x=time,y=exp,fill = cultivar))

#Type of chart and colour palette
p <- p + geom_bar(stat = "identity",position=position_dodge(5),color="black") + scale_fill_manual(values=c("yellow","#3399FF"))

#Error Bars
p <- p + geom_errorbar(aes(ymin=exp-SE,ymax=exp+SE,x=time),width=0.2,color="black",position = position_dodge(5))

#X/Y details
#p <- p + scale_y_continuous(expand = c(0,0),breaks = seq(0,6,1),limits=c(0,6.5),labels=seq(0,6,1))

#X axis labels and particulars
p <- p + xlab("Days Vernalised") + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12))

#Y axis labels and particulars
p <- p + ylab("Expression") + theme(axis.title.y=element_text(size=12,margin=margin(0,20,0,0)), axis.text.y = element_text(size=12))

#Legends and Panels
p <- p + theme(panel.grid.major.y = element_line(color="black"), panel.grid.minor = element_blank(), panel.background = element_blank())

#Legend attributes
p <- p + labs(fill = "Cultivar") + theme(legend.text=element_text(size=12), legend.title = element_text(size=12))

#Add both images to a signle chart
p <- p + facet_grid(method ~ gene,scales = "free_y") + theme(strip.text = element_text(size = 12))


#load!
p



#NB: This one just has the genes that are expressed, does NOT show the zero expression ones
#laptop
DE_actin <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/DE - Timecourse/Timecourse with Replicates - Actin.txt")

#What you are plotting
q <- ggplot(data=DE_actin, aes(x=time,y=exp,fill = cultivar))

#Type of chart and colour palette
q <- q + geom_bar(stat = "identity",position=position_dodge(5),color="black") + scale_fill_manual(values=c("yellow","#3399FF"))

#Error Bars
q <- q + geom_errorbar(aes(ymin=exp-SE,ymax=exp+SE,x=time),width=0.2,color="black",position = position_dodge(5))


#X axis labels and particulars
q <- q + xlab("Days Vernalised") + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12))

#Y axis labels and particulars
q <- q + ylab("Aligned Reads (normalised)") + theme(axis.title.y=element_text(size=12,margin=margin(0,20,0,0)), axis.text.y = element_text(size=12))

#Legends and Panels
q <- q + theme(panel.grid.major.y = element_line(color="black"), panel.grid.minor = element_blank(), panel.background = element_blank())

#Legend attributes
q <- q + labs(fill = "Cultivar") + theme(legend.text=element_text(size=12), legend.title = element_text(size=12))


#load!
q


