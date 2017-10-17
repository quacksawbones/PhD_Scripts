library("ggplot2")
library("rtracklayer")
library("dplyr")

TG1_bed <- read.delim("H:/Safflower/smallRNA/shortstack/ShortStack_safflower_WT_trimmed/Saff-w-t_S1_R1_001_trimmomatic.sorted.bed", header=FALSE, dec=",")
TG2_bed <- read.delim("H:/Safflower/smallRNA/shortstack/ShortStack_safflower_TG_trimmed/Saff-TG_S2_R1_001_trimmomatic.sorted.bed", header=FALSE, dec=",")


TG1_CTFAD2_1 <- TG1_bed[TG1_bed$V1 == "CTFAD2_1",c("V4","V13","V14")]
TG2_CTFAD2_1 <- TG2_bed[TG2_bed$V1 == "CTFAD2_1",c("V4","V13","V14")]
#TG2_CTFAD2_1$V14 <- -abs(TG2_CTFAD2_1$V14)

TG1_CTFAD2_2 <- TG1_bed[TG1_bed$V1 == "CTFAD2_2",c("V4","V13","V14")]
TG2_CTFAD2_2 <- TG2_bed[TG2_bed$V1 == "CTFAD2_2",c("V4","V13","V14")]
#TG2_CTFAD2_2$V14 <- -abs(TG2_CTFAD2_2$V14)

TG1_CTFATB_T12 <- TG1_bed[TG1_bed$V1 == "ctFatB_T12",c("V4","V13","V14")]
TG2_CTFATB_T12 <- TG2_bed[TG2_bed$V1 == "ctFatB_T12",c("V4","V13","V14")]


p_CTFAD2_1 <- ggplot(TG1_CTFAD2_1, aes(V13,V14)) + geom_line(color="blue") + geom_line(data=TG2_CTFAD2_1, aes(V13,V14),color="red")
p_CTFAD2_1 <- p_CTFAD2_1 + xlab("Position (bp)") + ylab("Count") + ggtitle("FAD2.1") + scale_x_continuous(expand = c(0, 0), limit = c(0,nrow(TG1_CTFAD2_1)), breaks = seq(0, nrow(TG1_CTFAD2_1), by=200)) + scale_y_continuous(expand = c(0,0), limit = c(0,400), breaks = seq(0, 400, by=50))
p_CTFAD2_1 <- p_CTFAD2_1 + theme(axis.title.y=element_text(vjust=1), plot.title = element_text(vjust = 1.5, size=16, face="bold"), panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),axis.text.x = element_text(size=9),panel.grid.major.y = element_line(color = "black",linetype = "dashed"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),panel.background = element_blank())
p_CTFAD2_1


p_CTFAD2_2 <- ggplot(TG1_CTFAD2_2, aes(V13,V14)) + geom_line(color="blue") + geom_line(data=TG2_CTFAD2_2, aes(V13,V14),color="red")
p_CTFAD2_2 <- p_CTFAD2_2 + xlab("Position (bp)") + ylab("Count") + ggtitle("FAD2.2") + scale_x_continuous(expand = c(0, 0), limit = c(0,nrow(TG1_CTFAD2_2)), breaks = seq(0, nrow(TG1_CTFAD2_2), by=200)) + scale_y_continuous(expand = c(0,0), limit = c(0,12000), breaks = seq(0, 12000, by=2000))
p_CTFAD2_2 <- p_CTFAD2_2 + theme(axis.title.y=element_text(vjust=1), plot.title = element_text(vjust = 1.5, size=16, face="bold"), panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),axis.text.x = element_text(size=9),panel.grid.major.y = element_line(color = "black",linetype = "dashed"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),panel.background = element_blank())
p_CTFAD2_2

p_CTFATB <- ggplot(TG1_CTFATB_T12, aes(V13,V14)) + geom_line(color="blue") + geom_line(data=TG2_CTFATB_T12, aes(V13,V14),color="red")
p_CTFATB <- p_CTFATB + xlab("Position (bp)") + ylab("Count") + ggtitle("FATB_T12") + scale_x_continuous(expand = c(0, 0), limit = c(0,nrow(TG1_CTFATB_T12)), breaks = seq(0, nrow(TG1_CTFATB_T12), by=200)) + scale_y_continuous(expand = c(0,0), limit = c(0,6000), breaks = seq(0, 6000, by=1000))
p_CTFATB <- p_CTFATB + theme(axis.title.y=element_text(vjust=1), plot.title = element_text(vjust = 1.5, size=16, face="bold"), panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),axis.text.x = element_text(size=9),panel.grid.major.y = element_line(color = "black",linetype = "dashed"),panel.grid.minor.y = element_blank(),axis.title=element_text(size=14,face="bold"),panel.background = element_blank())
p_CTFATB
