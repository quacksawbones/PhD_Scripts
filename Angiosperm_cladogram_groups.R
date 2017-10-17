library("ape")
#tr<-read.tree(text = "(((Brassicales,Fabales),((Asterales,Gentianales),Caryophyllales)),Poales);")
tr<-read.tree("E:/Data/Dropbox/PhD - Darren/Images/Diagrams for Documents/Evolution of Vernalisation/Angiosperm Phylogenetic Tree.nwk")
tr<-read.tree("C:/Users/cul07b/Dropbox/PhD - Darren/Images/Diagrams for Documents/Evolution of Vernalisation/Angiosperm Phylogenetic Tree.nwk")

tr$tip.label[tr$tip.label == "Brassicales"] <- "Brassicales\n(Arabidopsis)"
tr$tip.label[tr$tip.label == "Fabales"] <- "Fabales\n(Legumes)"
tr$tip.label[tr$tip.label == "Asterales"] <- "Asterales\n(Safflower)"
tr$tip.label[tr$tip.label == "Gentianales"] <- "Gentianales\n(Eustoma spp.)"
tr$tip.label[tr$tip.label == "Caryophyllales"] <- "Caryophyllales\n(Sugar Beet)"
tr$tip.label[tr$tip.label == "Poales"] <- "Poales\n(Cereals)"

par(mar=c(2, 4, 4, 12), xpd=TRUE)

plot(tr,label.offset = 2.5,type="p",edge.width=2,show.node.label = TRUE)
tipcol<-c("white","white","black","black","black","light gray")
tiplabels(pch=21, col="black", adj=-2.5, bg=tipcol, cex=2,)

legend(6,1.4,"E/O Boundary\n(34 mya)",col=c("gray"),box.col="white",pt.bg="gray",pch="-",pt.cex=1.5)
legend(6,2.85,c("Monocots","Asterids","Rosids"),col=c("black"),box.col="white",pt.bg=c("light gray","black","white"),pch=21,pt.cex=1.5,y.intersp=1.5,)

#This part is so you can visualise the E/O boundary at 34 Ma
lines(x=c(166,166),y=c(0.75,6.3),lwd=2,col="grey",lty=2)

