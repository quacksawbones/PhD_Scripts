library(pastecs)
library(ggplot2)
library(reshape)


#Import - Laptop
tissues_count_matrix <- read.delim("C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/De Novo transcriptome/CV/Tissues_count_matrix.txt")

tissues_count_matrix$X <- factor(tissues_count_matrix$X, levels = tissues_count_matrix$X)

names(tissues_count_matrix) <- gsub("\\.", "\n", names(tissues_count_matrix))
names(tissues_count_matrix) <- gsub("_", " ", names(tissues_count_matrix))


# levels(tissues_count_matrix$X) <- gsub(" ", "\n", levels(tissues_count_matrix$X))
# levels(tissues_count_matrix$X) <- gsub("_", " ", levels(tissues_count_matrix$X))

#replace any read with less than 10 counts with zero
tissues_count_matrix[ , 2:16 ][ tissues_count_matrix[ , 2:16 ] < 10 ] <- 0

tissues_count_matrix_nonzero <- tissues_count_matrix
tissues_count_matrix_nonzero[ , 2:16 ][ tissues_count_matrix[ , 2:16 ] < 10 ] <- NA

options(scipen=100)
options(digits=6)


stats_tissues <- stat.desc(tissues_count_matrix[,2:16])
stats_tissues_nonzero <- stat.desc(tissues_count_matrix_nonzero[,2:16])


Matrix_of_CV <- abs(outer(t(stats_tissues[14,]), t(stats_tissues[14,]), '-' ))
Matrix_of_CV_nonzero <- abs(outer(t(stats_tissues_nonzero[14,]), t(stats_tissues_nonzero[14,]), '-' ))

melted_matrix_of_cv <- melt(Matrix_of_CV)
melted_matrix_of_cv <- melt(Matrix_of_CV_nonzero)
melted_matrix_of_cv$X2 <- NULL
melted_matrix_of_cv$X4 <- NULL

melted_matrix_of_cv$X1 <- factor(melted_matrix_of_cv$X1, levels = melted_matrix_of_cv$X1)
melted_matrix_of_cv$X3 <- factor(melted_matrix_of_cv$X3, levels = melted_matrix_of_cv$X3)




p <- ggplot(melted_matrix_of_cv, aes(X3, X1))
p <- p + geom_tile(aes(fill = value), colour = "white") + geom_text(aes(fill = value, label = round(value, 1)))
p <- p + scale_fill_gradient(low = "white", high = "red",expand=c(0,0))
p <- p + theme(axis.text.x = element_text(angle = 90,size=10,vjust=.4), legend.position="none", axis.title=element_blank(),panel.background = element_blank())
p



write.csv(Matrix_of_CV, "C:/Users/cul07b/Dropbox/PhD - Darren/Sequences/De Novo transcriptome/CV/Matrix_of_CV.csv")

