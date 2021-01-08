data <- read.csv("phyla.csv", header = TRUE, stringsAsFactors = FALSE,sep=",")






a=data.matrix(data[364:450,1:786], rownames.force = NA)

corr_mat=cor(t(a),method="spearman",use = "complete.obs")




corr_sum<-rowSums(corr_mat)
corr_sum_sorted<-sort(corr_sum,decreasing=TRUE)

corr_mat_sorted<-corr_mat[order(corr_sum, decreasing = TRUE),order(corr_sum, decreasing = TRUE)]
corr_mat_sorted_sum<-rowSums(corr_mat_sorted)


library("lattice")
library(viridisLite)
coul <- viridis(30)

# plot it flipping the axis
levelplot( t(corr_mat[c(nrow(corr_mat):1) , ]), scales=list(x=list(rot=90)),
           col.regions = coul,xlab = "OTU", ylab = "OTU")




levelplot( t(corr_mat_sorted[c(nrow(corr_mat_sorted):1) , ]), scales=list(x=list(rot=90)),
           col.regions = coul,xlab = "OTU", ylab = "OTU")
