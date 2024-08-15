setwd("G:/NJ+10个样地/写论文/MDPI/校稿/Code/Figure 3")
KO_at <- read.table("selected_KO.txt", header=T, sep="\t")
QL <- read.csv("KEGG_KO.csv", header=T, sep=",")
name <- levels(as.factor(KO_at$function.))
name
for (i in 1:21) {
  c <- i
  z <- name[c]
  kO_A <- KO_at[KO_at$function. == z,]
  kO_B <- kO_A[,2]
  Z <- kO_B
  nrow_v<-length(Z)
  a <- NULL
  b <- c()
  at <- c()
  
  for(i in 1:nrow_v){
    a <- Z[i]
    #数据筛选
    b <- QL[QL$function. == a,]
    at <- rbind(at,b)
  }
  
  write.table(at,paste0(c,"-",z,".csv"),sep=",",row.names = F)
}



