emm.11 = read.table("https://raw.githubusercontent.com/taylahj1/SNP_Distances/main/mash_distances/emm11_distances_table.txt")
emm.11.names = emm.11[,1]
emm.11 = emm.11[,-1]
diag(emm.11)
w1 = which(duplicated(emm.11.names))

emm.11.names = emm.11.names[-w1]
emm.11 = emm.11[-w1,]
emm.11 = emm.11[,-w1]

colnames(emm.11) = emm.11.names
rownames(emm.11) = emm.11.names
emm.11


emm.11.mean = NULL
for(i in 1:(nrow(emm.11) - 1)){
  for(j in (i + 1):ncol(emm.11)){
    emm.11.mean = c(emm.11.mean, emm.11[i,j])
  }
}

mean(emm.11.mean)
mu.11 = sum(emm.11)/(nrow(emm.11) * (ncol(emm.11) - 1))


emm.77 = read.table("https://raw.githubusercontent.com/taylahj1/SNP_Distances/main/mash_distances/emm77_distances_table.txt")
emm.77.names = emm.77[,1]
emm.77 = emm.77[,-1]
w2 = which(duplicated(emm.77.names))

emm.77.names = emm.77.names[-w2]
emm.77 = emm.77[-w2,]
emm.77 = emm.77[,-w2]

colnames(emm.77) = emm.77.names
rownames(emm.77) = emm.77.names

mu.77 = sum(emm.77)/(nrow(emm.77) * (ncol(emm.77) - 1))


emm.11.77 = read.table("https://raw.githubusercontent.com/taylahj1/SNP_Distances/main/mash_distances/emm11vemm77_distances_table.txt")
emm.11.77.names = emm.11.77[,1]
emm.11.77 = emm.11.77[,-1]

w3 = which(duplicated(emm.11.77.names))

emm.11.77.names = emm.11.77.names[-w3]
emm.77 = emm.77[-w2,]
emm.77 = emm.77[,-w1]

mu.11.77 = sum(emm.11.77)/(nrow(emm.77) * ncol(emm.77))



r1 = readLines("https://github.com/taylahj1/SNP_Distances/tree/main/mash_distances")



"https://raw.githubusercontent.com/taylahj1/SNP_Distances/main/mash_distances/emm1vemm11_distances_table.txt"























