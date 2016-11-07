##create the original dataframe
dataset <- read.csv(file.choose(),header=T)
head(dataset)
gene <- data.frame()
a = which(colnames(dataset)=="gene_1" )
b = which(colnames(dataset)=="gene_186" )
c = which(colnames(dataset)=="peid" )
gene <- cbind(dataset[70:255])


##elimate duplicate
undup <- gene[!duplicated(gene[1:186]),]
dim(undup)[1]

##change all numeric to dummy 
finalgene = ifelse(undup == 0,0,1)
head(finalgene)

##distance function
m<-finalgene[1,]
n<-finalgene[2,]

dis<-function(x,y){
  count = 0 ;
  for(i in 1:length(x)){
    if(x[i]!=y[i]){
         count <- count+1
        } 
    }
  return(count)
  
  }

##calculate all distances
dis(finalgene[1,],finalgene[2,])
sumofdis <- vector(mode="numeric")

for(k in 1:(dim(finalgene)[1]-2)){
  for(j in (k+1):(dim(finalgene)[1]-1)){
  distance =  dis(finalgene[k,],finalgene[j,])
  sumofdis<-c(sumofdis,distance)
  }
}


##histogram
hist(sumofdis)
