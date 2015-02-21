require(stringr)
require(plyr)

datasets=read.csv("../res/datasets.txt",header=F,sep=" ",as.is=T)
names(datasets)=c("dataset","classes","train_size","test_size","length","error_euclidean","error_dtw")

tmp = read.csv("../res/errors_sax_vsm.txt",header=F,sep=" ",as.is=T)
names(tmp)=c("dataset","classic","exact","nored")
sax_vsm = data.frame(dataset=tmp$dataset, error=apply(tmp[,2:4],1,min))

tmp2 = read.csv("../res/errors_sax_vsm-g.txt",header=F,sep=" ",as.is=T)
names(tmp2)=c("dataset","classic","exact","nored")
sax_vsm_g = data.frame(dataset=tmp2$dataset, error=apply(tmp2[,2:4],1,min))

data=merge(datasets,sax_vsm,by.x=c("dataset"),by.y=c("dataset"),all=T)
data=merge(data,sax_vsm_g,by.x=c("dataset"),by.y=c("dataset"),all=T)

write.table(data,"../res/summary.txt",row.names=F,col.names=T)
