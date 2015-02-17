require(stringr)
require(plyr)

data = read.csv("../res/errors.txt",header=F,sep=" ",as.is=T)
str(data)

saxvsm=data[data$V1=="SAXVSMContinuousDirectSampler",]
saxvsmg=data[data$V1=="SAXVSMContinuousGrammarSampler",]

saxvsm = saxvsm[ order(saxvsm$V2), -1]

write.table(saxvsm,"../res/errors_sax_vsm.txt",row.names=F,col.names=F)
