require(dplyr)
require(PMCMR)
dat=read.csv("../results/sax-vsm-eval0.csv")
names(dat)

results = matrix(
           unlist(select(dat,ED,DTWB,SAX.VSM,FS,PS,SAX.VSM.G)),
           ncol=6,
           byrow=F,
           dimnames = list(dat$Dataset,
            c("Euclidean", "DTWB", "SAX-VSM", "FS", 
              "PS","SAX-VSM-G"))
           )
str(results)
groups <- gl(6,46,labels=colnames(results))
boxplot(as.vector(results) ~ groups)

friedman.test(results)

posthoc.friedman.nemenyi.test(results)

dat=read.csv("../results/sax-vsm-eval1.csv")
names(dat)

results = matrix(
  unlist(select(dat,ED,DTWB,SAX.VSM,FS,PS,SAX.VSM.G,
                LS,TSBF)),
  ncol=8,
  byrow=F,
  dimnames = list(dat$Dataset,
                  c("Euclidean", "DTWB", "SAX-VSM", "FS", 
                    "PS","SAX-VSM-G","SL","TSBF"))
)
str(results)
groups <- gl(8,42,labels=colnames(results))
boxplot(as.vector(results) ~ groups)

friedman.test(results)

posthoc.friedman.nemenyi.test(results)
