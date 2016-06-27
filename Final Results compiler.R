
rm(list=ls())

user=(Sys.info()[6])
Desktop=paste("C:/Users/",user,"/Desktop/",sep="")
setwd(Desktop)


home=paste(Desktop,"MEMS/S4/R Programming/German/",sep="")

setwd(paste(home,'Model',sep=''))

preproc_dir=paste(home,'Model',"/","preproc_data",sep='')

candidates_val=paste(home,'Model',"/","candidates_val",sep='')

candidates_test=paste(home,'Model',"/","candidates_test",sep='')
	
dir.create("ensemble_results")

ensemble_results=paste(home,'Model',"/","ensemble_results",sep='')






library(verification)





## AUC
auc = function (obs, pred)
{
  out = array()
  for(i in 1:ncol(pred)){
    out[i] = (roc.area(as.numeric(obs), as.numeric(pred[,i]))$A)
    }
  out
}



### Brier
brier = function (obs, pred)
{
  out = array()
  for(i in 1:ncol(pred)){
    out[i] = (brier(as.numeric(obs), as.numeric(pred[,i]))$bs)
    }
  out
}



setwd(candidates_val)


ensembleSource_val = "all_val.csv"

ensembleSource=read.csv(ensembleSource_val,as.is=T)
ensembleGrid=ensembleSource[,-1]
options = ncol(ensembleGrid) - 1  # FIX
workingGrid = as.matrix(ensembleGrid[,2:(options + 1)])


score_diagonal = diag(options)
scoreVector = workingGrid %*% score_diagonal
scores = auc(ensembleGrid[,1],scoreVector)
rm(score_diagonal)


# Ranking Candidates
ranked=cbind(col=colnames(ensembleGrid[,2:(options + 1)]),score=scores)

ranked=as.data.frame(ranked)

ranked$score=as.character(ranked$score)

ranked$score=as.numeric(ranked$score)
ranked=ranked[order(ranked[,2]),]

ranked=cbind(rank=c(1:nrow(ranked)),ranked)

ranked$col=as.character(ranked$col)

a=c(head(ranked[grep('rf',ranked$col),2],1),head(ranked[grep('lr',ranked$col),2],1),head(ranked[grep('ann',ranked$col),2],1))




###For test Sample
setwd(candidates_test)


ensembleSource_test = "all_test.csv"


ensembleSource=read.csv(ensembleSource_test,as.is=T)
ensembleGrid=ensembleSource[,-1]
options = ncol(ensembleGrid) - 1  # FIX
workingGrid = as.matrix(ensembleGrid[,2:(options + 1)])


score_diagonal = diag(options)
scoreVector = workingGrid %*% score_diagonal
scores = auc(ensembleGrid[,1],scoreVector)
rm(score_diagonal)


# Ranking Candidates
ranked=cbind(col=colnames(ensembleGrid[,2:(options + 1)]),score=scores)

ranked=as.data.frame(ranked)

ranked$score=as.character(ranked$score)

ranked$score=as.numeric(ranked$score)
ranked=ranked[order(ranked[,2]),]

ranked=cbind(rank=c(1:nrow(ranked)),ranked)

ranked$col=as.character(ranked$col)



print(paste ("AUC for ",ds[i,"name"]))
a=gsub('val','test',a)

print(ranked[ranked$col==a,c(2,3)])




