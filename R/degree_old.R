library(dplyr)
library(tidyr)
library(ggplot2)
library(arsenal)

#' Download and create node-level dataset.
#' @param redo If FALSE, will not redownload and recreate the data if
#' data already exists on disk. If TRUE, it will.
degree <- function(redo=FALSE) {
    
  rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
  load(paste(rootdir,"data/Nishi_etal_GINI/node.Rdata",sep="/"))
  load(paste(rootdir,"data/Nishi_etal_GINI/link.Rdata",sep="/"))
  cdata = read.csv(paste(rootdir,"data/cdata3_0531.csv",sep="/"))
  
  datafile <- paste(rootdir,"data/degree.Rda", sep="/")
  
  if (!redo && file.exists(datafile)) {
    cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
    cat("Call countyData(redo=TRUE) to update data.\n")
    load(datafile)
    
    return(list(sample,sample.long))
  }
  
  ndata$ID = paste(ndata$id,ndata$gameID,sep="_")
  ldata$ID = paste(ldata$id1,ldata$gameID,sep="_")
  
  n = 1
  degree=NULL
  round=NULL
  ID=NULL
  
  for(i in unique(ldata$ID)){
    for(m in c(0:10)){
      degree[n] = nrow(ldata[ldata$round==m & ldata$ID==i,])
      round[n] = m
      ID[n] = i
      n =  n+1
      }
  }
  
  sample = data.frame(ID, round, degree)
  
  sample = stats::reshape(data=sample,
                          v.names=c('degree'),
                          timevar='round',
                          idvar='ID',
                          direction='wide')
  
  #extract gameID from ID
  sample$gameID = sub("^[^_]*_", "", sample$ID)
  
  meanInitialDegree = NULL
  gameID = NULL
  n=1
  for(i in unique(sample$gameID)){
      meanInitialDegree[n] = mean(sample[sample$gameID==i,]$degree.0)
      gameID[n] = i
      n =  n+1
  }
  
  meanDegree = data.frame(gameID, meanInitialDegree)
  
  sample = merge(sample,meanDegree,by="gameID",all.x=TRUE)
  
  #normalize (first within each gameID, within each round)
  #https://towardsdatascience.com/transforming-skewed-data-73da4c2d0d16
  #https://www.cs.unm.edu/~mueen/DTW.pdf
  normalizeDegree = function(t) BBmisc::normalize(log(t+1))
  
  sample.zdegree <- sample[,grepl("degree", names(sample))] %>% 
    group_by(sample$gameID) %>% 
    summarize_all(funs(normalizeDegree))
  sample.zdegree <- data.frame(sample.zdegree)
  
  colnames(sample.zdegree) <- gsub("degree", "zdegree", colnames(sample.zdegree))
  sample.zdegree <- subset(sample.zdegree,select=-c(sample.gameID))
  
  sample = cbind(sample,sample.zdegree)
  
  #merging behavior variable
  sample = merge(sample,ndata[ndata$round==1,][c("behavior","ID")],all.x=TRUE)
  sample$behavior = ifelse(sample$behavior=="",NA,sample$behavior)
  
  #merging connecting variable
  makeLinkRate = function(x) sum(prop.table(table(x))[c("makeLink","notBreakLink")])
  cdata$gameID <- as.factor(cdata$gameID)
  df.connecting <- cdata[cdata$round==1,]["connecting"] %>% 
    group_by(cdata[cdata$round==1,]$gameID) %>% 
    summarize_all(funs(makeLinkRate))
  df.connecting <- data.frame(df.connecting)
  colnames(df.connecting)[1] = "gameID"
  sample = merge(sample,df.connecting,by="gameID",all.x=TRUE)
  
  #remove rows containing NAs
  sample <- sample[complete.cases(sample),]
  
  #convert to long format
  sample.long <- gather(sample[c("ID","behavior","connecting",
                               "zdegree.0","zdegree.1",
                               "zdegree.2","zdegree.3",
                               "zdegree.4","zdegree.5",
                               "zdegree.6","zdegree.7",
                               "zdegree.8","zdegree.9","zdegree.10")],
                      round, zdegree,
                      zdegree.0:zdegree.10, factor_key=TRUE)
  
  sample.long$round <- as.numeric(mapply(sample.long$round, FUN=function(t) gsub("zdegree.","",x=t)))
  
  sample.long <- sample.long %>%
    mutate(behavior = 
             case_when(
               behavior=="C" ~ "Initial cooperator",
               behavior=="D" ~ "Initial defector"
             ))
  
  save(sample, file=datafile)
  save(sample.long, file=datafile)
  
  return(list(sample,sample.long))

}




