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
  
  ndata[is.na(ndata)] <- "NA"
  ldata[is.na(ldata)] <- "NA"
  cdata[is.na(cdata)] <- "NA"
  
  ndata$ID = paste(ndata$id,ndata$gameID,sep="_")
  ldata$ID = paste(ldata$id1,ldata$gameID,sep="_")
  cdata$ID = paste(paste("p",cdata$ego_id %% 100,sep=""),cdata$gameID,sep="_")
  cdata$alterID = paste(paste("p",cdata$alter_id %% 100,sep=""),cdata$gameID,sep="_")
  
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

  #merging visibility, initial wealth, and gini variable
  sample = merge(sample,ndata[ndata$round==0,][c("ID","initScore","showScore","scoreA","scoreB","percentA")],by="ID",all.x=TRUE)
  sample$visibleWealth = ifelse(sample$showScore=="true",1,0)
  sample <- sample %>%
    mutate(inequality =
             case_when(
               scoreA==500 ~ "none",
               scoreA==700 ~ "low", #gini 0.2
               scoreA==850 ~ "low", #gini 0.2
               scoreA==1150 ~ "high" 
             )
           )
  sample <- sample %>%
    mutate(wealth = 
             case_when(
               initScore>500 ~ "rich",
               initScore==500 ~ "middle",
               initScore<500 ~ "poor"
             )
           )
  sample <- subset(sample, select = -c(initScore,showScore,scoreA,scoreB,percentA))
  
  
  #normalize as appropriate (within each gameID, within each round) 
  #in this case, we will not normalize, as normalizing will cost us important information 
  #eg1, 0 vs 1 degree is very different from a social isolation perspective; z-normalizing will cost us this information 
  #eg2, the number of friends (=degree) of an individual is important regardless of the size of the network; z-normalizing will cost us this information
  #https://www.researchgate.net/publication/322609711_Combining_raw_and_normalized_data_in_multivariate_time_series_classification_with_dynamic_time_warping
  #https://towardsdatascience.com/transforming-skewed-data-73da4c2d0d16
  #https://www.cs.unm.edu/~mueen/DTW.pdf
  normalizeDegree = function(t) (t)
  
  sample.zdegree <- sample[,grepl("degree", names(sample))] %>% 
    group_by(sample$gameID) %>% 
    summarize_all(funs(normalizeDegree))
  sample.zdegree <- data.frame(sample.zdegree)
  
  colnames(sample.zdegree) <- gsub("degree", "zdegree", colnames(sample.zdegree))
  sample.zdegree <- subset(sample.zdegree,select=-c(sample.gameID))
  
  sample = cbind(sample,sample.zdegree)
  
  #merging connecting variable
  cdata$ID <- as.factor(cdata$ID)
  cdata$alterID <- as.factor(cdata$alterID)
  cdata$round <- as.factor(cdata$round)
  
  f.linksMade = function(x) ifelse(is.na(table(x)["makeLink"]),0,as.numeric(table(x)["makeLink"]))
  f.linksNotMade = function(x) ifelse(is.na(as.numeric(table(x)["notMakeLink"])),0,as.numeric(table(x)["notMakeLink"]))
  f.linksBroken = function(x) ifelse(is.na(as.numeric(table(x)["breakLink"])),0,as.numeric(table(x)["breakLink"]))
  f.linksNotBroken = function(x) ifelse(is.na(as.numeric(table(x)["notBreakLink"])),0,as.numeric(table(x)["notBreakLink"]))
  
  
  #links by ego
  sample$linksMadeByEgo.0 = NA
  linksMadeByEgo <- cdata["connecting"] %>% 
    group_by(cdata$ID, cdata$round) %>% 
    summarize_all(funs(f.linksMade))
  linksMadeByEgo <- data.frame(linksMadeByEgo)
  colnames(linksMadeByEgo)[c(1:3)] = c("ID","round","linksMadeByEgo")
  linksMadeByEgo <- reshape(linksMadeByEgo, idvar = "ID", timevar = "round", direction = "wide")
  #if NA, this means that no links were chosen for the person for that round
  sample = merge(sample,linksMadeByEgo,by=c("ID"),all.x=TRUE)
  
  sample$linksNotMadeByEgo.0 = NA
  linksNotMadeByEgo <- cdata["connecting"] %>% 
    group_by(cdata$ID, cdata$round) %>% 
    summarize_all(funs(f.linksNotMade))
  linksNotMadeByEgo <- data.frame(linksNotMadeByEgo)
  colnames(linksNotMadeByEgo)[c(1:3)] = c("ID","round","linksNotMadeByEgo")
  linksNotMadeByEgo <- reshape(linksNotMadeByEgo, idvar = "ID", timevar = "round", direction = "wide")
  #if NA, this means that no links were chosen for the person for that round
  sample = merge(sample,linksNotMadeByEgo,by=c("ID"),all.x=TRUE)
  
  sample$linksBrokenByEgo.0 = NA
  linksBrokenByEgo <- cdata["connecting"] %>% 
    group_by(cdata$ID, cdata$round) %>% 
    summarize_all(funs(f.linksBroken))
  linksBrokenByEgo <- data.frame(linksBrokenByEgo)
  colnames(linksBrokenByEgo)[c(1:3)] = c("ID","round","linksBrokenByEgo")
  linksBrokenByEgo <- reshape(linksBrokenByEgo, idvar = "ID", timevar = "round", direction = "wide")
  #if NA, this means that no links were chosen for the person for that round
  sample = merge(sample,linksBrokenByEgo,by=c("ID"),all.x=TRUE)
  
  sample$linksNotBrokenByEgo.0 = NA
  linksNotBrokenByEgo <- cdata["connecting"] %>% 
    group_by(cdata$ID, cdata$round) %>% 
    summarize_all(funs(f.linksNotBroken))
  linksNotBrokenByEgo <- data.frame(linksNotBrokenByEgo)
  colnames(linksNotBrokenByEgo)[c(1:3)] = c("ID","round","linksNotBrokenByEgo")
  linksNotBrokenByEgo <- reshape(linksNotBrokenByEgo, idvar = "ID", timevar = "round", direction = "wide")
  #if NA, this means that no links were chosen for the person for that round
  sample = merge(sample,linksNotBrokenByEgo,by=c("ID"),all.x=TRUE)
  
  
  #links by alter
  sample$linksMadeByAlter.0 = NA
  linksMadeByAlter <- cdata["connecting"] %>% 
    group_by(cdata$alterID, cdata$round) %>% 
    summarize_all(funs(f.linksMade))
  linksMadeByAlter <- data.frame(linksMadeByAlter)
  colnames(linksMadeByAlter)[c(1:3)] = c("ID","round","linksMadeByAlter")
  linksMadeByAlter <- reshape(linksMadeByAlter, idvar = "ID", timevar = "round", direction = "wide")
  #if NA, this means that no links were chosen for the person for that round
  sample = merge(sample,linksMadeByAlter,by=c("ID"),all.x=TRUE)
  
  sample$linksNotMadeByAlter.0 = NA
  linksNotMadeByAlter <- cdata["connecting"] %>% 
    group_by(cdata$alterID, cdata$round) %>% 
    summarize_all(funs(f.linksNotMade))
  linksNotMadeByAlter <- data.frame(linksNotMadeByAlter)
  colnames(linksNotMadeByAlter)[c(1:3)] = c("ID","round","linksNotMadeByAlter")
  linksNotMadeByAlter <- reshape(linksNotMadeByAlter, idvar = "ID", timevar = "round", direction = "wide")
  #if NA, this means that no links were chosen for the person for that round
  sample = merge(sample,linksNotMadeByAlter,by=c("ID"),all.x=TRUE)
  
  sample$linksBrokenByAlter.0 = NA
  linksBrokenByAlter <- cdata["connecting"] %>% 
    group_by(cdata$alterID, cdata$round) %>% 
    summarize_all(funs(f.linksBroken))
  linksBrokenByAlter <- data.frame(linksBrokenByAlter)
  colnames(linksBrokenByAlter)[c(1:3)] = c("ID","round","linksBrokenByAlter")
  linksBrokenByAlter <- reshape(linksBrokenByAlter, idvar = "ID", timevar = "round", direction = "wide")
  #if NA, this means that no links were chosen for the person for that round
  sample = merge(sample,linksBrokenByAlter,by=c("ID"),all.x=TRUE)
  
  sample$linksNotBrokenByAlter.0 = NA
  linksNotBrokenByAlter <- cdata["connecting"] %>% 
    group_by(cdata$alterID, cdata$round) %>% 
    summarize_all(funs(f.linksNotBroken))
  linksNotBrokenByAlter <- data.frame(linksNotBrokenByAlter)
  colnames(linksNotBrokenByAlter)[c(1:3)] = c("ID","round","linksNotBrokenByAlter")
  linksNotBrokenByAlter <- reshape(linksNotBrokenByAlter, idvar = "ID", timevar = "round", direction = "wide")
  #if NA, this means that no links were chosen for the person for that round
  sample = merge(sample,linksNotBrokenByAlter,by=c("ID"),all.x=TRUE)
  
  #behavior of alters (shown as proportions of alters that cooperated)
  #alterBehavior=0.6 would mean that for a given ego, 60% of all alters cooperated, while 40% defected.
  sample$alterBehavior.0 = NA
  alterBehavior <- aggregate(as.numeric(alter_behavior) ~ ID + round, data = cdata, FUN = mean, na.rm = TRUE)
  colnames(alterBehavior)[c(1:3)] = c("ID","round","alterBehavior")
  alterBehavior <- reshape(alterBehavior, idvar = "ID", timevar = "round", direction = "wide")
  #if NA, this means that no links were chosen for the person for that round
  sample = merge(sample,alterBehavior,by=c("ID"),all.x=TRUE)
  
  
  behavior <- ndata[c("ID","round","behavior")]
  behavior[behavior=="NA"] <- NA
  behavior$behavior = ifelse(behavior$behavior=="",NA,behavior$behavior)
  behavior$behavior <- factor(behavior$behavior)
  behavior$behavior <- droplevels(behavior$behavior)
  behavior <- reshape(behavior, idvar = "ID", timevar = "round", direction = "wide")
  sample = merge(sample,behavior,by=c("ID"),all.x=TRUE)
  sample$behavior.0 = NA
  
  save(sample, file=datafile)
  
  return(list(sample,sample.long))

}




