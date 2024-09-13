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
  ldata$alterID = paste(ldata$id2,ldata$gameID,sep="_")
  cdata$ID = paste(paste("p",cdata$ego_id %% 100,sep=""),cdata$gameID,sep="_")
  cdata$alterID = paste(paste("p",cdata$alter_id %% 100,sep=""),cdata$gameID,sep="_")
  
  ldata.isolation = ldata
  ndata.isolation = ndata[c("id","gameID","round","cumulativePayoff")]
  ndata.isolation2 = ndata[c("id","gameID","round","behavior")]
  ndata.isolation2$round = ndata.isolation2$round-1
  ndata.isolation = merge(ndata.isolation,ndata.isolation2,by=c("id","gameID","round"),all.x=TRUE)
  
  cdata.wealth = cdata[c("ID","alterID","round","connecting","ego_local_gini","ego_behavior","alter_behavior")]
  cdata.wealth$round = cdata.wealth$round - 1
  ldata.wealth = merge(ldata,cdata.wealth,by=c("ID","alterID","round"),all.x=TRUE)
  ndata.wealth = ndata
  ndata.wealth$visibleWealth = ifelse(ndata.wealth$showScore=="true","Visible","Not visible")
  ndata.wealth[ndata.wealth$round==0,]$cumulativePayoff = ndata.wealth[ndata.wealth$round==0,]$initScore 
  ldata.wealth = merge(ldata.wealth,ndata.wealth[c("ID","round","cumulativePayoff","visibleWealth")],by=c("ID","round"),all.x=TRUE)
  ldata.wealth = ldata.wealth %>% rename(egoWealth = cumulativePayoff)
  ldata.wealth = merge(ldata.wealth,ndata.wealth[c("ID","round","cumulativePayoff")],by.x=c("alterID","round"),by.y=c("ID","round"),all.x=TRUE)
  ldata.wealth = ldata.wealth %>% rename(alterWealth = cumulativePayoff)
  ldata.wealth$wealthDiff = ldata.wealth$alterWealth - ldata.wealth$egoWealth
  ldata.wealth = subset(ldata.wealth,round!=10)
  
  
  
  #merging visibility, initial wealth, and gini variable
  sample = ndata[ndata$round==0,][c("ID","initScore","showScore","scoreA","scoreB","percentA")]
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
    mutate(gini =
             case_when(
               scoreA==500 ~ 0,
               scoreA==700 ~ 0.2, 
               scoreA==850 ~ 0.2, 
               scoreA==1150 ~ 0.4 
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
  sample <- subset(sample, select = -c(showScore,scoreA,scoreB,percentA))
  
  
  
  g.isolation <- NULL
  n = 1
  for(i in unique(ldata.isolation$gameID)){
    for(m in unique(ldata.isolation$round)){
      g.isolation[[n]] =
        graph_from_data_frame(
          ldata.isolation[ldata.isolation$round==m & ldata.isolation$gameID==i,],
          directed=FALSE, 
          vertices=ndata.isolation[ndata.isolation$round==m & ndata.isolation$gameID==i,])
      n=n+1
    }
  }
  
  vertexNames = function(x){
    V(x)$name
  }
  
  n=1
  round=NULL
  gameID=NULL
  vertex=NULL
  coefLocal=NULL
  coefGlobal=NULL
  degree=NULL
  for(i in 1:length(g.isolation)){
    round[[i]] = as.factor(V(g.isolation[[i]])$round)
    gameID[[i]] = as.factor(V(g.isolation[[i]])$gameID)
    vertex[[i]] = vertexNames(g.isolation[[i]])
    coefLocal[[i]] = transitivity(g.isolation[[i]], type="local")
    coefGlobal[[i]] = rep(transitivity(g.isolation[[i]], type="global"),length(V(g.isolation[[i]])$round))
    degree[[i]] = igraph::degree(g.isolation[[i]])
    n=n+1
  }
  
  clust.coef.df <- NULL
  clust.coef.df$round = unlist(round)
  clust.coef.df$gameID = unlist(gameID)
  clust.coef.df$vertex = unlist(vertex)
  clust.coef.df$coefLocal = unlist(coefLocal)
  clust.coef.df$coefGlobal = unlist(coefGlobal)
  clust.coef.df$degree = unlist(degree)
  clust.coef.df = data.frame(clust.coef.df)
  clust.coef.df$ID = paste(clust.coef.df$vertex,clust.coef.df$gameID,sep="_")
  sample = merge(sample,clust.coef.df,by=c("ID"),all.y=TRUE)
  
  sample$ID = paste(sample$vertex,sample$gameID,sep="_")
  
  sample = stats::reshape(data=sample,
                          v.names=c('degree','coefLocal','coefGlobal'),
                          timevar='round',
                          idvar='ID',
                          direction='wide')

  
  #merging connecting variable
  cdata$ID <- as.factor(cdata$ID)
  cdata$alterID <- as.factor(cdata$alterID)
  cdata$round <- as.factor(cdata$round)
  
  f.linksMade = function(x) ifelse(is.na(table(x)["makeLink"]),0,as.numeric(table(x)["makeLink"]))
  f.linksNotMade = function(x) ifelse(is.na(as.numeric(table(x)["notMakeLink"])),0,as.numeric(table(x)["notMakeLink"]))
  f.linksBroken = function(x) ifelse(is.na(as.numeric(table(x)["breakLink"])),0,as.numeric(table(x)["breakLink"]))
  f.linksNotBroken = function(x) ifelse(is.na(as.numeric(table(x)["notBreakLink"])),0,as.numeric(table(x)["notBreakLink"]))
  
  #alter previous wealth
  sample$alterPrevWealth.0 = NA
  alterPrevWealth <- cdata["alter_prev_wealth"] %>% 
    group_by(cdata$ID, cdata$round) %>% 
    summarize_all(mean, na.rm=TRUE)
  alterPrevWealth <- data.frame(alterPrevWealth)
  colnames(alterPrevWealth)[c(1:3)] = c("ID","round","alterPrevWealth")
  alterPrevWealth <- reshape(alterPrevWealth, idvar = "ID", timevar = "round", direction = "wide")
  sample = merge(sample,alterPrevWealth,by=c("ID"),all.x=TRUE)
  
  #ego previous wealth
  sample$egoPrevWealth.0 = NA
  egoPrevWealth <- cdata["ego_prev_wealth"] %>% 
    group_by(cdata$ID, cdata$round) %>% 
    summarize_all(mean, na.rm=TRUE)
  egoPrevWealth <- data.frame(egoPrevWealth)
  colnames(egoPrevWealth)[c(1:3)] = c("ID","round","egoPrevWealth")
  egoPrevWealth <- reshape(egoPrevWealth, idvar = "ID", timevar = "round", direction = "wide")
  sample = merge(sample,egoPrevWealth,by=c("ID"),all.x=TRUE)
  
  #previous cooperation or not
  sample$prevCoop.0 = NA
  sample$prevCoop.1 = NA
  prevCoop <- cdata["ego_behavior"] %>% 
    group_by(cdata$ID, cdata$round) %>% 
    summarize_all(mean, na.rm=TRUE)
  prevCoop <- data.frame(prevCoop)
  colnames(prevCoop)[c(1:3)] = c("ID","round","prevCoop")
  prevCoop$round = factor(as.numeric(prevCoop$round) + 1)
  prevCoop = prevCoop[prevCoop$round!='11',]
  prevCoop <- reshape(prevCoop, idvar = "ID", timevar = "round", direction = "wide")
  sample = merge(sample,prevCoop,by=c("ID"),all.x=TRUE)
  
  #wealth difference
  sample$wealthDiff.0 = NA
  sample$wealthDiff.1 = sample$alterPrevWealth.1 - sample$egoPrevWealth.1
  sample$wealthDiff.2 = sample$alterPrevWealth.2 - sample$egoPrevWealth.2
  sample$wealthDiff.3 = sample$alterPrevWealth.3 - sample$egoPrevWealth.3
  sample$wealthDiff.4 = sample$alterPrevWealth.4 - sample$egoPrevWealth.4
  sample$wealthDiff.5 = sample$alterPrevWealth.5 - sample$egoPrevWealth.5
  sample$wealthDiff.6 = sample$alterPrevWealth.6 - sample$egoPrevWealth.6
  sample$wealthDiff.7 = sample$alterPrevWealth.7 - sample$egoPrevWealth.7
  sample$wealthDiff.8 = sample$alterPrevWealth.8 - sample$egoPrevWealth.8
  sample$wealthDiff.9 = sample$alterPrevWealth.9 - sample$egoPrevWealth.9
  sample$wealthDiff.10 = sample$alterPrevWealth.10 - sample$egoPrevWealth.10
  
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
  
  sample.wide = sample[c("degree.0","degree.1",
                           "degree.2","degree.3",
                           "degree.4","degree.5",
                           "degree.6","degree.7",
                           "degree.8","degree.9","degree.10")]
  
  sample.wide = sample[complete.cases(sample.wide),]
  
  sample.wide = sample.wide[c("ID","visibleWealth","inequality","gini","wealth","initScore","gameID","vertex",
                  "degree.0","degree.1","degree.2","degree.3","degree.4","degree.5","degree.6","degree.7","degree.8","degree.9","degree.10",
                  "coefLocal.0","coefLocal.1","coefLocal.2","coefLocal.3","coefLocal.4","coefLocal.5","coefLocal.6","coefLocal.7","coefLocal.8","coefLocal.9","coefLocal.10",
                  "coefGlobal.0","coefGlobal.1","coefGlobal.2","coefGlobal.3","coefGlobal.4","coefGlobal.5","coefGlobal.6","coefGlobal.7","coefGlobal.8","coefGlobal.9","coefGlobal.10",
                  "alterPrevWealth.0","alterPrevWealth.1","alterPrevWealth.2","alterPrevWealth.3","alterPrevWealth.4","alterPrevWealth.5","alterPrevWealth.6","alterPrevWealth.7","alterPrevWealth.8","alterPrevWealth.9","alterPrevWealth.10",
                  "egoPrevWealth.0","egoPrevWealth.1","egoPrevWealth.2","egoPrevWealth.3","egoPrevWealth.4","egoPrevWealth.5","egoPrevWealth.6","egoPrevWealth.7","egoPrevWealth.8","egoPrevWealth.9","egoPrevWealth.10",
                  "wealthDiff.0","wealthDiff.1","wealthDiff.2","wealthDiff.3","wealthDiff.4","wealthDiff.5","wealthDiff.6","wealthDiff.7","wealthDiff.8","wealthDiff.9","wealthDiff.10",
                  "linksMadeByEgo.0","linksMadeByEgo.1","linksMadeByEgo.2","linksMadeByEgo.3","linksMadeByEgo.4","linksMadeByEgo.5","linksMadeByEgo.6","linksMadeByEgo.7","linksMadeByEgo.8","linksMadeByEgo.9","linksMadeByEgo.10",
                  "linksNotMadeByEgo.0","linksNotMadeByEgo.1","linksNotMadeByEgo.2","linksNotMadeByEgo.3","linksNotMadeByEgo.4","linksNotMadeByEgo.5","linksNotMadeByEgo.6","linksNotMadeByEgo.7","linksNotMadeByEgo.8","linksNotMadeByEgo.9","linksNotMadeByEgo.10",
                  "linksBrokenByEgo.0", "linksBrokenByEgo.1", "linksBrokenByEgo.2", "linksBrokenByEgo.3", "linksBrokenByEgo.4","linksBrokenByEgo.5", "linksBrokenByEgo.6", "linksBrokenByEgo.7", "linksBrokenByEgo.8", "linksBrokenByEgo.9", "linksBrokenByEgo.10",  
                  "linksNotBrokenByEgo.0","linksNotBrokenByEgo.1","linksNotBrokenByEgo.2","linksNotBrokenByEgo.3","linksNotBrokenByEgo.4","linksNotBrokenByEgo.5","linksNotBrokenByEgo.6","linksNotBrokenByEgo.7","linksNotBrokenByEgo.8","linksNotBrokenByEgo.9","linksNotBrokenByEgo.10",
                  "linksMadeByAlter.0","linksMadeByAlter.1", "linksMadeByAlter.2", "linksMadeByAlter.3", "linksMadeByAlter.4", "linksMadeByAlter.5", "linksMadeByAlter.6","linksMadeByAlter.7", "linksMadeByAlter.8", "linksMadeByAlter.9", "linksMadeByAlter.10",
                  "linksNotMadeByAlter.0","linksNotMadeByAlter.1","linksNotMadeByAlter.2","linksNotMadeByAlter.3","linksNotMadeByAlter.4","linksNotMadeByAlter.5","linksNotMadeByAlter.6","linksNotMadeByAlter.7","linksNotMadeByAlter.8","linksNotMadeByAlter.9","linksNotMadeByAlter.10",
                  "linksBrokenByAlter.0", "linksBrokenByAlter.1", "linksBrokenByAlter.2","linksBrokenByAlter.3", "linksBrokenByAlter.4", "linksBrokenByAlter.5", "linksBrokenByAlter.6", "linksBrokenByAlter.7", "linksBrokenByAlter.8","linksBrokenByAlter.9", "linksBrokenByAlter.10",
                  "linksNotBrokenByAlter.0","linksNotBrokenByAlter.1","linksNotBrokenByAlter.2","linksNotBrokenByAlter.3","linksNotBrokenByAlter.4","linksNotBrokenByAlter.5","linksNotBrokenByAlter.6","linksNotBrokenByAlter.7","linksNotBrokenByAlter.8","linksNotBrokenByAlter.9" ,"linksNotBrokenByAlter.10",
                  "alterBehavior.0", "alterBehavior.1", "alterBehavior.2", "alterBehavior.3", "alterBehavior.4","alterBehavior.5", "alterBehavior.6", "alterBehavior.7", "alterBehavior.8", "alterBehavior.9", "alterBehavior.10", 
                  "behavior.0","behavior.1","behavior.2","behavior.3","behavior.4","behavior.5","behavior.6","behavior.7","behavior.8","behavior.9","behavior.10",
                  "prevCoop.0","prevCoop.1","prevCoop.2","prevCoop.3","prevCoop.4","prevCoop.5","prevCoop.6","prevCoop.7","prevCoop.8","prevCoop.9","prevCoop.10"
                  )]
  
  sample.wide = as.data.frame(sample.wide)
  
  
  df.wide = data.frame(sample.wide)
  df.wide = df.wide[c("ID","gini","wealth","inequality","visibleWealth",
                      "degree.0","alterPrevWealth.0","egoPrevWealth.0","wealthDiff.0","behavior.0",'prevCoop.0',
                      "degree.1","alterPrevWealth.1","egoPrevWealth.1","wealthDiff.1","behavior.1",'prevCoop.1',
                      "degree.2","alterPrevWealth.2","egoPrevWealth.2","wealthDiff.2","behavior.2",'prevCoop.2',
                      "degree.3","alterPrevWealth.3","egoPrevWealth.3","wealthDiff.3","behavior.3",'prevCoop.3',
                      "degree.4","alterPrevWealth.4","egoPrevWealth.4","wealthDiff.4","behavior.4",'prevCoop.4',
                      "degree.5","alterPrevWealth.5","egoPrevWealth.5","wealthDiff.5","behavior.5",'prevCoop.5',
                      "degree.6","alterPrevWealth.6","egoPrevWealth.6","wealthDiff.6","behavior.6",'prevCoop.6',
                      "degree.7","alterPrevWealth.7","egoPrevWealth.7","wealthDiff.7","behavior.7",'prevCoop.7',
                      "degree.8","alterPrevWealth.8","egoPrevWealth.8","wealthDiff.8","behavior.8",'prevCoop.8',
                      "degree.9","alterPrevWealth.9","egoPrevWealth.9","wealthDiff.9","behavior.9",'prevCoop.9',
                      "degree.10","alterPrevWealth.10","egoPrevWealth.10","wealthDiff.10","behavior.10",'prevCoop.10')]
  sample.long = stats::reshape(df.wide, 
                        direction='long', 
                        varying=c( "degree.0","alterPrevWealth.0","egoPrevWealth.0","wealthDiff.0","behavior.0",'prevCoop.0',
                                   "degree.1","alterPrevWealth.1","egoPrevWealth.1","wealthDiff.1","behavior.1",'prevCoop.1',
                                   "degree.2","alterPrevWealth.2","egoPrevWealth.2","wealthDiff.2","behavior.2",'prevCoop.2',
                                   "degree.3","alterPrevWealth.3","egoPrevWealth.3","wealthDiff.3","behavior.3",'prevCoop.3',
                                   "degree.4","alterPrevWealth.4","egoPrevWealth.4","wealthDiff.4","behavior.4",'prevCoop.4',
                                   "degree.5","alterPrevWealth.5","egoPrevWealth.5","wealthDiff.5","behavior.5",'prevCoop.5',
                                   "degree.6","alterPrevWealth.6","egoPrevWealth.6","wealthDiff.6","behavior.6",'prevCoop.6',
                                   "degree.7","alterPrevWealth.7","egoPrevWealth.7","wealthDiff.7","behavior.7",'prevCoop.7',
                                   "degree.8","alterPrevWealth.8","egoPrevWealth.8","wealthDiff.8","behavior.8",'prevCoop.8',
                                   "degree.9","alterPrevWealth.9","egoPrevWealth.9","wealthDiff.9","behavior.9",'prevCoop.9',
                                   "degree.10","alterPrevWealth.10","egoPrevWealth.10","wealthDiff.10","behavior.10",'prevCoop.10'), 
                        timevar='round',
                        times=c(0:10),
                        idvar='ID') 
  
  
  #data with practice rounds
  datafile <- paste(rootdir,"data/node_prac_1216.Rdata", sep="/")
  
  load(datafile)
  
  sample.prac = ndata[ndata$isAI==FALSE,]
  
  sample.prac$ID = paste(sample.prac$id,sample.prac$gameID,sep="_")
  
  sample.prac <- sample.prac %>%
    mutate(showScore = 
             case_when(
               showScore == "true" ~ 1,
               showScore == "false" ~ 0
             ))
  
  sample.prac <- sample.prac %>%
    mutate(behavior = 
             case_when(
               behavior == "C" ~ 1,
               behavior == "D" ~ 0
             ))
  
  sample.prac$initialGini = NULL
  sample.prac <- sample.prac %>%
    mutate(initialGini = 
             case_when(
               scoreA == 500 ~ 0,
               scoreA == 700 ~ 0.2,
               scoreA == 850 ~ 0.2,
               scoreA == 1150 ~ 0.4
             ))
  
  
  
  #add in wealth difference variable here
  sample.prac.wide = stats::reshape(data=sample.prac[c("ID","round","behavior","showScore","initialGini","cumulativePayoff")],
                                    v.names=c('behavior','cumulativePayoff'),
                                    timevar='round',
                                    idvar='ID',
                                    direction='wide')
  
  sample.prac.wide = rename(sample.prac.wide,behavior.p1=behavior.1)
  sample.prac.wide = rename(sample.prac.wide,behavior.p2=behavior.2)
  
  sample.prac.wide = merge(sample.prac.wide[c("ID","initialGini","behavior.p1","behavior.p2")],
                           sample.wide[c("ID","behavior.1","visibleWealth","gini")], 
                           by=c("ID"), all.x=TRUE)
  
  sample.prac.wide <- sample.prac.wide %>%
    mutate(behavior.1 = 
             case_when(
               behavior.1 == "C" ~ 1,
               behavior.1 == "D" ~ 0
             ))
  
  
  save(sample,ldata.wealth,gameID,sample.wide,cdata,ndata,sample.long,sample.prac.wide,file=datafile)
  
  return(list(sample,ldata.wealth,g.isolation,sample.wide,cdata,ndata,sample.long,sample.prac.wide))

}




