library(dplyr)
library(tidyr)
library(ggplot2)
library(arsenal)

#' Download and create node-level dataset.
#' @param redo If FALSE, will not redownload and recreate the data if
#' data already exists on disk. If TRUE, it will.
experiment <- function(redo=FALSE) {
    
  rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
  datafile <- paste(rootdir,"data/experiment.Rda", sep="/")
  
  if (!redo && file.exists(datafile)) {
    cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
    cat("Call experiment(redo=TRUE) to update data.\n")
    load(datafile)
    
    return(list(sample,sample.wide,g.isolation))
  }
  
  json_files <- list.files(paste(rootdir,"Python/PyScript-BRB",sep="/"), pattern = "*.json", full.names = TRUE)
  
  ndata <- NULL
  for(file in json_files){
    list <- fromJSON(file)
    df <- do.call(rbind, lapply(names(list$result)[names(list$result)!="0"], function(round) {
      round_data <- list$result[[round]]$nodes
      
      # Extract variables for each person and round
      person_data <- lapply(seq_along(round_data), function(person) {
        data.frame(
          round = round,
          person = person,
          id = round_data[[person]]$id,
          age = round_data[[person]]$age,
          gender = round_data[[person]]$gender,
          initialLoneliness = round_data[[person]]$initialIsolation,
          initScore = round_data[[person]]$initScore,
          loneliness = round_data[[person]]$isolation,
          behavior = round_data[[person]]$behavior,
          makeLink = length(round_data[[person]]$makeLink),
          notMakeLink = length(round_data[[person]]$notMakeLink),
          breakLink = length(round_data[[person]]$breakLink),
          notBreakLink = length(round_data[[person]]$notBreakLink),
          payoff = round_data[[person]]$payoff,
          cumulativePayoff = round_data[[person]]$cumulativePayoff
        )
      })
      
      # Combine person dataframes for each round
      if (length(person_data) > 0) {
        do.call(rbind, person_data)
      } else {
        NULL
      }
    }))
    #names(list)
    df$gameId <- list$gameId
    ndata <- rbind(ndata,df)
  }
  
  ldata <- NULL
  for(file in json_files){
    list <- fromJSON(file)
    df <- do.call(rbind, lapply(names(list$result), function(round) {
      round_data <- list$result[[round]]$links
      
      # Extract link information
      person_data <- lapply(seq_along(round_data), function(person) {
        data.frame(
          id1 = round_data[[person]]$source,
          id2 = round_data[[person]]$target,
          round = round
        )
      })
      
      # Combine person dataframes for each round
      if (length(person_data) > 0) {
        do.call(rbind, person_data)
      } else {
        NULL
      }
    }))
    #names(list)
    df$gameId <- list$gameId
    ldata <- rbind(ldata,df)
  }
  
  ndata <- ndata[,!names(ndata) %in% c("person")]
  ndata.r0 <- ndata[ndata$round==1,]
  ndata.r0$round <- 0
  ndata.r0$cumulativePayoff <- ndata.r0$initScore
  ndata.r0[c("loneliness","behavior","makeLink","notMakeLink","breakLink","notBreakLink","payoff")] <- NA
  ndata <- rbind(ndata,ndata.r0)
  ndata$ID = paste(ndata$id,ndata$gameId,sep="_")
  ldata$ID = paste(ldata$id1,ldata$gameId,sep="_")
  ldata$alterID = paste(ldata$id2,ldata$gameId,sep="_")
  
  ndata$age <- ifelse(as.numeric(ndata$age)>1000, 2023 - as.numeric(ndata$age), as.numeric(ndata$age))
  ndata$round <- as.numeric(ndata$round)
  ndata$gender <- ifelse(ndata$gender=="",NA,ndata$gender)
  ndata$behavior <- ifelse(ndata$behavior=="",NA,ndata$behavior)
  #ndata$arm <- gsub("-.*", "", tolower(ndata$gameId)) #for MTurk
  ndata$arm <- gsub("*..-", "", tolower(ndata$gameId)) #for Prolific
  ndata <- ndata %>%
    mutate(initialLoneliness =
             case_when(
               initialLoneliness=="strongly_disagree" ~ 1, 
               initialLoneliness=="disagree" ~ 2, 
               initialLoneliness=="agree" ~ 3, 
               initialLoneliness=="strongly_agree" ~ 4
             ))
  ndata <- ndata %>%
    mutate(loneliness =
             case_when(
               loneliness=="strongly_disagree" ~ 1, 
               loneliness=="disagree" ~ 2, 
               loneliness=="agree" ~ 3, 
               loneliness=="strongly_agree" ~ 4
             ))
  ndata <- ndata[c(2,1,3:ncol(ndata))]
  ldata$round <- as.numeric(ldata$round)

  #for MTurk: remove ldata person p3 for round 0, gameId Low-20 (included in ldata for just round 0, but not included in ldata in later rounds or ndata)
  # ldata = ldata[!((ldata$id1=="p3" | ldata$id2=="p3") & ldata$round==0 & ldata$gameId=="Low-20"),]
  
  #for prolific: remove ldata person p7 for round 0, gameId 10-random (included in ldata for just round 0, but not included in ldata in later rounds or ndata)
  ldata = ldata[!((ldata$id1=="p7" | ldata$id2=="p7") & ldata$round==0 & ldata$gameId=="10-random"),]
  #for prolific: remove ldata person p7 for round 0, gameId 18-high (included in ldata for just round 0, but not included in ldata in later rounds or ndata)
  ldata = ldata[!((ldata$id1=="p7" | ldata$id2=="p7") & ldata$round==0 & ldata$gameId=="18-high"),]
  #for prolific: remove ldata person p4 for round 0, gameId 28-low (included in ldata for just round 0, but not included in ldata in later rounds or ndata)
  ldata = ldata[!((ldata$id1=="p4" | ldata$id2=="p4") & ldata$round==0 & ldata$gameId=="28-low"),]
  #for prolific: remove ldata person p5 and p15 for round 0, gameId 53-random (included in ldata for just round 0, but not included in ldata in later rounds or ndata)
  ldata = ldata[!((ldata$id1=="p5" | ldata$id2=="p5") & ldata$round==0 & ldata$gameId=="53-random"),]
  ldata = ldata[!((ldata$id1=="p15" | ldata$id2=="p15") & ldata$round==0 & ldata$gameId=="53-random"),]
  
  g.isolation <- NULL
  n = 1
  for(i in unique(ldata$gameId)){
    for(m in unique(ldata$round)){
      g.isolation[[n]] =
        graph_from_data_frame(
          ldata[ldata$round==m & ldata$gameId==i,],
          directed=FALSE, 
          vertices=ndata[ndata$round==m & ndata$gameId==i,])
      n=n+1
    }
  }
  
  #to see who only participated in round 1
  #sort(unique(c(ldata[ldata$round==0 & ldata$gameId==i,]$id1,ldata[ldata$round==0 & ldata$gameId==i,]$id2)))
  #sort(unique(c(ldata[ldata$round==1 & ldata$gameId==i,]$id1,ldata[ldata$round==1 & ldata$gameId==i,]$id2)))
  
  vertexNames = function(x){
    V(x)$name
  }
  
  n=1
  round=NULL
  gameId=NULL
  vertex=NULL
  coefLocal=NULL
  coefGlobal=NULL
  degree=NULL
  for(i in 1:length(g.isolation)){
    round[[i]] = as.factor(V(g.isolation[[i]])$round)
    gameId[[i]] = as.factor(V(g.isolation[[i]])$gameId)
    vertex[[i]] = vertexNames(g.isolation[[i]])
    coefLocal[[i]] = transitivity(g.isolation[[i]], type="local")
    coefGlobal[[i]] = rep(transitivity(g.isolation[[i]], type="global"),length(V(g.isolation[[i]])$round))
    degree[[i]] = igraph::degree(g.isolation[[i]])
    n=n+1
  }
  
  clust.coef.df <- NULL
  clust.coef.df$round = unlist(round)
  clust.coef.df$gameId = unlist(gameId)
  clust.coef.df$vertex = unlist(vertex)
  clust.coef.df$coefLocal = unlist(coefLocal)
  clust.coef.df$coefGlobal = unlist(coefGlobal)
  clust.coef.df$degree = unlist(degree)
  clust.coef.df = data.frame(clust.coef.df)
  clust.coef.df$ID = paste(clust.coef.df$vertex,clust.coef.df$gameId,sep="_")
  
  sample = merge(ndata,clust.coef.df,by=c("ID","round","gameId"))
  sample <- sample[,!names(sample) %in% c("vertex")]
  
  sample.wide = stats::reshape(data=sample,
                          v.names=c('loneliness','behavior',
                                    'makeLink','notMakeLink','breakLink','notBreakLink',
                                    'payoff','cumulativePayoff',
                                    'coefLocal','coefGlobal','degree'),
                          timevar='round',
                          idvar='ID',
                          direction='wide')
  
  save(sample,sample.wide,g.isolation,file=datafile)
  
  return(list(sample,sample.wide,g.isolation))

}




