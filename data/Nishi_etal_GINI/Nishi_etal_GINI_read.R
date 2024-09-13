#by Akihiro Nishi @YINS Human Nature Lab (akihironishi@gmail.com)

##########################################################################################
#Phase I: Unzip the "Nishi_Shirado_GINI.zip"
##########################################################################################

##########################################################################################
#Phase II: Rjson data infile 
##########################################################################################
#rjson is the package to import the json file to R. 
#install.packages("rjson")
library(rjson)

#Step 1: Original data
#setwd("/XXXXX/Nishi_Shirado_GINI") #Please specify your directory
setwd("/Users/akihironishi/Dropbox/ArticlesAN/ANHS13GINI/Archive/Nishi_Shirado_GINI") 
data_list = c("D2013-11-07-01_4227.json","D2013-11-07-02_4257.json", "D2013-11-07-03_4261.json","D2013-11-07-04_4289.json",
"D2013-11-08-03_4353.json","D2013-11-08-04_4354.json","D2013-11-11-01_4361.json","D2013-11-11-02_4385.json","D2013-11-11-03_4417.json",
"D2013-11-11-04_4421.json","D2013-11-12-01_4449.json","D2013-11-12-02_4461.json","D2013-11-14-01-4546.json","D2013-11-15-01_4547.json",
"D2013-11-16-01_4577.json","D2013-11-16-02_4578.json","D2013-11-18-01_4581.json",
"D2013-11-18-02_4609.json","D2013-11-18-03_4621.json","D2013-11-18-04_4641.json","D2013-11-18-05_4673.json",
"D2013-11-18-06_4681.json","D2013-11-19-01_4705.json","D2013-11-19-02_4737.json","D2013-11-19-03_4741.json",
"D2013-11-19-04_4769.json","D2013-11-19-05_4781.json","D2013-11-20-01_4801.json","D2013-11-20-02_4833.json",
"D2013-11-20-03_4841.json","D2013-11-21-01_4866.json","D2013-11-21-02_4897.json","D2013-11-21-03_4901.json",
"D2013-11-21-04_4929.json","D2013-11-21-05_4941.json","D2013-11-21-07_4993.json","D2013-11-22-01_5001.json",
"D2013-11-22-02_5002.json","D2013-11-22-03_5003.json","D2013-11-26-04_5061.json","D2013-12-02-02_5153.json",
"D2013-12-02-03_5161.json","D2013-12-02-04_5163.json","D2013-12-02-05_5164.json","D2013-12-02-06_5185.json",
"D2013-12-05-01_5313.json","D2013-12-05-02_5321.json","D2013-12-06-01_5377.json","D2013-12-06-04_5441.json",
"D2013-12-09-01_5473.json","D2013-12-09-02_5481.json","D2013-12-09-03_5505.json","D2013-12-09-04_5537.json",
"D2013-12-09-05_5541.json","D2013-12-10-01_5569.json","D2013-12-10-02_5581.json","D2013-12-10-04_5633.json",
"D2013-12-10-05_5641.json","D2013-12-10-06_5665.json","D2013-12-11-01_5697.json","D2013-12-11-02_5701.json",
"D2013-12-11-03_5729.json","D2013-12-11-04_5741.json","D2013-12-11-05_5761.json","D2013-12-11-06_5793.json",
"D2013-12-12-01_5801.json","D2013-12-12-03_5861.json","D2013-12-13-01_5889.json","D2013-12-13-02_5901.json",
"D2013-12-13-03_5921.json","D2013-12-16-01_5953.json","D2013-12-16-02_5961.json","D2013-12-16-03_5985.json",
"D2013-12-17-01_6061.json","D2013-12-17-02_6081.json","D2013-12-17-03_6113.json","D2013-12-18-01_6221.json",
"D2013-12-18-02_6241.json","D2013-12-18-03_6273.json","D2013-12-19-03_6337.json")

for (i in 1:80)
{
data1 = fromJSON(file=data_list[i], method="C", unexpected.escape="error") 

#The detail of the data format, see Hirokazu Shirado's json_format.rtf 
#Note: each graph data is the result AFTER cooperation and rewiring step. Therefore, if you want to know connections when players select cooperation or defection, you have to use the graph data of the PRECEDING round.
#T500_1 is a list
#Starting: gameId, scoreA, scoreB, profit, initDensity, percentA, showScore, cost
#In $result: `0` to `10`
#In each number: directed(always FALSE), graph (round number), nodes, links, multigraph(always FALSE)
#In nodes of `0`: nodes[[i]]$id, initScore (i = 1-25)
#In nodes of `1`-: nodes[[i]]$id, initScore, payoff, cumulativePayoff, behavior, behaviorTime, makeLink, notMakeLink, breakLink, notbreakLink 
#In links of `0`,`1`-: link[[j]]$id

#1. Nodes data
data2_r0 = as.data.frame(do.call("rbind", data1$result$`0`$nodes))
data2_r1 = as.data.frame(do.call("rbind", data1$result$`1`$nodes))
data2_r2 = as.data.frame(do.call("rbind", data1$result$`2`$nodes))
data2_r3 = as.data.frame(do.call("rbind", data1$result$`3`$nodes))
data2_r4 = as.data.frame(do.call("rbind", data1$result$`4`$nodes))
data2_r5 = as.data.frame(do.call("rbind", data1$result$`5`$nodes))
data2_r6 = as.data.frame(do.call("rbind", data1$result$`6`$nodes))
data2_r7 = as.data.frame(do.call("rbind", data1$result$`7`$nodes))
data2_r8 = as.data.frame(do.call("rbind", data1$result$`8`$nodes))
data2_r9 = as.data.frame(do.call("rbind", data1$result$`9`$nodes))
data2_r10 = as.data.frame(do.call("rbind", data1$result$`10`$nodes))
data2_r0$cumulativePayoff = data2_r0$behaviorTime = data2_r0$notMakeLink = data2_r0$notBreakLink = data2_r0$behavior = data2_r0$breakLink = data2_r0$makeLink = data2_r0$payoff = NA  
data2_r0 = data2_r0[,names(data2_r1)]
data2_r0$round = 0
data2_r1$round = 1
data2_r2$round = 2
data2_r3$round = 3
data2_r4$round = 4
data2_r5$round = 5
data2_r6$round = 6
data2_r7$round = 7
data2_r8$round = 8
data2_r9$round = 9
data2_r10$round = 10
data2 = rbind(data2_r0,data2_r1,data2_r2,data2_r3,data2_r4,data2_r5,data2_r6,data2_r7,data2_r8,data2_r9,data2_r10)
data2$gameID = data1$gameId
data2$scoreA = data1$scoreA
data2$scoreB = data1$scoreB
data2$percentA = data1$percentA
data2$showScore = data1$showScore

data2$initScore = unlist(data2$initScore)
data2$cumulativePayoff = unlist(data2$cumulativePayoff)
data2$behaviorTime = unlist(data2$behaviorTime)
data2$behavior = unlist(data2$behavior)
data2$id = unlist(data2$id)
data2$payoff = unlist(data2$payoff)

#2. Links data
data3_r0 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`0`$links))$id),ncol=2,byrow=TRUE))
data3_r1 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`1`$links))$id),ncol=2,byrow=TRUE))
data3_r2 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`2`$links))$id),ncol=2,byrow=TRUE))
data3_r3 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`3`$links))$id),ncol=2,byrow=TRUE))
data3_r4 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`4`$links))$id),ncol=2,byrow=TRUE))
data3_r5 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`5`$links))$id),ncol=2,byrow=TRUE))
data3_r6 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`6`$links))$id),ncol=2,byrow=TRUE))
data3_r7 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`7`$links))$id),ncol=2,byrow=TRUE))
data3_r8 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`8`$links))$id),ncol=2,byrow=TRUE))
data3_r9 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`9`$links))$id),ncol=2,byrow=TRUE))
data3_r10 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`10`$links))$id),ncol=2,byrow=TRUE))
data3_r0$round = 0
data3_r1$round = 1
data3_r2$round = 2
data3_r3$round = 3
data3_r4$round = 4
data3_r5$round = 5
data3_r6$round = 6
data3_r7$round = 7
data3_r8$round = 8
data3_r9$round = 9
data3_r10$round = 10
data3 = rbind(data3_r0,data3_r1,data3_r2,data3_r3,data3_r4,data3_r5,data3_r6,data3_r7,data3_r8,data3_r9,data3_r10)
colnames(data3) = c("id1","id2","round")
data3$gameID = data1$gameId
data3$scoreA = data1$scoreA
data3$scoreB = data1$scoreB
data3$percentA = data1$percentA
data3$showScore = data1$showScore

if (i ==1)
	{
	data2_combined = data2
	data3_combined = data3
	}
	
else
	{
	data2_combined = rbind(data2_combined,data2)
	data3_combined = rbind(data3_combined,data3)
	}
print(i)
}

#save as R object 
ndata = data2_combined #node data
ldata = data3_combined #link data

#REPEAT: ndata and ldata
ldata$game = NA 
ldata[ldata$gameID=="D2013-11-07-01_4227","game"] = 1
ldata[ldata$gameID=="D2013-11-07-02_4257","game"] = 2
ldata[ldata$gameID=="D2013-11-07-03_4261","game"] = 3
ldata[ldata$gameID=="D2013-11-07-04_4289","game"] = 4
ldata[ldata$gameID=="D2013-11-08-03_4353","game"] = 5
ldata[ldata$gameID=="D2013-11-08-04_4354","game"] = 6
ldata[ldata$gameID=="D2013-11-11-01_4361","game"] = 7
ldata[ldata$gameID=="D2013-11-11-02_4385","game"] = 8
ldata[ldata$gameID=="D2013-11-11-03_4417","game"] = 9
ldata[ldata$gameID=="D2013-11-11-04_4421","game"] = 10
ldata[ldata$gameID=="D2013-11-12-01_4449","game"] = 11
ldata[ldata$gameID=="D2013-11-12-02_4461","game"] = 12 
ldata[ldata$gameID=="D2013-11-14-01-4546","game"] = 13
ldata[ldata$gameID=="D2013-11-15-01_4547","game"] = 14
ldata[ldata$gameID=="D2013-11-16-01_4577","game"] = 15
ldata[ldata$gameID=="D2013-11-16-02_4578","game"] = 16
ldata[ldata$gameID=="D2013-11-18-01_4581","game"] = 17
ldata[ldata$gameID=="D2013-11-18-02_4609","game"] = 18
ldata[ldata$gameID=="D2013-11-18-03_4621","game"] = 19
ldata[ldata$gameID=="D2013-11-18-04_4641","game"] = 20
ldata[ldata$gameID=="D2013-11-18-05_4673","game"] = 21
ldata[ldata$gameID=="D2013-11-18-06_4681","game"] = 22
ldata[ldata$gameID=="D2013-11-19-01_4705","game"] = 23
ldata[ldata$gameID=="D2013-11-19-02_4737","game"] = 24
ldata[ldata$gameID=="D2013-11-19-03_4741","game"] = 25
ldata[ldata$gameID=="D2013-11-19-04_4769","game"] = 26
ldata[ldata$gameID=="D2013-11-19-05_4781","game"] = 27
ldata[ldata$gameID=="D2013-11-20-01_4801","game"] = 28
ldata[ldata$gameID=="D2013-11-20-02_4833","game"] = 29
ldata[ldata$gameID=="D2013-11-20-03_4841","game"] = 30
ldata[ldata$gameID=="D2013-11-21-01_4866","game"] = 31
ldata[ldata$gameID=="D2013-11-21-02_4897","game"] = 32
ldata[ldata$gameID=="D2013-11-21-03_4901","game"] = 33
ldata[ldata$gameID=="D2013-11-21-04_4929","game"] = 34
ldata[ldata$gameID=="D2013-11-21-05_4941","game"] = 35
ldata[ldata$gameID=="D2013-11-21-07_4993","game"] = 36
ldata[ldata$gameID=="D2013-11-22-01_5001","game"] = 37
ldata[ldata$gameID=="D2013-11-22-02_5002","game"] = 38
ldata[ldata$gameID=="D2013-11-22-03_5003","game"] = 39
ldata[ldata$gameID=="D2013-11-26-04_5061","game"] = 40
ldata[ldata$gameID=="D2013-12-02-02_5153","game"] = 41
ldata[ldata$gameID=="D2013-12-02-03_5161","game"] = 42
ldata[ldata$gameID=="D2013-12-02-04_5163","game"] = 43
ldata[ldata$gameID=="D2013-12-02-05_5164","game"] = 44
ldata[ldata$gameID=="D2013-12-02-06_5185","game"] = 45
ldata[ldata$gameID=="D2013-12-05-01_5313","game"] = 46
ldata[ldata$gameID=="D2013-12-05-02_5321","game"] = 47
ldata[ldata$gameID=="D2013-12-06-01_5377","game"] = 48
ldata[ldata$gameID=="D2013-12-06-04_5441","game"] = 49
ldata[ldata$gameID=="D2013-12-09-01_5473","game"] = 50
ldata[ldata$gameID=="D2013-12-09-02_5481","game"] = 51
ldata[ldata$gameID=="D2013-12-09-03_5505","game"] = 52
ldata[ldata$gameID=="D2013-12-09-04_5537","game"] = 53
ldata[ldata$gameID=="D2013-12-09-05_5541","game"] = 54
ldata[ldata$gameID=="D2013-12-10-01_5569","game"] = 55
ldata[ldata$gameID=="D2013-12-10-02_5581","game"] = 56
ldata[ldata$gameID=="D2013-12-10-04_5633","game"] = 57
ldata[ldata$gameID=="D2013-12-10-05_5641","game"] = 58
ldata[ldata$gameID=="D2013-12-10-06_5665","game"] = 59
ldata[ldata$gameID=="D2013-12-11-01_5697","game"] = 60
ldata[ldata$gameID=="D2013-12-11-02_5701","game"] = 61
ldata[ldata$gameID=="D2013-12-11-03_5729","game"] = 62
ldata[ldata$gameID=="D2013-12-11-04_5741","game"] = 63
ldata[ldata$gameID=="D2013-12-11-05_5761","game"] = 64
ldata[ldata$gameID=="D2013-12-11-06_5793","game"] = 65
ldata[ldata$gameID=="D2013-12-12-01_5801","game"] = 66
ldata[ldata$gameID=="D2013-12-12-03_5861","game"] = 67
ldata[ldata$gameID=="D2013-12-13-01_5889","game"] = 68
ldata[ldata$gameID=="D2013-12-13-02_5901","game"] = 69
ldata[ldata$gameID=="D2013-12-13-03_5921","game"] = 70
ldata[ldata$gameID=="D2013-12-16-01_5953","game"] = 71
ldata[ldata$gameID=="D2013-12-16-02_5961","game"] = 72
ldata[ldata$gameID=="D2013-12-16-03_5985","game"] = 73
ldata[ldata$gameID=="D2013-12-17-01_6061","game"] = 74
ldata[ldata$gameID=="D2013-12-17-02_6081","game"] = 75
ldata[ldata$gameID=="D2013-12-17-03_6113","game"] = 76
ldata[ldata$gameID=="D2013-12-18-01_6221","game"] = 77
ldata[ldata$gameID=="D2013-12-18-02_6241","game"] = 78
ldata[ldata$gameID=="D2013-12-18-03_6273","game"] = 79
ldata[ldata$gameID=="D2013-12-19-03_6337","game"] = 80
ldata$costbenefit = ifelse(ldata$game %in% 1:80, 50100, 1020)
#REPEAT DONE 

ndata$game = NA 
ndata[ndata$gameID=="D2013-11-07-01_4227","game"] = 1
ndata[ndata$gameID=="D2013-11-07-02_4257","game"] = 2
ndata[ndata$gameID=="D2013-11-07-03_4261","game"] = 3
ndata[ndata$gameID=="D2013-11-07-04_4289","game"] = 4
ndata[ndata$gameID=="D2013-11-08-03_4353","game"] = 5
ndata[ndata$gameID=="D2013-11-08-04_4354","game"] = 6
ndata[ndata$gameID=="D2013-11-11-01_4361","game"] = 7
ndata[ndata$gameID=="D2013-11-11-02_4385","game"] = 8
ndata[ndata$gameID=="D2013-11-11-03_4417","game"] = 9
ndata[ndata$gameID=="D2013-11-11-04_4421","game"] = 10
ndata[ndata$gameID=="D2013-11-12-01_4449","game"] = 11
ndata[ndata$gameID=="D2013-11-12-02_4461","game"] = 12 
ndata[ndata$gameID=="D2013-11-14-01-4546","game"] = 13
ndata[ndata$gameID=="D2013-11-15-01_4547","game"] = 14
ndata[ndata$gameID=="D2013-11-16-01_4577","game"] = 15
ndata[ndata$gameID=="D2013-11-16-02_4578","game"] = 16
ndata[ndata$gameID=="D2013-11-18-01_4581","game"] = 17
ndata[ndata$gameID=="D2013-11-18-02_4609","game"] = 18
ndata[ndata$gameID=="D2013-11-18-03_4621","game"] = 19
ndata[ndata$gameID=="D2013-11-18-04_4641","game"] = 20
ndata[ndata$gameID=="D2013-11-18-05_4673","game"] = 21
ndata[ndata$gameID=="D2013-11-18-06_4681","game"] = 22
ndata[ndata$gameID=="D2013-11-19-01_4705","game"] = 23
ndata[ndata$gameID=="D2013-11-19-02_4737","game"] = 24
ndata[ndata$gameID=="D2013-11-19-03_4741","game"] = 25
ndata[ndata$gameID=="D2013-11-19-04_4769","game"] = 26
ndata[ndata$gameID=="D2013-11-19-05_4781","game"] = 27
ndata[ndata$gameID=="D2013-11-20-01_4801","game"] = 28
ndata[ndata$gameID=="D2013-11-20-02_4833","game"] = 29
ndata[ndata$gameID=="D2013-11-20-03_4841","game"] = 30
ndata[ndata$gameID=="D2013-11-21-01_4866","game"] = 31
ndata[ndata$gameID=="D2013-11-21-02_4897","game"] = 32
ndata[ndata$gameID=="D2013-11-21-03_4901","game"] = 33
ndata[ndata$gameID=="D2013-11-21-04_4929","game"] = 34
ndata[ndata$gameID=="D2013-11-21-05_4941","game"] = 35
ndata[ndata$gameID=="D2013-11-21-07_4993","game"] = 36
ndata[ndata$gameID=="D2013-11-22-01_5001","game"] = 37
ndata[ndata$gameID=="D2013-11-22-02_5002","game"] = 38
ndata[ndata$gameID=="D2013-11-22-03_5003","game"] = 39
ndata[ndata$gameID=="D2013-11-26-04_5061","game"] = 40
ndata[ndata$gameID=="D2013-12-02-02_5153","game"] = 41
ndata[ndata$gameID=="D2013-12-02-03_5161","game"] = 42
ndata[ndata$gameID=="D2013-12-02-04_5163","game"] = 43
ndata[ndata$gameID=="D2013-12-02-05_5164","game"] = 44
ndata[ndata$gameID=="D2013-12-02-06_5185","game"] = 45
ndata[ndata$gameID=="D2013-12-05-01_5313","game"] = 46
ndata[ndata$gameID=="D2013-12-05-02_5321","game"] = 47
ndata[ndata$gameID=="D2013-12-06-01_5377","game"] = 48
ndata[ndata$gameID=="D2013-12-06-04_5441","game"] = 49
ndata[ndata$gameID=="D2013-12-09-01_5473","game"] = 50
ndata[ndata$gameID=="D2013-12-09-02_5481","game"] = 51
ndata[ndata$gameID=="D2013-12-09-03_5505","game"] = 52
ndata[ndata$gameID=="D2013-12-09-04_5537","game"] = 53
ndata[ndata$gameID=="D2013-12-09-05_5541","game"] = 54
ndata[ndata$gameID=="D2013-12-10-01_5569","game"] = 55
ndata[ndata$gameID=="D2013-12-10-02_5581","game"] = 56
ndata[ndata$gameID=="D2013-12-10-04_5633","game"] = 57
ndata[ndata$gameID=="D2013-12-10-05_5641","game"] = 58
ndata[ndata$gameID=="D2013-12-10-06_5665","game"] = 59
ndata[ndata$gameID=="D2013-12-11-01_5697","game"] = 60
ndata[ndata$gameID=="D2013-12-11-02_5701","game"] = 61
ndata[ndata$gameID=="D2013-12-11-03_5729","game"] = 62
ndata[ndata$gameID=="D2013-12-11-04_5741","game"] = 63
ndata[ndata$gameID=="D2013-12-11-05_5761","game"] = 64
ndata[ndata$gameID=="D2013-12-11-06_5793","game"] = 65
ndata[ndata$gameID=="D2013-12-12-01_5801","game"] = 66
ndata[ndata$gameID=="D2013-12-12-03_5861","game"] = 67
ndata[ndata$gameID=="D2013-12-13-01_5889","game"] = 68
ndata[ndata$gameID=="D2013-12-13-02_5901","game"] = 69
ndata[ndata$gameID=="D2013-12-13-03_5921","game"] = 70
ndata[ndata$gameID=="D2013-12-16-01_5953","game"] = 71
ndata[ndata$gameID=="D2013-12-16-02_5961","game"] = 72
ndata[ndata$gameID=="D2013-12-16-03_5985","game"] = 73
ndata[ndata$gameID=="D2013-12-17-01_6061","game"] = 74
ndata[ndata$gameID=="D2013-12-17-02_6081","game"] = 75
ndata[ndata$gameID=="D2013-12-17-03_6113","game"] = 76
ndata[ndata$gameID=="D2013-12-18-01_6221","game"] = 77
ndata[ndata$gameID=="D2013-12-18-02_6241","game"] = 78
ndata[ndata$gameID=="D2013-12-18-03_6273","game"] = 79
ndata[ndata$gameID=="D2013-12-19-03_6337","game"] = 80
ndata$costbenefit = ifelse(ndata$game %in% 1:80, 50100, 1020)

dim(ndata) #15258 individual-rounds with 18 variables (see the codebook)
dim(ldata) #63309 tie-rounds with 10 variables (see the codebook)

save(ndata,file="node.Rdata") #node data saved (Please specify your directory)
save(ldata,file="link.Rdata") #link data saved (Please specify your directory)
