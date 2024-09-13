'''
Created on 2013/06/06

@author: Hirokazu Shirado

Edited 2023 by Hiroyasu Ando for the Isolation Study
'''

import Player
import json
from networkx.readwrite import json_graph

class Game:

    def __init__(self, gameId):
        self.gameId = gameId
        self.Graphs = {}
        self.LoginPlayers = {}
        self.Players = {}
        self.Drops = {}
        self.MakeCandidates = {}
        self.CooperationTime = {}
        self.RewiringTime = {}
        self.numPID = 0
        self.PID = {}
    
    def setDensity(self, density):
        self.density = density

    def setNumRounds(self, numRounds):
        self.numRounds = numRounds
    
    def setRewiringRate(self, rewiringRate):
        self.rewiringRate = rewiringRate
    
    def setCooperationBalance(self, cost, profit):
        self.cost = cost
        self.profit = profit
    
    def setDisparityParameters(self, percentA, scoreA, scoreB, showScore):
        self.percentA = percentA
        self.scoreA = scoreA
        self.scoreB = scoreB
        self.showScore = showScore
    
    # Added 2023/06/05
    def setHighest(self, highest):
        self.highest = highest
    
    # Added 2023/06/05
    def setLowest(self, lowest):
        self.lowest = lowest
    
    def setStartDate(self, date):
        self.startDate = date

    def newPlayer(self, playerId, ipAddress):
        if playerId not in self.PID.keys():
            self.PID[playerId] = "p" + str(self.numPID)
            self.LoginPlayers[self.PID[playerId]] = Player.Player(self.PID[playerId], self.gameId, self.cost, self.profit, ipAddress, False)
            self.numPID = self.numPID + 1
    
    def setPlayerInitialScore(self, playerId, score):
        self.Players[playerId] = self.LoginPlayers[playerId]
        self.Players[playerId].setInitialScore(score)
    
    def addGraph(self, round, graph):

        self.Graphs[round] = graph

        # Added
        # graph.nodes()
        if round == 0:
            cooperators = []
            punishers = []
        else:
            cooperators = [x for x in self.Graphs[round-1].nodes() if x in self.Players and self.Players[x].getStrategy(round) == 'C']
            
            # Added
            punishers = [x for x in self.Graphs[round-1].nodes() if x in self.Players and self.Players[x].getStrategy(round) == 'P']
            #print(punishers)

        for playerId in graph.nodes():

            neighbors = graph.neighbors(playerId)

            # Added
            neighbors = [x for x in neighbors]

            if playerId in self.Players:
                self.Players[playerId].setNeighbors(round, neighbors)
                self.Players[playerId].setCooperators(round, cooperators)

                # Added
                self.Players[playerId].setPunishers(round, punishers)

                self.Players[playerId].setPayoff(round)
    
    # Added
    def setPlayerCooperationTime(self, round, time, playerId):
        self.Players[playerId].setCooperationTime(round, time)

    def setRewiringTime(self, round, time):
        self.RewiringTime[round] = time
    
    def makeConnections(self, round):
        newConnections = []
        for p, C in self.MakeCandidates.items():
            for c in C:
                if c in self.MakeCandidates and p in self.MakeCandidates[c]:
                    if set([p, c]) not in newConnections:
                        newConnections.append(set([p, c]))
                        self.Players[p].setMakeLink(round, c)
                        self.Players[c].setMakeLink(round, p)
        self.MakeCandidates = {}
        return newConnections
    
    def setPlayerStrategy(self, round, playerId, strategy, time, practice=False):

        if playerId not in self.Players:
            self.Players[playerId] = self.LoginPlayers[playerId]
            if practice:
                self.Players[playerId].setInitialScore(self.practiceScore)
                for i in range(1, round):
                    print("Players joined during practice rounds. Something is wrong with data")
                    self.Players[playerId].TotalPayoff[i] = self.practiceScore
            else:
                self.Players[playerId].setInitialScore(self.scoreA)
                for i in range(1, round):
                    print("Players joined during actual rounds. Something is wrong with data")
                    self.Players[playerId].TotalPayoff[i] = self.scoreA
          
        if isinstance(time, int):
            print("Time is int. Something is wrong with data")
            self.Players[playerId].setStrategy(round, strategy, "")
        else:
            #self.Players[playerId].setStrategy(round, strategy, time - self.CooperationTime[round])
            # Added
            self.Players[playerId].setStrategy(round, strategy, time)

    # Added 2023/06/05

    # Every round
    def setPlayerIsolation(self, round, playerId, isolation):
        self.Players[playerId].setIsolation(round, isolation)

    # Initial round
    def setPlayerInitialIsolation(self, round, playerId, isolation):
        self.Players[playerId].setInitialIsolation(round, isolation)
    
    def setGender(self,playerId,gender):
        self.Players[playerId].setGender(gender)
    
    def setAge(self,playerId,age):
        self.Players[playerId].setAge(age)
    
    def setPlayerBreakConnection(self, round, playerId, targetId, time):
        if (playerId) not in self.Players: return
        self.Players[playerId].setBreakLink(round, targetId, time - self.RewiringTime[round])
        self.Players[targetId].setBeBrokenLink(round, playerId)
    
    def setPlayerMakeConnection(self, round, playerId, targetId, time):
        if playerId not in self.Players: return
        if playerId not in self.MakeCandidates:
            self.MakeCandidates[playerId] = []
        self.MakeCandidates[playerId].append(targetId)
        self.Players[playerId].setTryMakeLink(round, targetId, time - self.RewiringTime[round])
    
    # Added
    def setPlayerNotBreakConnection(self, round, playerId, targetId, time):
        self.Players[playerId].setNotBreakLink(round, targetId, time - self.RewiringTime[round])
    
    # Added
    def setPlayerNotMakeConnection(self, round, playerId, targetId, time):
        self.Players[playerId].setNotMakeLink(round, targetId, time - self.RewiringTime[round])
    
    def writeJSON(self, folder = ""):

        Anony = {}
        num = 0

        for playerId in self.Players.keys():
            Anony[playerId] = num
            num = num + 1
        
        DG = {}

        for round in range(self.numRounds + 1):
            graph = self.Graphs[round]
            graph.graph["round"] = round

            print('---------------------')

            for n in graph.nodes():

                #print(n)

                graph.nodes[n]["initScore"] = self.Players[n].initialScore
                graph.nodes[n]["ipAddress"] = self.Players[n].address
                graph.nodes[n]["gender"] = self.Players[n].Gender

                graph.nodes[n]["age"] = self.Players[n].Age

                # Initial
                
                graph.nodes[n]["initialIsolation"] = self.Players[n].InitialIsolation

                #print(graph.nodes[n]["initialIsolation"])

                if round > 0:
                    graph.nodes[n]["payoff"] = self.Players[n].Payoff[round]
                    graph.nodes[n]["cumulativePayoff"] = self.Players[n].TotalPayoff[round]

                    print(graph.nodes[n]["cumulativePayoff"])

                    if  round not in self.Players[n].Strategy:
                        graph.nodes[n]["behavior"] =""
                        graph.nodes[n]["behaviorTime"] = ""
                    else:
                        graph.nodes[n]["behavior"] = self.Players[n].Strategy[round]

                        if round not in self.Players[n].StrategyTime:

                            # Added
                            graph.nodes[n]["behaviorTime"] = ""
                        else:
                            graph.nodes[n]["behaviorTime"] = self.Players[n].StrategyTime[round].seconds * 1000 + (self.Players[n].StrategyTime[round].microseconds)/1000
                    
                    #print(graph.nodes[n]["behavior"])
                    #print(graph.nodes[n]["behaviorTime"])
                    
                    graph.nodes[n]["makeLink"] = self.getLinkTargets(self.Players[n].TryMakeLink, round)
                    graph.nodes[n]["notMakeLink"] = self.getLinkTargets(self.Players[n].NotMakeLink, round)
                    graph.nodes[n]["breakLink"] = self.getLinkTargets(self.Players[n].BreakLink, round)
                    graph.nodes[n]["notBreakLink"] = self.getLinkTargets(self.Players[n].NotBreakLink, round)

                    #print(graph.nodes[n]["makeLink"])
                    #print(graph.nodes[n]["notMakeLink"])
                    #print(graph.nodes[n]["breakLink"])
                    #print(graph.nodes[n]["notBreakLink"])
                    
                    # Added 2023/06/05

                    # Feel isolated
                    if round not in self.Players[n].Isolation:
                        graph.nodes[n]["isolation"] = ""
                    else:
                        graph.nodes[n]["isolation"] = self.Players[n].Isolation[round]
                    
                    #print(graph.nodes[n]["isolation"])

            for e in graph.edges():
                graph.edges[(e[0],e[1])]['id'] = e

            roundName = round
            DG[roundName] = json_graph.node_link_data(graph)
        
        Game = {}
        Game["gameId"] = self.gameId
        Game["cost"] = self.cost
        Game["profit"] = self.profit
        Game["percentA"] = self.percentA
        Game["scoreA"] = self.scoreA
        Game["scoreB"] = self.scoreB
        Game["initDensity"] = self.density
        Game["rewiringRate"] = self.rewiringRate
        Game["showScore"] = self.showScore

        # Added 2023/06/05

        # Highest
        Game["highest"] = self.highest

        # Lowest
        Game["lowest"] = self.lowest

        Game["result"] = DG

        json.dump(Game, open(self.gameId + '.json', 'w'))

    def getLinkTargets(self, dict, round):
        if round in dict:
            return dict[round]
        else:
            return []
    
    












                


        



    



























    


    

    

