'''
Created on 2013/06/06

@author: Hirokazu Shirado

Edited 2023 by Hiroyasu Ando for the Isolation Study
'''

class Player:

    def __init__(self, id, gameId, cost, profit, address, isAI=False):

        self.Id = id
        self.gameId = gameId
        self.Strategy = {}

        # Added
        self.CooperationTime = {}

        self.StrategyTime = {}
        self.MakeLink = {}
        self.TryMakeLink = {}
        self.NotMakeLink = {}
        self.BreakLink = {}
        self.NotBreakLink = {}
        self.TryMakeLinkTime = {}
        self.NotMakeLinkTime = {}
        self.BreakLinkTime = {}
        self.NotBreakLinkTime = {}

        self.BeBrokenLink = {}
        self.Neighbors = {}
        self.Cooperators = {}

        # Added
        self.Punishers = {}

        self.CooperativeNeighbors = {}
        self.Payoff = {}
        self.TotalPayoff = {}
        self.cost = cost
        self.profit = profit
        self.address = address
        self.isAI = isAI

        # Added 2023/06/05
        
        # Every round
        self.Isolation = {}

        # Initial round
        self.InitialIsolation = ''

        self.Gender = ''
        self.Age = ''
    
    def setInitialScore(self, initialScore):
        self.initialScore = initialScore
        self.TotalPayoff[0] = initialScore
    
    # Added
    def setCooperationTime(self, round, time):
        self.CooperationTime[round] = time
        #print(self.CooperationTime)
    
    def getStrategy(self, round):
        if round in self.Strategy:
            return self.Strategy[round]
        return None
    
    def setNeighbors(self, round, neighbors):
        self.Neighbors[round] = neighbors
    
    def setCooperators(self, round, cooperators):
        self.Cooperators[round] = cooperators
    
    # Added
    def setPunishers(self, round, punishers):
        self.Punishers[round] = punishers
        #print(self.Punishers)

    def setPayoff(self, round):

        payoff = 0

        if round != 0:
            if round in self.Strategy and (round-1) in self.Neighbors:
                if self.Strategy[round] == 'C':
                    payoff -= self.cost * len(list(self.Neighbors[round-1]))
                # Added
                elif self.Strategy[round] == 'P':
                    payoff -= self.cost * len(list(self.Neighbors[round-1]))
                    #print(8888888888888)

                c_neighbors = [x for x in self.Neighbors[round-1] if x in self.Cooperators[round]]

                # Added
                p_neighbors = [x for x in self.Neighbors[round-1] if x in self.Punishers[round]]
                #print(p_neighbors)

                payoff += self.profit * len(c_neighbors)

                # Added
                payoff -= self.profit * len(p_neighbors)

            self.Payoff[round] = payoff

            if (round - 1) not in self.TotalPayoff:
                maxRound = max(self.TotalPayoff.keys())
                for i in range(maxRound, round):
                    self.TotalPayoff[i] = self.TotalPayoff[maxRound]
            self.TotalPayoff[round] = self.TotalPayoff[round-1] + payoff
    
    def setMakeLink(self, round, targetId):
        if round not in self.MakeLink:
            self.MakeLink[round] = []
        self.MakeLink[round].append(targetId)
    
    def setStrategy(self, round, strategy, time):
        self.Strategy[round] = strategy
        #self.StrategyTime[round] = time
        
        # Added
        self.StrategyTime[round] = time - self.CooperationTime[round]

    # Added 2023/06/05

    # Every round
    def setIsolation(self, round, isolation):
        self.Isolation[round] = isolation

    # Initial round
    def setInitialIsolation(self, round, isolation):
        self.InitialIsolation = isolation
    
    def setGender(self, gender):
        self.Gender = gender
    
    def setAge(self,age):
        self.Age = age
    
    def setBreakLink(self, round, targetId, time):
        if round not in self.BreakLink:
            self.BreakLink[round] = []
        self.BreakLink[round].append(targetId)
        if round not in self.BreakLinkTime:
            self.BreakLinkTime[round] = []
        self.BreakLinkTime[round].append(time)
    
    def setBeBrokenLink(self, round, targetId):
        if round not in self.BeBrokenLink:
            self.BeBrokenLink[round] = []
        self.BeBrokenLink[round].append(targetId)
    
    def setTryMakeLink(self, round, targetId, time):
        if round not in self.TryMakeLink:
            self.TryMakeLink[round] = []
        self.TryMakeLink[round].append(targetId)

        if round not in self.TryMakeLinkTime:
            self.TryMakeLinkTime[round] = []
        self.TryMakeLinkTime[round].append(time)

    def setNotMakeLink(self, round, targetId, time):
        if round not in self.NotMakeLink:
            self.NotMakeLink[round] = []
        self.NotMakeLink[round].append(targetId)

        if round not in self.NotMakeLinkTime:
            self.NotMakeLinkTime[round] = []
        self.NotMakeLinkTime[round].append(time)
    
    def setNotBreakLink(self, round, targetId, time):
        if round not in self.NotBreakLink:
            self.NotBreakLink[round] = []
        self.NotBreakLink[round].append(targetId)

        if round not in self.NotBreakLinkTime:
            self.NotBreakLinkTime[round] = []
        self.NotBreakLinkTime[round].append(time)




        









            


                










        





    














































