'''
Created on 2013/06/06

@author: Hirokazu Shirado

Edited 2017/4/28 by Chris German for Happiness Study

Edited 2023 by Hiroyasu Ando for the Isolation Study
'''

import csv
from datetime import datetime as dt
import Game
import networkx as nx
import sys
import matplotlib.pyplot as plt

def preProcess(filename):

    dataFile = csv.reader(open(filename, newline=''))

    next(dataFile)

    gameId = filename.split("/")[-1]
    gameId = gameId.split(".")[0]

    IDlist = []
    Move = {}
    game = Game.Game(gameId)
    id = 0

    for data in dataFile:

        data = list(map(lambda x:x.strip(), data))
        uid = int(data[0])
        event = data[1]
        datetime = dt.strptime(data[2].replace("T"," ").replace("Z", ""), "%Y-%m-%d %H:%M:%S:%f")
        dname = data[3]
        dvalue = data[4]

        if uid not in IDlist:
            IDlist.append(uid)
            id += 1
            Move[id] = {'event': event, 'time': datetime, dname: dvalue}
        else:
            Move[id][dname] = dvalue

    for uid, d in Move.items():

        event = d['event']

        if event == "initParameters":
            game.setDensity(float(d['connectivity']))
            game.setNumRounds(int(d['nRounds']))
            game.setRewiringRate(float(d['k']))
            game.setCooperationBalance(int(d['coc']), int(d['po']))
            game.setDisparityParameters(float(d['percentA']), int(d['scoreA']), int(d['scoreB']), d['showScore'])

            # Added 2023/06/05

            # Highest
            game.setHighest((d['highDegreeOn']))
            # Lowest
            game.setLowest((d['lowDegreeOn']))

    g = nx.Graph()
    lastRound = 0
    start = False

    for uid, d in sorted(Move.items()):

        event = d['event']
        time = d['time']

        if event == "initStart":
            game.setStartDate(time)
        
        elif event == "clientLogIn":
            game.newPlayer(d["clientId"], d["ipAddress"])
        
        elif event == "EndPractice2Start":
            start = True

        elif event == "InitialScore":
            game.setPlayerInitialScore(game.PID[d['pid']], int(d['score']))
            g.add_node(game.PID[d['pid']])
        
        elif event == "Connected":
            g.add_edge(game.PID[d['playerId1']], game.PID[d['playerId2']])
        
        elif event == "CooperationStart" and start:
            game.addGraph(lastRound, g.copy())
            lastRound += 1
        
        # Added
        elif event == "StartMakingChoices" and start:
            game.setPlayerCooperationTime(lastRound, time, game.PID[d['pid']])

        # This measures pointless time. 
        elif event == "RewiringStart" and start:
            game.setRewiringTime(lastRound, time)
        
        elif event == "RewiringResultsStart" and start:
            newConnections = game.makeConnections(lastRound)
            for s in newConnections:
                pair = list(s)

                # Added
                if (pair[0] in g.nodes()) and (pair[1] in g.nodes()):
                    g.add_edge(pair[0], pair[1])
        
        elif event == "cooperationEvent" and start:
            if d['action'] == 'cooperate':
                game.setPlayerStrategy(int(d['round']), game.PID[d['pid']], 'C', time)
            #
            # Added
            #
            elif d['action'] == 'punish':
                game.setPlayerStrategy(int(d['round']), game.PID[d['pid']], 'P', time)
            else:
                game.setPlayerStrategy(int(d['round']), game.PID[d['pid']], 'D', time)
        
        # Added 2023/06/05

        # Every round
        elif event == "reportIsolation" and start:
            game.setPlayerIsolation(int(d['round']), game.PID[d['pid']],d['isolation'])
        
        # Initial round
        elif event == "initialReportIsolation" and start:
            game.setPlayerInitialIsolation(int(d['round']), game.PID[d['pid']],d['isolation'])
        
        elif event == "q5":
            game.setGender(game.PID[d['pid']],d['gender'])
        
        elif event == "q6":
            game.setAge(game.PID[d['pid']],d['age'])
        
        elif event == "rewiringEvent" and start:
            if d['action'] == 'breakConnection':
                if (g.has_edge(game.PID[d['pid']], game.PID[d['nid']])):
                    g.remove_edge(game.PID[d['pid']], game.PID[d['nid']])
                    #game.setPlayerBreakConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)
                # Added
                game.setPlayerBreakConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)
            elif d['action'] == 'makeConnection':
                game.setPlayerMakeConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)
            # Added
            elif d['action'] == 'doNotBreakConnection':
                game.setPlayerNotBreakConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)
            # Added
            elif d['action'] == 'doNotMakeConnection':
                game.setPlayerNotMakeConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)

            #elif d['action'] == 'maintainConnection':
                #game.setPlayerMaintainConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)
        
        elif event == "dropped" and start:
            if game.PID[d['pid']] in g:
                g.remove_node(game.PID[d['pid']])

    game.addGraph(lastRound, g.copy())

    return game

if __name__ == '__main__':

    argv = sys.argv

    if len(argv) < 2:
        print("Usage: # python %s filename" % argv[0])
        quit()
    
    input_file = argv[1]

    game = preProcess(input_file)

    game.writeJSON()



