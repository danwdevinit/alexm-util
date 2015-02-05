#!/usr/bin/env python

#Import
import sys, os
from optparse import OptionParser
import subprocess
import math
import csv
import numpy as np
np.seterr(divide='ignore', invalid='ignore')
import itertools

def main(argv):
    #Parse options
    parser = OptionParser()
    parser.add_option("-i", "--input", dest="input",
                        help="Input csv file path", metavar="FILE")
    parser.add_option("-m", "--month", dest="month",
                        help="Three letter month abbreviation and last 2 digits of year. E.g. 'OCT 14'")
    (options, args) = parser.parse_args()
    
    #Read CSV
    data = []
    underlying = 0
    inRange = 0
    underlyingRange = 0
    with open(options.input,'rb') as csvfile:
        csvreader = csv.reader(csvfile, delimiter=',')
        for row in csvreader:
            if underlyingRange==1:
                underlying+=float(row[0])
            if len(row)>0:
                if row[0]!="LAST":
                    underlyingRange = 0
                if row[0]=="LAST":
                    underlyingRange = 1
                if len(row[0])>10:
                    if row[0][:6]==options.month and row[0][-3:]=="100":
                        inRange = 1
                    if row[0][:6]!=options.month or row[0][-3:]!="100":
                        inRange = 0
            if inRange==1:
                data.append(row)
    
    #Drop extra columns
    data.pop()
    data.pop(0)
    data.pop(0)
    
    #Grab Strikes, Call Prices, and Put Prices
    dataObj = []
    stockRange = []
    for row in data:
        if row[2]!="":
            obj = {}
            obj["call"] = 1
            obj["strike"] = float(row[10])
            obj["price"] = float(row[2])
            dataObj.append(obj)
        if row[15]!="":
            obj = {}
            obj["call"] = 0
            obj["strike"] = float(row[10])
            obj["price"] = float(row[15])
            dataObj.append(obj)
    maxStrike = max(obj["strike"] for obj in dataObj)
    minStrike = min(obj["strike"] for obj in dataObj)
    print "Max states: "+str(len(dataObj))+"; Min strike: "+str(minStrike)+"; Max strike: "+str(maxStrike)+"; Underlying: "+str(underlying)+";"
    statesInput = int(raw_input("How many states would you like to estimate: "))
    states = []
    lastState = float(raw_input("Please pick the lowest state: "))
    states.append(lastState)
    for x in range(1,statesInput):
        state = float(raw_input("Please pick the next state, greater than "+str(lastState)+": "))
        states.append(state)
        lastState = state
    print "Please wait a moment..."
    
    #Calculate Payoff Matrix
    FArray = []
    PArray = []
    for obj in dataObj:
        Frow = []
        excise = obj["strike"]
        price = obj["price"]
        for stock in states:
            if obj["call"]==1:
                f = max(stock-excise,0)
                Frow.append(f)
            if obj["call"]==0:
                f = max(excise-stock,0)
                Frow.append(f)
        FArray.append(Frow)
        PArray.append(price)
    F = np.matrix(FArray)
    P = np.matrix(PArray).T
    (rows, cols) = F.shape
    
    #Every possible combination solution... This takes a while...
    combinations = list(itertools.combinations(range(0,rows),statesInput))
    slices = []
    arrows = []
    for slice in combinations:
        if np.linalg.det(F[slice,:])!=0:
            slices.append(slice)
    for slice in slices:
        arrow = F[slice,:].I*P[slice,:]
        if max(arrow)<=1 and min(arrow)>=0:
            arrows.append(arrow)
    if len(arrows)==0:
        print "Sorry, no feasible solutions..."
    else:
        inducedProb = {}
        for arrow in arrows:
            sum = 0
            for price in arrow:
                sum+=price
            if sum>0 and sum<1.1:
                for x in range(0,statesInput):
                    price = arrow[x]
                    percent = (price/sum)*100
                    if not x in inducedProb:
                        inducedProb[x] = [percent[0,0]]
                    else:
                        inducedProb[x].append(percent[0,0])
        print "Found "+str(len(inducedProb))+" feasible solutions..."
        for x in inducedProb:
            print "For "+str(states[x])+": "+str(round(min(inducedProb[x]),2))+"% - "+str(round(max(inducedProb[x]),2))+"%; Avg: "+str(round(np.sum(inducedProb[x])/float(len(inducedProb[x])),2))+"%;"
        printAll = raw_input("Print all "+str(len(arrows))+" solutions (Y/N)? ")
        if printAll.strip().upper()=="Y":
            for x in arrows:
                print x
                print ""
if __name__ == "__main__":
   main(sys.argv[1:])