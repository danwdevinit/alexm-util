#!/usr/bin/env python

#Import
import sys, os
from optparse import OptionParser
import subprocess
import math
import csv
import numpy as np
np.seterr(divide='ignore', invalid='ignore')
import random

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
    for x in range(0,len(dataObj)):
        step = minStrike+((maxStrike-minStrike)/(len(dataObj)-1))*x
        stockRange.append(step)
    
    #Calculate Payoff Matrix
    FArray = []
    PArray = []
    for obj in dataObj:
        Frow = []
        excise = obj["strike"]
        price = obj["price"]
        for stock in stockRange:
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
    rank = np.linalg.matrix_rank(F)
    print "Found "+str(rank)+" independent rows in dataset. Determining combinations..."
    slice = range(0,rank)
    while np.linalg.det(F[slice,:][:,slice])==0:
        slice = random.sample(xrange(rows),rank)
    print "Found one solution for full dataset, checking feasibility of security prices... If not feasible: trying random solutions..."
    arrow = F[slice,:][:,slice].I*P[slice,:]
    while rank>=1 and min(arrow)<0:
        counter = 100
        while min(arrow)<0 and counter>1:
            if len(xrange(int(maxStrike)))<rows:
                stockRange = []
                for x in range(0,len(dataObj)):
                    step = minStrike+((maxStrike-minStrike)/(len(dataObj)-1))*x
                    stockRange.append(step)
            else:
                stockRange = random.sample(xrange(int(maxStrike)),rows)
            FArray = []
            PArray = []
            for obj in dataObj:
                Frow = []
                excise = obj["strike"]
                price = obj["price"]
                for stock in stockRange:
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
            timeout = 100000
            while np.linalg.det(F[slice,:][:,slice])==0 and timeout>1:
                slice = random.sample(xrange(rows),rank)
                timeout -= 1
            if not np.linalg.det(F[slice,:][:,slice])==0:
                arrow = F[slice,:][:,slice].I*P[slice,:]
            counter -= 1
            sys.stdout.write('.')
            sys.stdout.flush()
        rank -= 1
        sys.stdout.write('\n')
        print "No solutions found after 100 random combinations... Dropping one independent variable..."
    if timeout==0:
        print "Sorry, no feasible solutions found, please try again..."
    else:
        sliceRange = []
        for x in slice:
            sliceRange.append(stockRange[x])
        arrow = F[slice,:][:,slice].I*P[slice,:]
        sum = 0
        for x in list(arrow):
            sum+=x[0,0]
        results = {}
        counter = 0
        for x in arrow:
            results[int(round(sliceRange[counter],2))] = round((x[0,0]/sum)*100,2)
            counter += 1
        for x in sorted(results):
                print "For "+str(x)+": "+str(results[x])+"%"
if __name__ == "__main__":
   main(sys.argv[1:])