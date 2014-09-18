#!/usr/bin/env python

#Import
import sys, os
from optparse import OptionParser
import subprocess
import math
import csv
import numpy as np
import random

def main(argv):
    #Parse options
    parser = OptionParser()
    parser.add_option("-i", "--input", dest="input",
                        help="Input csv file path", metavar="FILE")
    parser.add_option("-m", "--month", dest="month",
                        help="Three letter month abbreviation. E.g. 'OCT'")
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
                    if row[0][:3]==options.month and row[0][-3:]=="100":
                        inRange = 1
                    if row[0][:3]!=options.month or row[0][-3:]!="100":
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
    print "Matrix F's Determinate:"
    print np.linalg.det(F)
    if np.linalg.det(F)==0:
        print "F is not invertible..."
if __name__ == "__main__":
   main(sys.argv[1:])