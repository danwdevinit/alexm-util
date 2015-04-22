#!/usr/bin/env python

import sys, os
import re
import scraperwiki
import urllib2, lxml.etree
import pyPdf
from optparse import OptionParser
import pandas as pd
import pdb
import itertools
import operator
from operator import itemgetter

parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="./tmp/Draft Detailed Estimates FY13-14 10.7.13.pdf",
                help="Input pdf name", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="./tmp/",
                        help="Output path. Default is './tmp/'",metavar="FOLDER")
(options, args) = parser.parse_args()

def trytext(el):
    children = el.getchildren()
    childLen = len(children)
    if childLen>0:
        grandchildren = children[0].getchildren()
        grandchildLen = len(grandchildren)
        if grandchildLen>0:
            if el.getchildren()[0].getchildren()[0].text!=None:
                return el.getchildren()[0].getchildren()[0].text
            elif el.getchildren()[0].text!=None:
                return el.getchildren()[0].text
            else:
                return el.text
        else:
            if el.getchildren()[0].text!=None:
                return el.getchildren()[0].text
            else:
                return el.text
    else:
        return el.text

def main():
    basename = os.path.basename(options.input)
    inputname, inputextension = os.path.splitext(basename)
    pdfdata = open(options.input,'r',1).read()
    xmldata = scraperwiki.pdftoxml(pdfdata)
    root = lxml.etree.fromstring(xmldata)
    pages = list(root)
    output = []
    pageLen = len(pages)
    #Cascade these down...
    ministry = ""
    vote = ""
    department = ""
    budgetType = ""
    programme = ""
    econOutput = ""
    #Cascade this up...
    sector = 0
    sectors = []
    for i in range(0,pageLen):
        page = pages[i]
        elLen = len(page)
        for j in range(0,elLen):
            el = page[j]
            if el.tag == "text":
                left = int(el.attrib['left'])
                right = int(el.attrib['left'])+int(el.attrib['width'])
                top = int(el.attrib['top'])
                font = int(el.attrib['font'])
                if font==0:
                    if trytext(el)!=None:
                        ministry = trytext(el)
                elif font==1:
                    if trytext(el)!=None:
                        vote = trytext(el)
                elif font==3:
                    if trytext(el)!=None:
                        department = trytext(el)
                elif font==4:            
                    if trytext(el)!=None:
                        budgetType = trytext(el)
                elif font==5:
                    if trytext(el)!=None:
                        programme = trytext(el)
                elif font==9:
                    if trytext(el)!=None:
                        econOutput = trytext(el)
                elif font==14:
                    sector+=1
                    try:
                        sectors.append(str.split(el.getprevious().text,"- ")[1])
                    except:
                        try:
                            sectors.append(el.getprevious().text)
                        except:
                            sectors.append("Unknown Sector")
                elif font==10:
                    if j<elLen-6:
                        el2 = page[j+1]
                        font2 = int(el2.attrib['font'])
                        el3 = page[j+2]
                        font3 = int(el3.attrib['font'])
                        el4 = page[j+3]
                        font4 = int(el4.attrib['font'])
                        el5 = page[j+4]
                        font5 = int(el5.attrib['font'])
                        el6 = page[j+5]
                        font6 = int(el6.attrib['font'])
                        el7 = page[j+6]
                        font7 = int(el7.attrib['font'])
                        #Pattern is 10 10 11 10 10 11 8 for:
                        #wage non-wage total wageEst non-wageEst totalEst econFunc
                        if font2==10 and font3==11 and font4==10 and font5==10 and font6==11 and font7==8:
                            #Wage 2012/13
                            obj = {}
                            obj['year']="2012/13"
                            obj['Government']="Central Government"
                            obj['sectorId']=sector
                            obj['Vote']=vote
                            obj['Ministry']=ministry
                            obj['Budget Type']=budgetType
                            obj['Department']=department
                            obj['Programme']=programme
                            obj['Budget Function']="Wage"
                            obj['Economic Function']=trytext(el7)
                            obj['Budget']=trytext(el)
                            obj['Output']=econOutput
                            output.append(obj)
                            #Non-Wage 2012/13
                            obj = {}
                            obj['year']="2012/13"
                            obj['Government']="Central Government"
                            obj['sectorId']=sector
                            obj['Vote']=vote
                            obj['Ministry']=ministry
                            obj['Budget Type']=budgetType
                            obj['Department']=department
                            obj['Programme']=programme
                            obj['Budget Function']="Non Wage"
                            obj['Economic Function']=trytext(el7)
                            obj['Budget']=trytext(el2)
                            obj['Output']=econOutput
                            output.append(obj)
                            #Wage 2013/14
                            obj = {}
                            obj['year']="2013/14"
                            obj['Government']="Central Government"
                            obj['sectorId']=sector
                            obj['Vote']=vote
                            obj['Ministry']=ministry
                            obj['Budget Type']=budgetType
                            obj['Department']=department
                            obj['Programme']=programme
                            obj['Budget Function']="Wage"
                            obj['Economic Function']=trytext(el7)
                            obj['Budget']=trytext(el4)
                            obj['Output']=econOutput
                            output.append(obj)
                            #Non-Wage 2013/14
                            obj = {}
                            obj['year']="2013/14"
                            obj['Government']="Central Government"
                            obj['sectorId']=sector
                            obj['Vote']=vote
                            obj['Ministry']=ministry
                            obj['Budget Type']=budgetType
                            obj['Department']=department
                            obj['Programme']=programme
                            obj['Budget Function']="Non Wage"
                            obj['Economic Function']=trytext(el7)
                            obj['Budget']=trytext(el5)
                            obj['Output']=econOutput
                            output.append(obj)
    #Add sectors in by sectorId
    for obj in output:
        obj['MTEF Sector'] = sectors[obj['sectorId']]
        del obj['sectorId']
    #Sort/Find Unique
    #getvals = operator.itemgetter('page','type','top','val')
    #output.sort(key=getvals)
    #unique = []
    #for k, g in itertools.groupby(output,getvals):
    #    unique.append(list(g)[0])
    #output = unique[:]
    data = pd.DataFrame(output)
    data.to_csv(options.output+inputname+".csv",encoding="utf-8",index=False)
    print("Done.")

main()