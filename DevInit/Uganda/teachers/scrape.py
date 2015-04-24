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
parser.add_option("-i", "--input", dest="input", default="../tmp/ABIM BFP_1.pdf",
                help="Input pdf name", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="../tmp/",
                        help="Output path. Default is './tmp/'",metavar="FOLDER")
parser.add_option("-d", "--debug", dest="debug", default=False,
                        help="Debug",metavar="BOOLEAN")
(options, args) = parser.parse_args()

syms = ['\\', '|', '/', '-']
bs = '\b'
spinIndex = 0
def spin():
    global spinIndex
    spinIndex = 0 if spinIndex>3 else spinIndex
    sym = syms[spinIndex]
    sys.stdout.write("\b%s" % sym)
    sys.stdout.flush()
    spinIndex+=1

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
    #Before writing, try pdftohtml NAME.pdf -xml NAME.xml
    basename = os.path.basename(options.input)
    inputname, inputextension = os.path.splitext(basename)
    sys.stdout.write("Reading "+basename+"... This may take a while....")
    pdfdata = open(options.input,'r',1).read()
    xmldata = scraperwiki.pdftoxml(pdfdata)
    root = lxml.etree.fromstring(xmldata)
    pages = list(root)
    output = []
    pageLen = len(pages)
    for i in range(0,pageLen):
        if i%100==0:
            sys.stdout.write("\n")
            sys.stdout.write("On page "+str(i)+" out of "+str(pageLen)+"....")
            sys.stdout.flush()
        spin()
        page = pages[i]
        elLen = len(page)
        for j in range(0,elLen):
            el = page[j]
            if el.tag == "text":
                left = int(el.attrib['left'])
                top = int(el.attrib['top'])
                font = int(el.attrib['font'])
                obj = {}
                obj['page'] = i
                obj['top'] = top
                obj['left'] = left
                obj['text'] = trytext(el)
                obj['font'] = font
                output.append(obj)
    #Sort/Find Rows
    getvals = operator.itemgetter('page','top')
    rowvals = operator.itemgetter('left')
    output.sort(key=getvals)
    rows = []
    for k, g in itertools.groupby(output,getvals):
        row = list(g)
        row.sort(key=rowvals)
        rows.append(row)
    results = []
    rowLen = len(rows)
    errs = []
    #Cascade down
    vote = ""
    district = ""
    subcountry = ""
    costCentre = ""
    for i in range(0,rowLen):
        row = rows[i]
        if len(row)==1 and row[0]['text'][:6]=="Vote: ":
            vote = row[0]['text'][6:]
        if len(row)==1 and row[0]['font']==0:
            district = row[0]['text']
        if len(row)==1 and row[0]['text'][:48]=='Subcounty / Town Council / Municipal Division : ':
            subcountry = row[0]['text'][48:]
        if len(row)==1 and row[0]['text'][:14]=="Cost Centre : ":
            costCentre = row[0]['text'][14:]
        if len(row)==6:
            if row[0]['font']==5 and row[1]['font']==5 and row[2]['font']==5 and row[3]['font']==5 and row[4]['font']==5 and row[5]['font']==5:
                if row[0]['left']==39 and row[1]['left']==154 and row[2]['left']==342 and row[3]['left']==509 and row[4]['left']==629 and row[5]['left']==737:
                    obj={}
                    obj["Vote"]=vote
                    obj["District"]=district
                    obj["Sub-county"]=subcountry
                    obj["Cost Centre"]=costCentre
                    obj["File Number"]=row[0]['text']
                    obj["Staff Name"]=row[1]['text']
                    obj["Staff Title"]=row[2]['text']
                    obj["Salary Scale"]=row[3]['text']
                    obj["Monthly Gross Salary"]=row[4]['text']
                    obj["Annual Gross Salary"]=row[5]['text']
                    results.append(obj)
                    
    if len(errs)>0:
        sys.stdout.write("\n")
        sys.stdout.write(str(len(errs))+" odd rows omitted. Debug (-d true) for more info.")
    if options.debug:
        errData = pd.DataFrame(errs)
        errData.to_csv(options.output+"debug.csv",encoding="utf-8",index=False)
        pdb.set_trace()
    data = pd.DataFrame(results)
    data.to_csv(options.output+inputname+".csv",encoding="utf-8",index=False)
    sys.stdout.write("\n")
    print("Done.")

main()