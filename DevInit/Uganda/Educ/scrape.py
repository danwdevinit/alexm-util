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
parser.add_option("-i", "--input", dest="input", default="./tmp/Uganda Primary Leaving Examinations Results 2014.pdf",
                help="Input pdf name", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="./tmp/",
                        help="Output path. Default is './tmp/'",metavar="FOLDER")
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
    for i in range(1,pageLen):
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
    #Cascade down
    school = ""
    district = ""
    data = False
    notSecondRow = True
    rowLen = len(rows)
    errs = []
    for i in range(0,rowLen):
        row = rows[i]
        if row[0]['text']=="SCHOOL:":
            [school, district] = map(str.strip,str.split(rows[i][1]['text'],"DISTRICT:"))
        if row[0]['text']=="DIV-1":
            #Detect footer, end data read
            data = False
        if data:
            if notSecondRow:
                if i<rowLen-1:
                    row2 = rows[i+1]
                realRow = row+row2
                realRow.sort(key=rowvals)
                rowArr = []
                for obj in realRow:
                    rowArr.append(obj['text'])
                #Split concatenated rows into 2
                if len(rowArr)==18:
                    rowArr1 = rowArr[:9]
                    rowArr2 = rowArr[9:]
                    rowArr1.insert(0, school)
                    rowArr1.insert(0, district)
                    rowArr2.insert(0, school)
                    rowArr2.insert(0, district)
                    results.append(rowArr1)
                    results.append(rowArr2)
                    notSecondRow = False
                #Unless it's already a solo row
                elif len(rowArr)==9:
                    rowArr.insert(0, school)
                    rowArr.insert(0, district)
                    results.append(rowArr)
                    notSecondRow = False
                #Detect page break, reset toggles
                elif rowArr[0]=="Printed On:":
                    data = False
                    notSecondRow = True
                #And if we're missing something, add it to errors
                else:
                    errs.append(rowArr)
            else:
                notSecondRow = True
        if row[0]['text']=="No.":
            #Detect header, start data read
            data = True
    cols = ["DISTRICT","SCHOOL","NO.","NAME","M/F","ENG","SCI","SST","MAT","AGG","DIV"]
    data = pd.DataFrame(results,columns=cols)
    data.to_csv(options.output+inputname+".csv",encoding="utf-8",index=False)
    sys.stdout.write("\n")
    print("Done.")

main()