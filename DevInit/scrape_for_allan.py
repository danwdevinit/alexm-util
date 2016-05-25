#!/usr/bin/env python

import sys, os
import re
import urllib2, lxml.etree
from optparse import OptionParser
import pdb
import itertools
import operator
import csv
from operator import itemgetter
import glob

parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="D:/Documents/2014-15.pdf",
                help="Input pdf name", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="./",
                        help="Output path. Default is './'",metavar="FOLDER")
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

def remdash(string):
    return unicode(string.replace(u'\u2013',"-")).encode('utf-8')
def trytext(el):
    textList = []
    text = el.text
    childText = None
    grandchildText = None
    children = el.getchildren()
    childLen = len(children)
    if childLen>0:
        child = children[0]
        childText = child.text
        grandchildren = child.getchildren()
        grandchildLen = len(grandchildren)
        if grandchildLen>0:
            grandchild = grandchildren[0]
            grandchildText = grandchild.text
    result = ""
    textList.append(text)
    textList.append(childText)
    textList.append(grandchildText)
    finalList = filter(None,textList)
    result = " ".join(finalList)
    output = remdash(result)
    if output=="":
        return None
    else:
        return output
    
def pdftoxml(pdfdata, options):
    """converts pdf file to xml file"""
    # lots of hacky Windows fixes c.f. original
    basename = os.path.basename(pdfdata)
    inputname, inputextension = os.path.splitext(basename)
    absDir = os.path.dirname(pdfdata)+"/"
    cmd = 'pdftohtml -xml -nodrm -zoom 1.5 -enc UTF-8 -noframes "'
    if options:
        cmd += options
    cmd += pdfdata
    cmd +=  '" "'
    cmd += absDir
    cmd += inputname+'.xml"'
    cmd = cmd + " > NUL 2>&1" # can't turn off output, so throw away even stderr yeuch
    os.system(cmd)
    with open(absDir+inputname+'.xml', 'r') as f:
        return f.read()

def main():
    #Before writing, try pdftohtml NAME.pdf -xml NAME.xml
    #Requires Poppler for windows in your path
    #http://blog.alivate.com.au/poppler-windows/
    basename = os.path.basename(options.input)
    inputname, inputextension = os.path.splitext(basename)
    sys.stdout.write("Reading "+basename+"... This may take a while....")
    xmldata = pdftoxml(options.input,False)
    root = lxml.etree.fromstring(xmldata)
    pages = list(root)
    output = []
    pageLen = len(pages)
    #Cascade these down...
    quarter = ""
    vote = ""
    ministry = ""
    for i in range(0,pageLen):
        if i%100==0:
            sys.stdout.write("\n")
            sys.stdout.write("On page "+str(i)+" out of "+str(pageLen)+"....")
            sys.stdout.flush()
        spin()
        isTable = False
        page = pages[i]
        elLen = len(page)
        for j in range(0,elLen):
            el = page[j]
            if el.tag == "text":
                left = int(el.attrib['left'])
                top = int(el.attrib['top'])
                right = int(el.attrib['left'])+int(el.attrib['width'])
                font = int(el.attrib['font'])
                pageNum = i
                if not isTable:
                    if trytext(el)=="Table V1.1: Overview of Vote Expenditures  (UShs Billion)":
                        prev2 = el.getprevious().getprevious()
                        prev3 = el.getprevious().getprevious().getprevious()
                        prev4 = el.getprevious().getprevious().getprevious().getprevious()
                        quarter = "" if prev2 is None else trytext(prev2)
                        vote = "" if prev3 is None else trytext(prev3)
                        ministry = "" if prev4 is None else trytext(prev4)
                        isTable = True
                else:
                    if trytext(el)=="Table V1.1: Overview of Vote Expenditures  (UShs Billion)":
                        prev2 = el.getprevious().getprevious()
                        prev3 = el.getprevious().getprevious().getprevious()
                        prev4 = el.getprevious().getprevious().getprevious().getprevious()
                        quarter = "" if prev2 is None else trytext(prev2)
                        vote = "" if prev3 is None else trytext(prev3)
                        ministry = "" if prev4 is None else trytext(prev4)
                    if abs(right-288)<20:
                        #Find row by going backwards and forwards...
                        row = []
                        elTop = int(el.attrib['top'])
                        obj = {}
                        obj['text'] = trytext(el)
                        obj['top'] = int(el.attrib['top'])
                        obj['left'] = int(el.attrib['left'])
                        obj['right'] = int(el.attrib['left'])+int(el.attrib['width'])
                        obj['font'] = int(el.attrib['font'])
                        row.append(obj)
                        #Backwards
                        prev = el.getprevious()
                        if prev is not None:
                            prevTop = int(prev.attrib['top'])
                        else:
                            prevTop = 0
                        while prev is not None and "top" in prev.attrib:
                            obj = {}
                            obj['text'] = trytext(prev)
                            obj['top'] = int(prev.attrib['top'])
                            obj['left'] = int(prev.attrib['left'])
                            obj['right'] = int(prev.attrib['left'])+int(prev.attrib['width'])
                            obj['font'] = int(prev.attrib['font'])
                            if abs(elTop-prevTop)<4:
                                row.append(obj)
                            prev = prev.getprevious()
                            if prev is not None and "top" in prev.attrib:
                                prevTop = int(prev.attrib['top'])
                            else:
                                prevTop = 0
                        #Forwards
                        nxt = el.getnext()
                        if nxt is not None and "top" in nxt.attrib:
                            nxtTop = int(nxt.attrib['top'])
                        else:
                            nxtTop = 0
                        while nxt is not None:
                            obj = {}
                            obj['text'] = trytext(nxt)
                            obj['top'] = int(nxt.attrib['top'])
                            obj['left'] = int(nxt.attrib['left'])
                            obj['right'] = int(nxt.attrib['left'])+int(nxt.attrib['width'])
                            obj['font'] = int(nxt.attrib['font'])
                            if  abs(elTop-nxtTop)<4:
                                row.append(obj)
                            nxt = nxt.getnext()
                            if nxt is not None and "top" in nxt.attrib:
                                nxtTop = int(nxt.attrib['top'])
                            else:
                                nxtTop = 0
                        rowvals = operator.itemgetter('left')
                        row.sort(key=rowvals)
                        if len(row)>7 and row[0]['text']!="(i) Excluding Arrears, Taxes":
                            #Find missing pieces of data, replace them with blanks 
                            rowArr = []
                            rowArr.append(row[0]['text'])
                            rights = [288,367,446,525,601,680,758]
                            altRights = [288,367,446,525,601,680,758]
                            for r in range(len(rights)):
                                right = rights[r]
                                altRight = altRights[r]
                                textMatch = False
                                for element in row:
                                    if abs(element['right']-right)<20 or abs(element['right']-altRight)<20:
                                        textMatch = element['text']
                                if textMatch:
                                    rowArr.append(textMatch)
                                else:
                                    rowArr.append("")
                            metaObj = {}
                            metaObj["Source"] = rowArr[0]
                            metaObj["AB"] = rowArr[1]
                            metaObj["CL"] = rowArr[2]
                            metaObj["RE"] = rowArr[3]
                            metaObj["SE"] = rowArr[4]
                            metaObj["BR"] = rowArr[5]
                            metaObj["BS"] = rowArr[6]
                            metaObj["RS"] = rowArr[7]
                            obj = {}
                            obj['Quarter']=quarter
                            obj['Ministry']=ministry
                            obj['Vote']=vote
                            obj['Expenditure']=metaObj["Source"]
                            obj['Approved budget'] = metaObj["AB"]
                            obj['Cash limits by End'] = metaObj["CL"]
                            obj['Released by End'] = metaObj["RE"]
                            obj['Spent by End Jun'] = metaObj["SE"]
                            obj['% Budget Released'] = metaObj["BR"]
                            obj['% Budget Spent'] = metaObj["BS"]
                            obj['% Releases Spent'] = metaObj["RS"]
                            output.append(obj)
    if options.debug:
        pdb.set_trace()
    keys = ["Vote","Quarter","Ministry","Expenditure","Approved budget","Cash limits by End","Released by End","Spent by End Jun","% Budget Released","% Budget Spent","% Releases Spent"]
    with open(options.output+"/allan.csv", 'wb') as output_file:
        dict_writer = csv.DictWriter(output_file, keys)
        dict_writer.writeheader()
        dict_writer.writerows(output)
    sys.stdout.write("\n")
    print("Done.")

main()