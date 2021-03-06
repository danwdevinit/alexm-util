#!/usr/bin/env python

import sys, os
import re
import scraperwiki
import urllib2, lxml.etree
from optparse import OptionParser
import pandas as pd
import pdb
import itertools
import operator
from operator import itemgetter

parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="./tmp/name.pdf",
                help="Input pdf name", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="./tmp/",
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
    for i in range(0,rowLen):
        row = rows[i]
    if len(errs)>0:
        sys.stdout.write("\n")
        sys.stdout.write(str(len(errs))+" odd rows omitted. Debug (-d true) for more info.")
    if options.debug:
        errData = pd.DataFrame(errs)
        errData.to_csv(options.output+"debug.csv",encoding="utf-8",index=False)
        pdb.set_trace()
    cols = []
    data = pd.DataFrame(results,columns=cols)
    data.to_csv(options.output+inputname+".csv",encoding="utf-8",index=False)
    sys.stdout.write("\n")
    print("Done.")

main()