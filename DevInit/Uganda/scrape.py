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
    for i in range(0,pageLen):
        page = pages[i]
        for el in page:
            if el.tag == "text":
                obj = {}
                left = int(el.attrib['left'])
                right = int(el.attrib['left'])+int(el.attrib['width'])
                top = int(el.attrib['top'])
                font = int(el.attrib['font'])
                if left == 37:
                    obj['val']=trytext(el)
                    obj['type']="Economic Function"
                    obj['top'] = top
                    obj['page'] = i
                    if obj['val']!=None:
                        output.append(obj)
                elif 362<=right<=363:
                    obj['val']=trytext(el)
                    obj['type']="Wage"
                    obj['top'] = top
                    obj['page'] = i
                    if obj['val']!=None:
                        output.append(obj)
                elif 432<=right<=434:
                    obj['val']=trytext(el)
                    obj['type']="Non-wage"
                    obj['top'] = top
                    obj['page'] = i
                    if obj['val']!=None:
                        output.append(obj)
                elif 557<=right<=558:
                    obj['val']=trytext(el)
                    obj['type']="Total"
                    obj['top'] = top-4
                    obj['page'] = i
                    if obj['val']!=None:
                        output.append(obj)
                elif 629<=right<=630:
                    obj['val']=trytext(el)
                    obj['type']="Wage Est"
                    obj['top'] = top
                    obj['page'] = i
                    if obj['val']!=None:
                        output.append(obj)
                elif 696<=right<=697:
                    obj['val']=trytext(el)
                    obj['type']="Non-wage Est"
                    obj['top'] = top
                    obj['page'] = i
                    if obj['val']!=None:
                        output.append(obj)
                elif 831<=right<=832:
                    obj['val']=trytext(el)
                    obj['type']="Total Est"
                    obj['top'] = top-4
                    obj['page'] = i
                    if obj['val']!=None:
                        output.append(obj)
                if font==0:
                    obj['val']=trytext(el)
                    obj['type']="Ministry"
                    obj['top'] = top
                    obj['page'] = i
                    if obj['val']!=None:
                        output.append(obj)
                elif font==1:
                    obj['val']=trytext(el)
                    obj['type']="Vote"
                    obj['top'] = top
                    obj['page'] = i
                    if obj['val']!=None:
                        output.append(obj)
                elif font==3:
                    obj['val']=trytext(el)
                    obj['type']="Department"
                    obj['top'] = top
                    obj['page'] = i
                    if obj['val']!=None:
                        output.append(obj)
                elif font==4:            
                    obj['val']=trytext(el)
                    obj['type']="Budget Type"
                    obj['top'] = top
                    obj['page'] = i
                    if obj['val']!=None:
                        output.append(obj)
                elif font==5:
                    obj['val']=trytext(el)
                    obj['type']="Programme"
                    obj['top'] = top
                    obj['page'] = i
                    if obj['val']!=None:
                        output.append(obj)
                elif font==9:
                    obj['val']=trytext(el)
                    obj['type']="Output"
                    obj['top'] = top
                    obj['page'] = i
                    if obj['val']!=None:
                        output.append(obj)
                elif font==14:
                    try:
                        obj['val']=str.split(el.getprevious().text,"- ")[1]
                    except:
                        try:
                            obj['val']=el.getprevious().text
                        except:
                            obj['val']
                    obj['type']="Sector"
                    obj['top'] = top
                    obj['page'] = i
                    if obj['val']!=None:
                        output.append(obj)
    #Sort/Find Unique
    getvals = operator.itemgetter('page','type','top','val')
    output.sort(key=getvals)
    unique = []
    for k, g in itertools.groupby(output,getvals):
        unique.append(list(g)[0])
    output = unique[:]
    getvals = operator.itemgetter('page','top')
    output.sort(key=getvals)
    outputFixed = output[:1]
    prev = output[1]['top']
    for i in range(1,len(output)):
        obj = output[i]
        if obj['top']-prev==1:
            obj['top']-=1
            outputFixed.append(obj)
        elif obj['top']-prev==-1:
            obj['top']+=1
            outputFixed.append(obj)
        else:
            outputFixed.append(obj)
        prev = obj['top']
    results = []
    groups = []
    for k, g in itertools.groupby(outputFixed,getvals):
        groups.append(list(g))
    #Add in votes/ministry/sector
    #Add in vote function and programme
    for group in groups:
        if len(group)==1:
            obj = {}
            for el in group:
                obj[el['type']]=el['val']
            if 'Economic Function' not in obj:    
                results.append(obj)
        if len(group)==7:
            obj = {}
            for el in group:
                obj[el['type']]=el['val']
            results.append(obj)
    data = pd.DataFrame(results)
    data.to_csv(options.output+inputname+".csv",encoding="utf-8")
    print("Done.")

main()