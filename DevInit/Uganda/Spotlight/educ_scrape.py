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
parser.add_option("-i", "--input", dest="input", default="S:/Projects/Programme resources/Data/Data sets/Domestic Government Expenditure/Government budgets/Uganda/2015-16/BFP/",
                help="Input pdf name", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="./",
                        help="Output path. Default is './'",metavar="FOLDER")
parser.add_option("-d", "--debug", dest="debug", default=False,
                        help="Debug",metavar="BOOLEAN")
(options, args) = parser.parse_args()

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
    paths = glob.glob(options.input+"/*.pdf")
    datasets = []
    for path in paths:
        basename = os.path.basename(path)
        inputname, inputextension = os.path.splitext(basename)
        print("Reading "+basename+"...")
        xmldata = pdftoxml(path,False)
        root = lxml.etree.fromstring(xmldata)
        pages = list(root)
        output = []
        pageLen = len(pages)
        #Cascade these down...
        district = ""
        vote = ""
        function = ""
        service = ""
        services = ["1. Higher LG Services","2. Lower Level Services","3. Capital Purchases"]
        serviceOutput = ""
        isTable = False
        isEducation = False
        dataMarkers = ["Wage Rec't:","Non Wage Rec't:","Domestic Dev't","Donor Dev't"]
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
                    if trytext(el)[:6]=="Vote: ":
                        vote = trytext(el)
                        district = trytext(el.getprevious())
                    if not isTable:
                        if trytext(el)=="Workplan Outputs":
                                isTable = True
                    else:
                        if not isEducation:
                            if trytext(el)=="6. Education":
                                isEducation=True
                        else:
                            if trytext(el)=="7a. Roads and Engineering":
                                isEducation=False
                            elif trytext(el)[:10]=="Function: ":
                                function = trytext(el)
                            elif trytext(el) in services:
                                service = trytext(el)
                            elif trytext(el)[:8]=="Output: ":
                                serviceOutput = trytext(el)
                            elif trytext(el) in dataMarkers:
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
                                wrongText = [""]
                                if len(row)==6:
                                    rowArr = [item['text'] for item in row]
                                    metaObj = {}
                                    metaObj["Budget Type"] = rowArr[0]
                                    metaObj["AB1415"] = rowArr[1]
                                    metaObj["PB1516"] = rowArr[5]
                                    #2014/15 Approved Budget
                                    obj = {}
                                    obj['Year']="2014/15 Approved Budget"
                                    obj['Government']="District Government"
                                    obj['District']=district
                                    obj['Vote']=vote
                                    obj['Budget Type'] = metaObj["Budget Type"]
                                    obj['Function'] = function
                                    obj['Service'] = service
                                    obj['Service Output'] = serviceOutput
                                    obj['Value']=metaObj["AB1415"]
                                    output.append(obj)
                                    #2015/16 Proposed Budget
                                    obj = {}
                                    obj['Year']="2015/16 Proposed Budget"
                                    obj['Government']="District Government"
                                    obj['District']=district
                                    obj['Vote']=vote
                                    obj['Budget Type'] = metaObj["Budget Type"]
                                    obj['Function'] = function
                                    obj['Service'] = service
                                    obj['Service Output'] = serviceOutput
                                    obj['Value']=metaObj["PB1516"]
                                    output.append(obj)
        outputVals = operator.itemgetter('Year','Budget Type','Function','Service','Service Output','Value')
        output.sort(key=outputVals)
        uniques = []
        for k, g in itertools.groupby(output,outputVals):
            group = list(g)
            uniques.append(group[0])
        datasets+=uniques
    if options.debug:
        pdb.set_trace()
    keys = datasets[0].keys()
    with open(options.output+"Educ-15-16.csv", 'wb') as output_file:
        dict_writer = csv.DictWriter(output_file, keys)
        dict_writer.writeheader()
        dict_writer.writerows(datasets)
    sys.stdout.write("\n")
    print("Done.")

main()