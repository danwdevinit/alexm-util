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
import StringIO

parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="C:/git/alexm-util/DevInit/R/GHA/presentation/UNHCR global trends report 2014.pdf",
                help="Input pdf name", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="C:/git/alexm-util/DevInit/R/GHA/presentation/tmp/global_trends.csv",
                        help="Output path",metavar="FOLDER")
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
    path = options.input
    basename = os.path.basename(path)
    inputname, inputextension = os.path.splitext(basename)
    print("Reading "+basename+"...")
    xmldata = pdftoxml(path,False)
    parser = lxml.etree.XMLParser(encoding='utf-8', recover=True)
    root = lxml.etree.XML(xmldata,parser)
    pages = list(root)
    output = []
    pageLen = len(pages)
    for i in range(0,pageLen):
        page = pages[i]
        isTable = int(page.attrib['number']) >= 49 and int(page.attrib['number']) <= 53
        if isTable:
            elLen = len(page)
            for j in range(0,elLen):
                el = page[j]
                if el.tag == "text":
                    left = int(el.attrib['left'])
                    right = int(el.attrib['left'])+int(el.attrib['width'])
                    top = int(el.attrib['top'])
                    font = int(el.attrib['font'])
                    if abs(left-52)<5:
                        #Find row by going backwards and forwards...
                        row = []
                        elTop = int(el.attrib['top'])
                        obj = {}
                        obj['text'] = trytext(el)
                        obj['top'] = int(el.attrib['top'])
                        obj['left'] = int(el.attrib['left'])
                        obj['right'] = int(el.attrib['left'])+int(el.attrib['width'])
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
                            if abs(elTop-prevTop)<3:
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
                            if  abs(elTop-nxtTop)<3:
                                row.append(obj)
                            nxt = nxt.getnext()
                            if nxt is not None and "top" in nxt.attrib:
                                nxtTop = int(nxt.attrib['top'])
                            else:
                                nxtTop = 0
                        rowvals = operator.itemgetter('left')
                        row.sort(key=rowvals)
                        if len(row)==12:
                            rowArr = [element['text'] for element in row]
                            output.append(rowArr)
    if options.debug:
        pdb.set_trace()
    keys = ["Origin","Refugees","People in refugee-like situations","Total refugees and people in refugee-like situations","of whom: UNHCR-assisted","Asylum-seekers (pending cases)","Returned refugees","IDPs protected/assisted by UNHCR, incl. people in IDP-like situations","Returned IDPs","Persons under UNHCR's statelessness mandate","Others of concern to UNHCR","Total population of concern"]
    with open(options.output, 'wb') as output_file:
        dict_writer = csv.writer(output_file)
        dict_writer.writerow(keys)
        dict_writer.writerows(output)
    sys.stdout.write("\n")
    print("Done.")

main()