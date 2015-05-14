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

parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="./Central govt/Approved Estimates 2012-13.pdf",
                help="Input pdf name", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="./tmp/",
                        help="Output path. Default is './tmp/'",metavar="FOLDER")
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
    absDir = os.path.dirname(pdfdata)+"/"
    cmd = 'pdftohtml -xml -nodrm -zoom 1.5 -enc UTF-8 -noframes "'
    if options:
        cmd += options
    cmd += pdfdata
    cmd +=  '" "'
    cmd += absDir
    cmd +='output.xml"'
    cmd = cmd + " > NUL 2>&1" # can't turn off output, so throw away even stderr yeuch
    os.system(cmd)
    with open(absDir+'output.xml', 'r') as f:
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
        isTableV3 = False
        page = pages[i]
        elLen = len(page)
        for j in range(0,elLen):
            el = page[j]
            if el.tag == "text":
                left = int(el.attrib['left'])
                right = int(el.attrib['left'])+int(el.attrib['width'])
                top = int(el.attrib['top'])
                font = int(el.attrib['font'])
                if not isTableV3:
                    if trytext(el)=="Table V3: Detailed Estimates by Vote Function, Cost Centre, Output and Item ":
                        isTableV3 = True
                        ministry = trytext(el.getprevious().getprevious())
                        vote = trytext(el.getprevious())
                else:
                    if font==46:
                        if trytext(el)!=None:
                            department = trytext(el)
                    elif font==34:            
                        if trytext(el)!=None:
                            budgetType = trytext(el)
                    elif font==47:
                        if trytext(el)!=None:
                            programme = trytext(el)
                    elif font==41:
                        if trytext(el)!=None:
                            econOutput = trytext(el)
                    elif font==36 and "Sector" in trytext(el):
                        sector+=1
                        try:
                            sectors.append(str.split(el.text,"- ")[1])
                        except:
                            sectors.append("Unknown Sector")
                    elif font==30:
                        if j<elLen-8:
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
                            el8 = page[j+7]
                            font8 = int(el8.attrib['font'])
                            el9 = page[j+8]
                            font9 = int(el9.attrib['font'])
                            #Pattern is 30 30 27 30 30 27 36 30 30 for:
                            #wage non-wage total wageEst non-wageEst totalEst econFunc NTR NTREst
                            if font2==30 and font3==27 and font4==30 and font5==30 and font6==27 and font7==36 and font8==30 and font9==30:
                                #Wage 2011/12 Approved Budget
                                obj = {}
                                obj['year']="2011/12 Approved Budget"
                                obj['Government']="Central Government"
                                obj['sectorId']=sector
                                obj['Vote']=vote
                                obj['Ministry']=ministry
                                obj['Budget Type']=budgetType
                                obj['Department']=department
                                obj['Programme']=programme
                                obj['Budget']=trytext(el)
                                obj['Budget Function']="Wage" if obj['Budget Type']=="Recurrent Budget Estimates" else "GOU"
                                obj['Economic Function']=trytext(el7)
                                obj['Output']=econOutput
                                obj['ofWhich']=""
                                output.append(obj)
                                #Non-Wage 2011/12 Approved Budget
                                obj = {}
                                obj['year']="2011/12 Approved Budget"
                                obj['Government']="Central Government"
                                obj['sectorId']=sector
                                obj['Vote']=vote
                                obj['Ministry']=ministry
                                obj['Budget Type']=budgetType
                                obj['Department']=department
                                obj['Programme']=programme
                                obj['Budget']=trytext(el2)
                                obj['Budget Function']="Non Wage" if obj['Budget Type']=="Recurrent Budget Estimates" else "External Fin."
                                obj['Economic Function']=trytext(el7)
                                obj['Output']=econOutput
                                obj['ofWhich']=""
                                output.append(obj)
                                #Wage 2012/13 Approved Estimates
                                obj = {}
                                obj['year']="2012/13 Approved Estimates"
                                obj['Government']="Central Government"
                                obj['sectorId']=sector
                                obj['Vote']=vote
                                obj['Ministry']=ministry
                                obj['Budget Type']=budgetType
                                obj['Department']=department
                                obj['Programme']=programme
                                obj['Budget']=trytext(el4)
                                obj['Budget Function']="Wage" if obj['Budget Type']=="Recurrent Budget Estimates" else "GOU"
                                obj['Economic Function']=trytext(el7)
                                obj['Output']=econOutput
                                obj['ofWhich']=""
                                output.append(obj)
                                #Non-Wage 2012/13 Approved Estimates
                                obj = {}
                                obj['year']="2012/13 Approved Estimates"
                                obj['Government']="Central Government"
                                obj['sectorId']=sector
                                obj['Vote']=vote
                                obj['Ministry']=ministry
                                obj['Budget Type']=budgetType
                                obj['Department']=department
                                obj['Programme']=programme
                                obj['Budget']=trytext(el5)
                                obj['Budget Function']="Non Wage" if obj['Budget Type']=="Recurrent Budget Estimates" else "External Fin."
                                obj['Economic Function']=trytext(el7)
                                obj['Output']=econOutput
                                obj['ofWhich']=""
                                output.append(obj)
                                #NTR 2011/12 Approved Budget
                                obj = {}
                                obj['year']="2011/12 Approved Budget"
                                obj['Government']="Central Government"
                                obj['sectorId']=sector
                                obj['Vote']=vote
                                obj['Ministry']=ministry
                                obj['Budget Type']=budgetType
                                obj['Department']=department
                                obj['Programme']=programme
                                obj['Budget']=trytext(el8)
                                obj['Budget Function']="NTR"
                                obj['Economic Function']=trytext(el7)
                                obj['Output']=econOutput
                                obj['ofWhich']=""
                                output.append(obj)
                                #NTR 2012/13 Approved Estimates
                                obj = {}
                                obj['year']="2012/13 Approved Estimates"
                                obj['Government']="Central Government"
                                obj['sectorId']=sector
                                obj['Vote']=vote
                                obj['Ministry']=ministry
                                obj['Budget Type']=budgetType
                                obj['Department']=department
                                obj['Programme']=programme
                                obj['Budget']=trytext(el9)
                                obj['Budget Function']="NTR"
                                obj['Economic Function']=trytext(el7)
                                obj['Output']=econOutput
                                obj['ofWhich']=""
                    #of which columns
                    elif trytext(el)[:3]=="o/w":
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
                        while prev is not None and abs(elTop-prevTop)<4:
                            obj = {}
                            obj['text'] = trytext(prev)
                            obj['top'] = int(prev.attrib['top'])
                            obj['left'] = int(prev.attrib['left'])
                            obj['right'] = int(prev.attrib['left'])+int(prev.attrib['width'])
                            obj['font'] = int(prev.attrib['font'])
                            row.append(obj)
                            prev = prev.getprevious()
                            if prev is not None:
                                prevTop = int(prev.attrib['top'])
                            else:
                                prevTop = 0
                        #Forwards
                        nxt = el.getnext()
                        if nxt is not None:
                            nxtTop = int(nxt.attrib['top'])
                        else:
                            nxtTop = 0
                        while nxt is not None and abs(elTop-nxtTop)<4:
                            obj = {}
                            obj['text'] = trytext(nxt)
                            obj['top'] = int(nxt.attrib['top'])
                            obj['left'] = int(nxt.attrib['left'])
                            obj['right'] = int(nxt.attrib['left'])+int(nxt.attrib['width'])
                            obj['font'] = int(nxt.attrib['font'])
                            row.append(obj)
                            nxt = nxt.getnext()
                            if nxt is not None:
                                nxtTop = int(nxt.attrib['top'])
                            else:
                                nxtTop = 0
                        rowvals = operator.itemgetter('left')
                        row.sort(key=rowvals)
                        #Find missing pieces of data, replace them with blanks
                        rowArr = []
                        rowArr.append(row[0]['text'])
                        rights = [353,421,489,556,624,692,760,828]
                        for right in rights:
                            textMatch = False
                            for element in row:
                                if abs(element['right']-right)<4:
                                    textMatch = element['text']
                            if textMatch:
                                rowArr.append(textMatch)
                            else:
                                rowArr.append("")
                        #Find of last font 36 for 'of which'
                            ofWhich = el.getprevious()
                            ofWhichFont = int(ofWhich.attrib['font'])
                            while ofWhichFont!=36:
                                ofWhich = ofWhich.getprevious()
                                ofWhichFont = int(ofWhich.attrib['font'])
                        metaObj = {}
                        metaObj["Economic Function"] = rowArr[0]
                        metaObj["Wage"] = rowArr[1]
                        metaObj["NonWage"] = rowArr[2]
                        metaObj["NTR"] = rowArr[3]
                        metaObj["Total"] = rowArr[4]
                        metaObj["WageEst"] = rowArr[5]
                        metaObj["NonWageEst"] = rowArr[6]
                        metaObj["NTREst"] = rowArr[7]
                        metaObj["TotalEst"] = rowArr[8]
                        #Wage 2011/12 Approved Budget
                        obj = {}
                        obj['year']="2011/12 Approved Budget"
                        obj['Government']="Central Government"
                        obj['sectorId']=sector
                        obj['Vote']=vote
                        obj['Ministry']=ministry
                        obj['Budget Type']=budgetType
                        obj['Department']=department
                        obj['Programme']=programme
                        obj['Budget']=metaObj["Wage"]
                        obj['Budget Function']="Wage" if obj['Budget Type']=="Recurrent Budget Estimates" else "GOU"
                        obj['Economic Function']=metaObj["Economic Function"]
                        obj['Output']=econOutput
                        obj['ofWhich']=trytext(ofWhich)
                        output.append(obj)
                        #Non-Wage 2011/12 Approved Budget
                        obj = {}
                        obj['year']="2011/12 Approved Budget"
                        obj['Government']="Central Government"
                        obj['sectorId']=sector
                        obj['Vote']=vote
                        obj['Ministry']=ministry
                        obj['Budget Type']=budgetType
                        obj['Department']=department
                        obj['Programme']=programme
                        obj['Budget']=metaObj["NonWage"]
                        obj['Budget Function']="Non Wage" if obj['Budget Type']=="Recurrent Budget Estimates" else "External Fin."
                        obj['Economic Function']=metaObj["Economic Function"]
                        obj['Output']=econOutput
                        obj['ofWhich']=trytext(ofWhich)
                        output.append(obj)
                        #Wage 2012/13 Approved Estimates
                        obj = {}
                        obj['year']="2012/13 Approved Estimates"
                        obj['Government']="Central Government"
                        obj['sectorId']=sector
                        obj['Vote']=vote
                        obj['Ministry']=ministry
                        obj['Budget Type']=budgetType
                        obj['Department']=department
                        obj['Programme']=programme
                        obj['Budget']=metaObj["WageEst"]
                        obj['Budget Function']="Wage" if obj['Budget Type']=="Recurrent Budget Estimates" else "GOU"
                        obj['Economic Function']=metaObj["Economic Function"]
                        obj['Output']=econOutput
                        obj['ofWhich']=trytext(ofWhich)
                        output.append(obj)
                        #Non-Wage 2012/13 Approved Estimates
                        obj = {}
                        obj['year']="2012/13 Approved Estimates"
                        obj['Government']="Central Government"
                        obj['sectorId']=sector
                        obj['Vote']=vote
                        obj['Ministry']=ministry
                        obj['Budget Type']=budgetType
                        obj['Department']=department
                        obj['Programme']=programme
                        obj['Budget']=metaObj["NonWageEst"]
                        obj['Budget Function']="Non Wage" if obj['Budget Type']=="Recurrent Budget Estimates" else "External Fin."
                        obj['Economic Function']=metaObj["Economic Function"]
                        obj['Output']=econOutput
                        obj['ofWhich']=trytext(ofWhich)
                        output.append(obj)
                        #NTR 2011/12 Approved Budget
                        obj = {}
                        obj['year']="2011/12 Approved Budget"
                        obj['Government']="Central Government"
                        obj['sectorId']=sector
                        obj['Vote']=vote
                        obj['Ministry']=ministry
                        obj['Budget Type']=budgetType
                        obj['Department']=department
                        obj['Programme']=programme
                        obj['Budget']=metaObj["NTR"]
                        obj['Budget Function']="NTR"
                        obj['Economic Function']=metaObj["Economic Function"]
                        obj['Output']=econOutput
                        obj['ofWhich']=trytext(ofWhich)
                        output.append(obj)
                        #NTR 2012/13 Approved Estimates
                        obj = {}
                        obj['year']="2012/13 Approved Estimates"
                        obj['Government']="Central Government"
                        obj['sectorId']=sector
                        obj['Vote']=vote
                        obj['Ministry']=ministry
                        obj['Budget Type']=budgetType
                        obj['Department']=department
                        obj['Programme']=programme
                        obj['Budget']=metaObj["NTREst"]
                        obj['Budget Function']="NTR"
                        obj['Economic Function']=metaObj["Economic Function"]
                        obj['Output']=econOutput
                        obj['ofWhich']=trytext(ofWhich)
    #Add sectors in by sectorId
    for obj in output:
        obj['MTEF Sector'] = sectors[obj['sectorId']]
        del obj['sectorId']
    if options.debug:
        pdb.set_trace()
    keys = output[0].keys()
    with open(options.output+inputname+".csv", 'wb') as output_file:
        dict_writer = csv.DictWriter(output_file, keys)
        dict_writer.writeheader()
        dict_writer.writerows(output)
    sys.stdout.write("\n")
    print("Done.")

main()