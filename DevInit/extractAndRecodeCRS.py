#!/usr/bin/env python

#Import system
import glob
import sys, os
import zipfile,os.path
import io
from optparse import OptionParser
import codecs


#Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="/s/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/Raw/",
                help="Input folder.", metavar="FOLDER")
parser.add_option("-o", "--output", dest="output", default="/s/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/Raw/",
                help="Output folder.", metavar="FILE")
(options, args) = parser.parse_args()

def uni(input):
    output = unicode(input).encode(sys.stdout.encoding, 'replace')
    return output

def unzip(source_filename, dest_dir):
    with zipfile.ZipFile(source_filename) as zf:
        for member in zf.infolist():
            # Path traversal defense copied from
            # http://hg.python.org/cpython/file/tip/Lib/http/server.py#l789
            words = member.filename.split('/')
            path = dest_dir
            for word in words[:-1]:
                drive, word = os.path.splitdrive(word)
                head, word = os.path.split(word)
                if word in (os.curdir, os.pardir, ''): continue
                path = os.path.join(path, word)
            zf.extract(member, path)

#Find .zip in folder
paths = glob.glob(options.input+"/CRS 2012.zip")

#Iterate through paths and unzip
#for inPath in paths:
#    filename = os.path.basename(inPath)
#    print "Extracting "+filename
#    unzip(inPath,options.input)
    #os.remove(inPath)

#Find .txt in folder
txtpaths = glob.glob(options.input+"/CRS 2012 data.txt")

#Iterate through paths and re-encode and replace nul
for inPath in txtpaths:
    filename = os.path.basename(inPath)
    name, extension = os.path.splitext(filename)
    print "Encoding "+filename
    outPath = options.output+name+"-enc.txt"
    fr = io.open(inPath, 'r' , encoding='utf-16-le')
    fw = codecs.open(outPath, "w", "utf-8")
    for l in fr.readlines():
        print >> fw, l.replace("\0", '')
    fr.close()
    fw.close()
    os.remove(inPath)