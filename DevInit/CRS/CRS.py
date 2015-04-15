#!/usr/bin/env python

#Import system
import glob
import sys, os
import zipfile,os.path
import io
from optparse import OptionParser
import codecs
import subprocess
import csv
import pdb


#Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/Raw/",
                help="Input folder.", metavar="FOLDER")
parser.add_option("-o", "--output", dest="output", default="S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/",
                help="Output folder.", metavar="FILE")
parser.add_option("-d", "--download", dest="download", default=False,
                help="Re-download?", metavar="BOOLEAN")
(options, args) = parser.parse_args()

reload(sys)
sys.setdefaultencoding('utf-8')

def uni(input):
    try:
        output = unicode(input).encode('latin1', 'replace')
    except:
        output = unicode(input).encode('utf-8', 'replace')
    return output

class Recoder(object):
    def __init__(self, stream, decoder, encoder, eol='\r\n'):
        self._stream = stream
        self._decoder = decoder if isinstance(decoder, codecs.IncrementalDecoder) else codecs.getincrementaldecoder(decoder)()
        self._encoder = encoder if isinstance(encoder, codecs.IncrementalEncoder) else codecs.getincrementalencoder(encoder)()
        self._buf = ''
        self._eol = eol
        self._reachedEof = False

    def read(self, size=None):
        r = self._stream.read(size)
        raw = self._decoder.decode(r, size is None)
        return self._encoder.encode(raw)

    def __iter__(self):
        return self

    def __next__(self):
        if self._reachedEof:
            raise StopIteration()
        while True:
            line,eol,rest = self._buf.partition(self._eol)
            if eol == self._eol:
                self._buf = rest
                return self._encoder.encode(line + eol)
            raw = self._stream.read(1024)
            if raw == '':
                self._decoder.decode(b'', True)
                self._reachedEof = True
                return self._encoder.encode(self._buf)
            self._buf += self._decoder.decode(raw)
    next = __next__

    def close(self):
        return self._stream.close()

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

#Re-download?
if options.download:
    print "Re-downloading zip files to "+options.input
    print subprocess.check_output(["node","downloadCRS.js",options.input])

#Find .zip in folder
paths = glob.glob(options.input+"/*.zip")

#Iterate through paths and unzip
for inPath in paths:
    filename = os.path.basename(inPath)
    print "Extracting "+filename
    unzip(inPath,options.input)
    #os.remove(inPath)

#Find .txt in folder
txtpaths = glob.glob(options.input+"/*.txt")

#Iterate through paths and re-encode and replace nul
for inPath in txtpaths:
    filename = os.path.basename(inPath)
    name, extension = os.path.splitext(filename)
    print "Reading "+filename
    with open(inPath,'rb') as fr:
        sr = Recoder(fr, 'utf-16', 'utf-8')
        outPath = options.output+name+".csv"
        with open(outPath, 'wb') as fw:
            writer = csv.writer(fw,delimiter=",",quotechar="\"")
            for line in sr:
                spin()
                row = line.replace("\x00","").split("|")[0:-1]
                writer.writerow(map(uni,row))
    os.remove(inPath)