#!/usr/bin/env python

#Import
import smtplib
import sys
import ast
from email.mime.text import MIMEText
from optparse import OptionParser
import time
import pdb
import json

##To run:
#python stream.py ... | python -u notify.py

##Parse Options
parser = OptionParser()
parser.add_option("-w", "--wait", dest="wait", default="1",
                help="Wait time in seconds", metavar="INT")
(options, args) = parser.parse_args()

def uni(input):
    try:
        output = unicode(input).encode('latin1', 'replace')
    except:
        output = unicode(input).encode('utf-8', 'replace')
    return output
##Collect
#Set lastTime to the present time
lastTime = time.time()
#Parse wait option
wait = int(options.wait)
#Initialize tweets as empty list
tweets = []
#Parse errCode option
#errCodes = ast.literal_eval(options.errors)
#print("Listening for keywords: "+" or ".join(errCodes))
#Infinite loop
while 1:
    #Read logs
    tweet = sys.stdin.readline().rstrip('\n')
    if tweet:
        try:
            user = uni(json.loads(tweet)['user']['screen_name'])
            title = uni(json.loads(tweet)['text'])
            tweets.append(user+": "+title)
        except:
            continue
    #If we've waited 60 seconds since the last email
    #And we have at least one msg, send the emails
    if time.time()-lastTime>=wait and len(tweets)>=1:
        #Message is concat of tweets list
        print("\n".join(tweets))
        lastTime = time.time()
        tweets = []