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
#python stream.py ... | python -u notify.py -p password

##Parse Options
parser = OptionParser()
parser.add_option("-u", "--user", dest="user", default="devinitautomailer",
                help="Sender Gmail username", metavar="TEXT")
parser.add_option("-p", "--password", dest="password",
                help="Sender Gmail password", metavar="TEXT")
parser.add_option("-r", "--recip", dest="recip", default='["Alex.Miller@devinit.org"]',
                help="Recipient email address array.", metavar="TEXT")
parser.add_option("-w", "--wait", dest="wait", default="60",
                help="Wait time in seconds", metavar="INT")
(options, args) = parser.parse_args()

def uni(input):
    try:
        output = unicode(input).encode('latin1', 'replace')
    except:
        output = unicode(input).encode('utf-8', 'replace')
    return output
##Collect
print("Email notification system initialized")
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
            title = uni(json.loads(tweet)['text'])
            tweets.append(title)
        except:
            continue
    #If we've waited 60 seconds since the last email
    #And we have at least one msg, send the emails
    if time.time()-lastTime>=wait and len(tweets)>=1:
        #Message is concat of tweets list
        #msg = MIMEText("\n".join(tweets))
        msg = MIMEText(tweets[0])
        #Parse recipient option
        recip = ast.literal_eval(options.recip)
        #Print some output...
        print(str(len(tweets))+" tweets found...")
        print("Sending emails to "+" and ".join(recip))
        print("")
        #Connect to gmail and login
        s = smtplib.SMTP("smtp.gmail.com:587")
        s.starttls()
        s.login(options.user,options.password)
        #Send a message to each recipient
        for recipient in recip:
            me = options.user+"@gmail.com"
            you = recipient
            msg['Subject'] = 'Daily twitter notification: '+str(len(tweets))+' tweet(s) logged'
            msg['From'] = me
            msg['To'] = you
            s.sendmail(me, [you], msg.as_string())
        s.quit()
        #Reset lastTime and tweets
        lastTime = time.time()
        tweets = []