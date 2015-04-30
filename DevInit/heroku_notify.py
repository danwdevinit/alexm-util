#!/usr/bin/env python

#Import
import smtplib
import sys
import ast
from email.mime.text import MIMEText
from optparse import OptionParser
import time
import pdb

##To run:
#heroku logs --tail | python -u /git/alexm-util/DevInit/heroku_notify.py -p password

##Parse Options
parser = OptionParser()
parser.add_option("-u", "--user", dest="user", default="devinitautomailer",
                help="Sender Gmail username", metavar="TEXT")
parser.add_option("-p", "--password", dest="password",
                help="Sender Gmail password", metavar="TEXT")
parser.add_option("-r", "--recip", dest="recip", default='["Alex.Miller@devinit.org","Leighton.James@devinit.org"]',
                help="Recipient email address array.", metavar="TEXT")
parser.add_option("-w", "--wait", dest="wait", default="60",
                help="Wait time in seconds", metavar="INT")
parser.add_option("-e", "--errors", dest="errors", default='["error code=H","Error R","Error L"]',
                help="Error code array.", metavar="TEXT")
(options, args) = parser.parse_args()

##Collect errors
#Set lastTime to the present time
lastTime = time.time()
#Parse wait option
wait = int(options.wait)
#Initialize errs as empty list
errs = []
#Parse errCode option
errCodes = ast.literal_eval(options.errors)
#Infinite loop
while 1:
    #Read logs
    log = sys.stdin.readline()
    if log:
        #Check if errorcode in logs
        for code in errCodes:
            if code in log:
                #Append it to err list if so
                errs.append(log)
    #If we've waited 60 seconds since the last email
    #And we have at least one error, send the emails
    if time.time()-lastTime>=wait and len(errs)>=1:
        #Message is concat of err list
        msg = MIMEText("\n".join(errs))
        #Parse recipient option
        recip = ast.literal_eval(options.recip)
        #Print some output...
        print(str(len(errs))+" errors found...")
        print("Sending emails to "+" and ".join(recip))
        print("")
        #Reset lastTime and errs
        lastTime = time.time()
        errs = []
        #Connect to gmail and login
        s = smtplib.SMTP("smtp.gmail.com:587")
        s.starttls()
        s.login(options.user,options.password)
        #Send a message to each recipient
        for recipient in recip:
            me = options.user+"@gmail.com"
            you = recipient
            msg['Subject'] = 'Heroku Error Notification'
            msg['From'] = me
            msg['To'] = you
            s.sendmail(me, [you], msg.as_string())
        s.quit()