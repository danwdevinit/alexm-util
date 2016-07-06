from selenium import webdriver
import time
import json
import pdb
from selenium.webdriver.remote.command import Command
from optparse import OptionParser
import os
from urllib.request import urlretrieve
from codecs import encode
import string

class Error(Exception):
    """Base class for exceptions in this module."""
    pass

class InputError(Error):
    """Exception raised for errors in the input.

    Attributes:
        msg  -- explanation of the error
    """

    def __init__(self, msg):
        self.msg = msg
        
def format_filename(s):
    """Take a string and return a valid filename constructed from the string.
Uses a whitelist approach: any characters not present in valid_chars are
removed. Also spaces are replaced with underscores.
 
Note: this method may produce invalid filenames such as ``, `.` or `..`
When I use this method I prepend a date string like '2009_01_15_19_46_32_'
and append a file extension like '.txt', so I avoid the potential of using
an invalid filename.
 
"""
    valid_chars = "-_.() %s%s" % (string.ascii_letters, string.digits)
    filename = ''.join(c for c in s if c in valid_chars)
    filename = filename.replace(' ','_') # I don't like spaces in filenames.
    return filename

parser = OptionParser()
parser.add_option("-o", "--output", dest="output", default=os.getcwd(),
                        help="Output path. Default is wd",metavar="FOLDER")
(options, args) = parser.parse_args()

profile = webdriver.FirefoxProfile() #Change default download location for Firefox
profile.set_preference("browser.download.folderList", 2)
profile.set_preference('browser.download.manager.showWhenStarting', False)
profile.set_preference("browser.download.dir", options.output)
profile.set_preference("browser.helperApps.alwaysAsk.force", False)
profile.set_preference("browser.helperApps.neverAsk.saveToDisk", "application/pdf")
profile.set_preference("pdfjs.disabled", True)
profile.set_preference("plugin.scan.plid.all", False)
profile.set_preference("plugin.scan.Acrobat", "99.0")

browser = webdriver.Firefox(firefox_profile=profile) # Create a session of Firefox
browser.maximize_window()
browser.implicitly_wait(30) # Configure the WebDriver to wait up to 30 seconds for each page to load

browser.get("http://gadebate.un.org") # Load page

links = browser.find_elements_by_css_selector('h5 a')

def downloadPDFs(browser,links):
    for i in range(0,len(links)):
        links = browser.find_elements_by_css_selector('h5 a')
        speaker = links[i].text
        links[i].click()
        name = browser.find_element_by_css_selector('h2#gd_speakername').text
        try:
            url = browser.find_elements_by_link_text('English')[1].get_attribute('href')
            urlretrieve(url, options.output+"/"+format_filename(speaker+" ; "+name)+".pdf")
        except:
            summary = encode(browser.find_element_by_css_selector('div.even').text,"utf-8","replace")
            with open(options.output+"/"+format_filename(speaker+" ; "+name)+".txt","wb") as text_file:
                text_file.write(summary)
        browser.back()
        
downloadPDFs(browser,links)
    
dateTexts = ["29-Sep-2015 (37)","30-Sep-2015 (38)","1-Oct-2015 (37)","2-Oct-2015 (33)","3-Oct-2015 (14)"]

for i in range(0,len(dateTexts)):
    dateText = dateTexts[i]
    option = browser.find_element_by_xpath("//*[contains(text(), '%s')]" % dateText)
    option.click()
    browser.find_elements_by_xpath('//*[@type="submit"]')[0].click()
    links = browser.find_elements_by_css_selector('h5 a')
    downloadPDFs(browser,links)
    browser.back()