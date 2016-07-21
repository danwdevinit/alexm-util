from selenium import webdriver
import time
from time import sleep
import json
import pdb
from selenium.webdriver.remote.command import Command
from optparse import OptionParser
import os

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

parser = OptionParser()
parser.add_option("-o", "--output", dest="output", default="D:\\Documents\\Data\\DHS gps data",
                        help="Output path. Default is wd",metavar="FOLDER")
(options, args) = parser.parse_args()

profile = webdriver.FirefoxProfile() #Change default download location for Firefox
profile.set_preference("browser.download.folderList", 2)
profile.set_preference('browser.download.manager.showWhenStarting', False)
profile.set_preference("browser.download.dir", options.output)
profile.set_preference("browser.helperApps.alwaysAsk.force", False)
profile.set_preference("browser.helperApps.neverAsk.saveToDisk", "application/x-zip-compressed,application/zip,application/octet-stream")

browser = webdriver.Firefox(firefox_profile=profile) # Create a session of Firefox
browser.maximize_window()
browser.implicitly_wait(30) # Configure the WebDriver to wait up to 30 seconds for each page to load

browser.get("http://spatialdata.dhsprogram.com/boundaries/#view=table")

links = browser.page_source.split("\n")

logged_in = False
for link in links:
    if link != "" and logged_in == False:
        browser.get(link)
        queries = []
        userInput = {}
        userInput["input_id"] = "UserName"
        userInput["input_str"] = options.user
        queries.append(userInput)
        passInput = {}
        passInput["input_id"] = "Password"
        passInput["input_str"] = options.password
        queries.append(passInput)
        input_text(browser, queries)
        browser.find_element_by_xpath('//*[@name="submit"]').click() #Click the submit button
        browser.find_element_by_xpath("//*[@name='proj_id']/option[{}]".format(options.proj+1)).click() #Click on the project option in the drop down
        browser.find_element_by_xpath('//*[@type="submit"]').click() #Click the submit button
        logged_in = True
        browser.get(link)
        sleep(1)
    elif link!="":
        browser.get(link)
        sleep(1)