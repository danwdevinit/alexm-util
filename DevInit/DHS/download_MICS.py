from selenium import webdriver
import time
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
parser.add_option("-u", "--username", dest="user", default="alex.miller@devinit.org",
                help="DHS username", metavar="STRING")
parser.add_option("-p", "--password", dest="password", default=False,
                        help="DHS password",metavar="STRING")
parser.add_option("-r", "--project", dest="proj", default=1,
                        help="Project index",metavar="INTEGER")
parser.add_option("-o", "--output", dest="output", default=os.getcwd(),
                        help="Output path. Default is wd",metavar="FOLDER")
(options, args) = parser.parse_args()


def input_text(browser, inputs):
    # Fills a list of text boxes
    #
    # inputs: [{"input_id": "someId", "input_str": "some string"}, ... ]

    # This will cause Selenium to wait until the element is found.
    browser.find_element_by_xpath('//*[@id="{}"]'.format(inputs[0]["input_id"]))
    # browser.find_element_by_id(inputs[0]["input_id"]) 

    # Selenium is very slow at traversing the DOM. 
    # To quickly input text in many boxes, we inject a 
    # javacript function into the iframe. The collection
    # of textbox ids and strings is serialized 
    # as a Javascript object literal using the json module.
    inputs= json.dumps(inputs)
    js = "var inputs = {};".format(inputs)
    js += """
    console.log(inputs)
    for (var k = 0; k < inputs.length; k++) {
        var inputStr = inputs[k]["input_str"];
        var input = document.getElementById(inputs[k]["input_id"]);
        input.value = inputStr;
    }
    return true;"""
    browser.execute_script(js)
    
if not options.password:
    raise InputError("A valid password was not supplied.")

profile = webdriver.FirefoxProfile() #Change default download location for Firefox
profile.set_preference("browser.download.folderList", 2)
profile.set_preference('browser.download.manager.showWhenStarting', False)
profile.set_preference("browser.download.dir", options.output)
profile.set_preference("browser.helperApps.alwaysAsk.force", False)
profile.set_preference("browser.helperApps.neverAsk.saveToDisk", "application/x-zip-compressed,application/zip,application/octet-stream")

browser = webdriver.Firefox(firefox_profile=profile) # Create a session of Firefox
browser.maximize_window()
browser.implicitly_wait(30) # Configure the WebDriver to wait up to 30 seconds for each page to load

browser.get("http://mics.unicef.org/visitors/sign-in") # Load page
queries = []
userInput = {}
userInput["input_id"] = "visitor_email"
userInput["input_str"] = options.user
queries.append(userInput)
passInput = {}
passInput["input_id"] = "visitor_password"
passInput["input_str"] = options.password
queries.append(passInput)
input_text(browser, queries)

capcha_answer = input("Solve capcha... then press enter... ")

browser.find_element_by_xpath('//*[@name="commit"]').click() #Click the submit button

#Once logged in, we can query the API for data download URLs
browser.get("http://mics.unicef.org/api/survey")

pre = browser.find_element_by_tag_name('pre')
surveys = json.loads(pre.text)
for survey in surveys:
    url = survey['dataset']['url']
    if url is not "":
        browser.get(url)
