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
profile.set_preference("browser.helperApps.neverAsk.saveToDisk", "application/x-zip-compressed")

browser = webdriver.Firefox(firefox_profile=profile) # Create a session of Firefox
browser.maximize_window()
browser.implicitly_wait(30) # Configure the WebDriver to wait up to 30 seconds for each page to load

browser.get("http://www.dhsprogram.com/data/dataset_admin/login_main.cfm") # Load page
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

countries = browser.find_elements_by_xpath("//*[@name='Apr_Ctry_list_id']/option")

for i in range(1,len(countries)-1):
    countries = browser.find_elements_by_xpath("//*[@name='Apr_Ctry_list_id']/option")
    country = countries[i].text
    browser.find_element_by_xpath("//*[@name='Apr_Ctry_list_id']/option[{}]".format(i+1)).click() #Click on the country option in the drop down
    browser.find_element_by_xpath('//*[@type="submit"]').click() #Click the submit button
    surveyTds = browser.find_elements_by_xpath('//*[@id="link to download Survey datasets"]')
    for k in range(0,len(surveyTds)):
        surveyTds = browser.find_elements_by_xpath('//*[@id="link to download Survey datasets"]')
        surveyTd = surveyTds[k]
        surveyLinks = surveyTd.find_elements_by_tag_name('a')
        if len(surveyLinks)>0:
            surveyLinks[0].click()
            browser.find_element_by_xpath('//*[@value="stata"]').click() #select all the stata sets
            browser.find_element_by_xpath('//*[@type="submit"]').click() #Click the submit button
            downloadBtn = browser.find_elements_by_partial_link_text('Click here to download')
            if len(downloadBtn)>0:
                # downloadBtn[0].click()
                browser.get(downloadBtn[0].get_attribute('href'))
                browser.back()
            browser.back()