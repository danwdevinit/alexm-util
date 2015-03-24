#!/usr/bin/nodejs
var request = require('request'),
cheerio = require('cheerio'),
fs = require('fs'),
baseUrl = "http://www.mofep.gov.gh",
pageUrl = "http://www.mofep.gov.gh/?q=divisions/fdu/composite-budget-of-MDAs-2013",
outputPath = "/s/Projects/Programme resources/Data/Data sets/Domestic Government Expenditure/Government budgets/Ghana/2013/District budgets/"
request(pageUrl,parseCallback);

function parseCallback(e,r,b) {
    if(e){throw e};
    var $ = cheerio.load(b,{normalizeWhitespace:true}),
    links = $('a'),
    linkLen = links.length;
    for(var i = 0; i < linkLen; i++){
        var link = links[i],
        keys = Object.keys(link.attribs),
        href = keys.indexOf("href")>-1?link.attribs.href:"";
        if(href.substr(0,30)=="/?q=budget-statement/2013-comp"){
            var regionUrl = baseUrl+href;
            request(regionUrl,parseRegion);
        };
    };
};
function parseRegion(e,r,b){
    if(e){throw e};
    var uri = r.request.uri.href;
    var $ = cheerio.load(b,{normalizeWhitespace:true}),
    title = $('h3#page-title')[0];
    console.log(uri)
    console.log(title.children[0].data);
};