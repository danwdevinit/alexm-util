#!/usr/bin/env node
var request = require('request'),
moment = require('moment'),
cheerio = require('cheerio'),
wikiURL = 'http://en.wikipedia.org/wiki/List_of_S%26P_500_companies',
results = [];

request(wikiURL,parseWiki);
console.log("Processing...")

function parseWiki(e,r,b){
    var $ = cheerio.load(b),
    links = $('a');
    for(var i = 0; i < links.length; i++){
        if($(links[i]).attr('href')){
            if($(links[i]).attr('href').substr(0,32)=="https://www.nyse.com/quote/XNYS:"||$(links[i]).attr('href').substr(0,29)=="http://www.nasdaq.com/symbol/"){
                var s = $(links[i]).text(),
                diff = process.argv[2]?parseInt(process.argv[2]):8,
                now = moment(),
                d = now.month()<10?"0"+now.month():now.month(),
                ee = now.date(),
                f = now.year(),
                then = moment().subtract(diff,'weeks'),
                a = then.month()<10?"0"+then.month():then.month(),
                bb = then.date(),
                c = then.year(),
                url = "http://real-chart.finance.yahoo.com/table.csv?s="+s+"&a="+a+"&b="+bb+"&c="+c+"&d="+d+"&e="+ee+"&f="+f+"&g=d&ignore=.csv";
                request(url,parseCSV);
            };
        };
    };
};

function standardDev(values){
    var avg = average(values);
    var squareDiffs = values.map(function(value){
        var diff = value - avg;
        var sqrDiff = diff * diff;
        return sqrDiff;
    });
    var avgSquareDiff = average(squareDiffs);
    var stdDev = Math.sqrt(avgSquareDiff);
    return stdDev;
}
 
function average(data){
    var sum = data.reduce(function(sum, value){
        return sum + value;
    }, 0);
    var avg = sum / data.length;
    return avg;
}

function parseCSV(e,r,b){
    if(r){
        var stock = r.request.uri.query.substr(2,r.request.uri.query.indexOf('&')-2);
        if(b.substr(0,1)=="<"){
            console.log("WARN: No stock found for "+stock)
            console.log("")
        };
        var raw = b.split("\n"),
        data = [];
        raw.pop();
        raw.shift();
        for(var row in raw){
            var close = parseFloat(raw[row].split(",")[4]);
            data.push(close);
        };
        var dailyChanges = data.map(function(value,index,values){
            if (index+1<=data.length) {
                return(values[index+1]-value);
            } else {
                return(null);
            };
        });
        dailyChanges.pop();
        var stdDev = standardDev(dailyChanges),
        obj = {"stock":stock,"sd":stdDev,"vol":(stdDev/data[0])*100};
        results.push(obj);
    };
};


function sortAndPrint(){
    results.sort(function(a,b){
        return a.vol-b.vol;       
    });
    for(var i = 0; i < results.length; i++){
        console.log(results[i].stock+": "+decodeURI('%C2%B1')+" "+results[i].sd.toFixed(2)+"; "+decodeURI('%C2%B1')+" "+results[i].vol.toFixed(2)+"%");
    };
};
//If you want it to keep running until CTRL+c
//process.stdin.resume();
function exitHandler(options, err) {
    if (options.cleanup){
        sortAndPrint();
        process.exit();
    };
    if (err){console.log(err.stack)};
    if (options.exit){};
};
process.on('exit', exitHandler.bind(null,{cleanup:true}));
process.on('SIGINT', exitHandler.bind(null, {exit:true}));
process.on('uncaughtException', exitHandler.bind(null, {exit:true}));
