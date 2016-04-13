#!/usr/bin/env node
var request = require('request'),
moment = require('moment'),
cheerio = require('cheerio'),
s = encodeURI(process.argv[2]),
now = moment(),
d = now.month()<10?"0"+now.month():now.month(),
e = now.date(),
f = now.year(),
diff = 52,
then = moment().subtract(diff,'weeks'),
a = then.month()<10?"0"+then.month():then.month(),
b = then.date(),
c = then.year(),
url = "http://real-chart.finance.yahoo.com/table.csv?s="+s+"&a="+a+"&b="+b+"&c="+c+"&d="+d+"&e="+e+"&f="+f+"&g=d&ignore=.csv",
data = [];
request(url,parseCSV);

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
};
 
function average(data){
    var sum = data.reduce(function(sum, value){
        return sum + value;
    }, 0);
    var avg = sum / data.length;
    return avg;
};

function parseCSV(e,r,b){
    if(b.substr(0,1)=="<"){
        throw "ERR: No stock found!"
    };
    var stock = r.request.uri.query.substr(2,r.request.uri.query.indexOf('&')-2),
    raw = b.split("\n");
    raw.pop();
    raw.shift();
    var latest = moment(raw[0].split(",")[0],"YYYY-MM-DD");
    for(var row in raw){
        var close = parseFloat(raw[row].split(",")[4]);
        data.push(close);
    };
    var logReturns = data.map(function(value,index,values){
        if (index+1<=data.length) {
            return(Math.log(value)-Math.log(values[index+1]));
        } else {
            return(null);
        };
    });
    logReturns.pop();
    var annualVol = standardDev(logReturns) * Math.sqrt(data.length) * 100;
    console.log("The annualized volatility for "+stock+
                " for the last "+data.length+" trading days "+
                " (from "+latest.format("MMMM Do, YYYY")+
                " to "+then.format("MMMM Do, YYYY")+
                ") is "+annualVol.toFixed(2)+"%"
                );
};