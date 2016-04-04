#!/usr/bin/env node
var request = require('request'),
moment = require('moment'),
cheerio = require('cheerio'),
s = encodeURI(process.argv[2]),
now = moment(),
d = now.month()<10?"0"+now.month():now.month(),
e = now.date(),
f = now.year(),
then = moment().subtract(parseInt(process.argv[3]),'days'),
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
    var stdDev = standardDev(data);
    console.log("The standard deviation for "+stock+
                " from "+latest.format("MMMM Do, YYYY")+
                " to "+then.format("MMMM Do, YYYY")+
                " is "+decodeURI('%C2%B1')+stdDev.toFixed(2)+
                ", or "+((stdDev/data[0])*100).toFixed(2)+"% of the latest close. Given the fact that "+
                data.length+
                " trading days occured during this time period, the average daily standard deviation is "+
                decodeURI('%C2%B1')+(stdDev/data.length).toFixed(2)+
                ", or "+(((stdDev/data.length)/data[0])*100).toFixed(2)+"% of the latest close."
                )
};