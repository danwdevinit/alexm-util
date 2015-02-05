#!/usr/bin/env node
var request = require('request'),
moment = require('moment'),
cheerio = require('cheerio'),
s = encodeURI(process.argv[2]),
now = moment(),
d = now.month()<10?"0"+now.month():now.month(),
e = now.date(),
f = now.year(),
then = moment().subtract(8,'weeks'),
a = then.month()<10?"0"+then.month():then.month(),
b = then.date(),
c = then.year(),
url = "http://real-chart.finance.yahoo.com/table.csv?s="+s+"&a="+a+"&b="+b+"&c="+c+"&d="+d+"&e="+e+"&f="+f+"&g=d&ignore=.csv",
data = [];
request(url,parseCSV);

function ema(prices,days){
    var num = 0,
    den = 0,
    alpha = (2/(days+1));
    for(var i = 0; i < days; i++){
        var price = prices[i];
        num += Math.pow((1-alpha),i)*price;
        den += Math.pow((1-alpha),i);
    };
    return num/den;
};

function macd(data){
    var macd = ema(data,12) - ema(data,26),
    macds = [];
    for(var i = 2; i < 9; i++){
        var tempMacd = ema(data.slice(i),12) - ema(data.slice(i),26);
        macds.push(tempMacd);
    };
    var trigger = ema(macds,macds.length);
    return [macd,trigger]
};

function parseCSV(e,r,b){
    if(b.substr(0,1)=="<"){
        throw "ERR: No stock found!"
    };
    var raw = b.split("\n"),
    sum = 0;
    raw.pop();
    raw.shift();
    var latest = moment(raw[0].split(",")[0],"YYYY-MM-DD");
    for(var row in raw){
        var close = parseFloat(raw[row].split(",")[4]);
        data.push(close);
    };
    data.unshift(parseFloat(process.argv[3]))
    var macdArr = macd(data),
    thisMacd = macdArr[0],
    thisTrigger = macdArr[1];
    console.log("Hypothetically, if today's close was equal to "+process.argv[3]+"...")
    console.log("The MACD is equal to "+thisMacd.toFixed(2)+" and the trigger line is equal to "+thisTrigger.toFixed(2)+".")
    if(thisMacd>thisTrigger){
        console.log("Since the MACD is greater than the trigger line, this could be indicative of a bullish crossover.")
    };
    if(thisMacd<thisTrigger){
        console.log("Since the MACD is less than the trigger line, this could be indicative of a bearish crossover.")
    };
    if(thisMacd==thisTrigger){
        console.log("Oddly enough, they're exactly equal.")
    };
    console.log("Copy and paste the following line into your log-book:")
    console.log(now.format("MM/DD/YY")+","+parseFloat(process.argv[3])+","+thisMacd+","+thisTrigger)
};