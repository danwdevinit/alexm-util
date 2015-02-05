#!/usr/bin/env node
var request = require('request'),
moment = require('moment'),
cheerio = require('cheerio');

for(var j = 0; j < parseInt(process.argv[3]); j++){
    var s = encodeURI(process.argv[2]),
    now = moment().subtract(j,'days'),
    d = now.month()<10?"0"+now.month():now.month(),
    ee = now.date(),
    f = now.year(),
    then = moment().subtract(j,'days').subtract(10,'weeks'),
    a = then.month()<10?"0"+then.month():then.month(),
    bb = then.date(),
    c = then.year(),
    url = "http://real-chart.finance.yahoo.com/table.csv?s="+s+"&a="+a+"&b="+bb+"&c="+c+"&d="+d+"&e="+ee+"&f="+f+"&g=d&ignore=.csv";
    if(now.day()>=1&&now.day()<=5){
        request(url,parseCSV);
    };
};

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
    if(r){
        var stock = r.request.uri.query.substr(2,r.request.uri.query.indexOf('&')-2);
        if(b.substr(0,1)=="<"){
        }else{
            var raw = b.split("\n"),
            data = [];
            raw.pop();
            raw.shift();
            for(var row in raw){
                var close = parseFloat(raw[row].split(",")[4]);
                data.push(close);
            };
            var latest = moment(raw[0].split(",")[0],"YYYY-MM-DD"),
            macdArr = macd(data),
            thisMacd = macdArr[0],
            thisTrigger = macdArr[1];
            console.log(latest.format("MM/DD/YY")+","+data[0]+","+thisMacd+","+thisTrigger);
        };
    };
};