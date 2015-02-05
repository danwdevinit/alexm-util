#!/usr/bin/env node
var request = require('request'),
moment = require('moment'),
s = encodeURI(process.argv[2]),
w = parseInt(process.argv[3]),
now = moment(),
d = now.month()<10?"0"+now.month():now.month(),
e = now.date(),
f = now.year(),
then = now.subtract(w,'days'),
a = then.month()<10?"0"+then.month():then.month(),
b = then.date(),
c = then.year(),
url = "http://real-chart.finance.yahoo.com/table.csv?s="+s+"&a="+a+"&b="+b+"&c="+c+"&d="+d+"&e="+e+"&f="+f+"&g=d&ignore=.csv";
request(url,parseCSV);

function parseCSV(e,r,b){
    if(b.substr(0,1)=="<"){
        throw "ERR: No stock found!"
    };
    var raw = b.split("\n"),
    data = [],
    sum = 0;
    raw.pop();
    raw.shift();
    for(var row in raw){
        var close = parseFloat(raw[row].split(",")[4]);
        data.push(close);
    };
    var prices = data.reverse(),
    last = prices.shift();
    for(var price in prices){
        var up = prices[price]>last;
        if(up==true){
            sum+=1;
        };
        last = prices[price];
    };
    var rsi = sum/(raw.length-1),
    dateStr = "From "+then.format("MMMM Do, YYYY")+" to today, the RSI for "+process.argv[2]+" is: ",
    expStr = "";
    if(rsi>=0.7){
        expStr = "This stock may be considered overbought."
    };
    if(rsi<=0.3){
        expStr = "This stock may be considered oversold."
    };
    if(rsi<=0.0){
        expStr = "Either this stock has experienced no movement, or you need to add more days to your window."
    };
    console.log(dateStr+(rsi*100).toFixed(2)+"%. "+expStr)
};