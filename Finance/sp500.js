#!/usr/bin/env node
var request = require('request'),
moment = require('moment'),
cheerio = require('cheerio'),
colors = require('colors'),
wikiURL = 'http://en.wikipedia.org/wiki/List_of_S%26P_500_companies';

request(wikiURL,parseWiki);

function parseWiki(e,r,b){
    var $ = cheerio.load(b),
    links = $('a');
    for(var i = 0; i < links.length; i++){
        if($(links[i]).attr('href')){
            if($(links[i]).attr('href').substr(0,32)=="https://www.nyse.com/quote/XNYS:"||$(links[i]).attr('href').substr(0,29)=="http://www.nasdaq.com/symbol/"){
                var s = $(links[i]).text(),
                now = moment(),
                d = now.month()<10?"0"+now.month():now.month(),
                ee = now.date(),
                f = now.year(),
                then = moment().subtract(10,'weeks'),
                a = then.month()<10?"0"+then.month():then.month(),
                bb = then.date(),
                c = then.year(),
                url = "http://real-chart.finance.yahoo.com/table.csv?s="+s+"&a="+a+"&b="+bb+"&c="+c+"&d="+d+"&e="+ee+"&f="+f+"&g=d&ignore=.csv";
                request(url,parseCSV);
            };
        };
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

function parseCSV(e,r,b){
    if(r){
        var stock = r.request.uri.query.substr(2,r.request.uri.query.indexOf('&')-2);
        if(b.substr(0,1)=="<"){
            console.log("WARN: No stock found for "+stock)
            console.log("")
        };
        var data = [],
        raw = b.split("\n");
        raw.pop();
        raw.shift();
        for(var row in raw){
            var close = parseFloat(raw[row].split(",")[4]);
            data.push(close);
        };
            var macd = ema(data,12) - ema(data,26),
            macds = [];
            data.shift();
            data.shift();
            for(var i = 0; i < 7; i++){
                var tempMacd = ema(data,12) - ema(data,26);
                macds.push(tempMacd)
                data.shift();
            };
            trigger = ema(macds,macds.length);
        var sum = 0,
        prices = data.reverse(),
        last = prices.shift();
        for(var price in prices){
            var up = prices[price]>last;
            if(up==true){
                sum+=1;
            };
            last = prices[price];
        };
        var rsi = sum/(raw.length-1);
        var data2 = [],
        raw2 = b.split("\n");
        raw2.pop();
        raw2.shift();
        raw2.shift();
        for(var row in raw2){
            var close = parseFloat(raw2[row].split(",")[4]);
            data2.push(close);
        };
        var macd2 = ema(data2,12) - ema(data2,26),
            macds2 = [];
            data2.shift();
            data2.shift();
            for(var i = 0; i < 7; i++){
                var tempMacd2 = ema(data2,12) - ema(data2,26);
                macds2.push(tempMacd2)
                data2.shift();
            };
        trigger2 = ema(macds2,macds2.length);
        var data3 = [],
        raw3 = b.split("\n");
        raw3.pop();
        raw3.shift();
        raw3.shift();
        raw3.shift();
        for(var row in raw3){
            var close = parseFloat(raw3[row].split(",")[4]);
            data3.push(close);
        };
        var macd3 = ema(data3,12) - ema(data3,26),
            macds3 = [];
            data3.shift();
            data3.shift();
            for(var i = 0; i < 7; i++){
                var tempMacd3 = ema(data3,12) - ema(data3,26);
                macds3.push(tempMacd3)
                data3.shift();
            };
        trigger3 = ema(macds3,macds3.length);
        var data4 = [],
        raw4 = b.split("\n");
        raw4.pop();
        raw4.shift();
        raw4.shift();
        raw4.shift();
        for(var row in raw4){
            var close = parseFloat(raw4[row].split(",")[4]);
            data4.push(close);
        };
        var macd4 = ema(data4,12) - ema(data4,26),
            macds4 = [];
            data4.shift();
            data4.shift();
            for(var i = 0; i < 7; i++){
                var tempMacd4 = ema(data4,12) - ema(data4,26);
                macds4.push(tempMacd4)
                data4.shift();
            };
        trigger4 = ema(macds4,macds4.length);
        if(macd>trigger&&macd2<trigger2){
            console.log(stock.green+" experienced a bullish cross from yesterday to today.".green)
            if(rsi>=0.7){
                console.log("It's also considered overbought by the RSI.".red)
            };
            if(rsi<=0.3){
                console.log("It's also considered oversold by the RSI.".green)
            };
            console.log("")
        };
        if(macd<trigger&&macd2>trigger2){
            console.log(stock.red+" experienced a bearish cross from yesterday to today.".red)
            if(rsi>=0.7){
                console.log("It's also considered overbought by the RSI.".red)
            };
            if(rsi<=0.3){
                console.log("It's also considered oversold by the RSI.".green)
            };
            console.log("")
        };
        if(macd>trigger&&macd2>trigger2&&macd3<trigger3){
            console.log(stock+" experienced a bullish cross two days ago and maintained that trend today.")
            if(rsi>=0.7){
                console.log("It's also considered overbought by the RSI.".red)
            };
            if(rsi<=0.3){
                console.log("It's also considered oversold by the RSI.".green)
            };
            console.log("")
        };
        if(macd<trigger&&macd2<trigger2&&macd3>trigger3){
            console.log(stock+" experienced a bearish cross two days ago and maintained that trend today.")
            if(rsi>=0.7){
                console.log("It's also considered overbought by the RSI.".red)
            };
            if(rsi<=0.3){
                console.log("It's also considered oversold by the RSI.".green)
            };
            console.log("")
        };
        if(macd>trigger&&macd2>trigger2&&macd3>trigger3&&macd4<trigger4){
            console.log(stock+" experienced a bullish cross three days ago and maintained that trend for the past two days.")
            if(rsi>=0.7){
                console.log("It's also considered overbought by the RSI.".red)
            };
            if(rsi<=0.3){
                console.log("It's also considered oversold by the RSI.".green)
            };
            console.log("")
        };
        if(macd<trigger&&macd2<trigger2&&macd3<trigger3&&macd4>trigger4){
            console.log(stock+" experienced a bearish cross three days ago and maintained that trend for the past two days.")
            if(rsi>=0.7){
                console.log("It's also considered overbought by the RSI.".red)
            };
            if(rsi<=0.3){
                console.log("It's also considered oversold by the RSI.".green)
            };
            console.log("")
        };
        if(macd>trigger&&macd2>trigger2&&macd3>trigger3&&macd4>trigger4){
            console.log(stock+" has experienced a protracted uptrend for the past four days.")
            if(rsi>=0.7){
                console.log("It's also considered overbought by the RSI.".red)
            };
            if(rsi<=0.3){
                console.log("It's also considered oversold by the RSI.".green)
            };
            console.log("")
        };
        if(macd<trigger&&macd2<trigger2&&macd3<trigger3&&macd4<trigger4){
            console.log(stock+" has experienced a protracted downtrend for the past four days.")
            if(rsi>=0.7){
                console.log("It's also considered overbought by the RSI.".red)
            };
            if(rsi<=0.3){
                console.log("It's also considered oversold by the RSI.".green)
            };
            console.log("")
        };
        if(macd<trigger&&macd2>trigger2&&macd3<trigger3&&macd4>trigger4){
            console.log(stock.yellow+" is extremely volatile!".yellow)
            if(rsi>=0.7){
                console.log("It's also considered overbought by the RSI.".red)
            };
            if(rsi<=0.3){
                console.log("It's also considered oversold by the RSI.".green)
            };
            console.log("")
        };
        if(macd>trigger&&macd2<trigger2&&macd3>trigger3&&macd4<trigger4){
            console.log(stock.yellow+" is extremely volatile!".yellow)
            if(rsi>=0.7){
                console.log("It's also considered overbought by the RSI.".red)
            };
            if(rsi<=0.3){
                console.log("It's also considered oversold by the RSI.".green)
            };
            console.log("")
        };
    }else{console.log("Something went wrong. No request found.");console.log("")};
};