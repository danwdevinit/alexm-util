#!/usr/bin/env node --stack-size=65500
var basicCSV = require('basic-csv'),
syl = require('sylvester'),
cmb = require('cmb'),
moment = require('moment'),
csvFile = process.argv[2],
underlying = 0,
upState = 0,
downState = 0,
expiriesAhead = process.argv[3]?parseInt(process.argv[3]):0,
date="",
data = {},
probabilities = {},
changes = [];
for(var i = 1; i<21; i++){
    changes.push(i/2);
}

function isNumber(n) {
  return !isNaN(parseFloat(n)) && isFinite(n);
};

function CBOEdate(string){
    var monthStr = string.substr(0,3),
    weekStr = string.split(" ")[0][3]?parseInt(string.split(" ")[0][3]):3,
    yearStr = string.split(" ")[1],
    fridays = ["",moment(monthStr+" "+yearStr,"MMM YY").day("Friday"),moment(monthStr+" "+yearStr,"MMM YY").day("Friday").add(1,'w'),moment(monthStr+" "+yearStr,"MMM YY").day("Friday").add(2,'w'),moment(monthStr+" "+yearStr,"MMM YY").day("Friday").add(3,'w'),moment(monthStr+" "+yearStr,"MMM YY").day("Friday").add(4,'w'),moment(monthStr+" "+yearStr,"MMM YY").day("Friday").add(5,'w')];
    date = fridays[weekStr];
    return date;
};

function isCBOEdate(string){
    var monthStr = string.substr(0,3),
    weekStr = string.split(" ")[0][3]?parseInt(string.split(" ")[0][3]):3,
    yearStr = string.split(" ")[1]?string.split(" ")[1]:false,
    date = moment(monthStr+" "+yearStr,"MMM YY");
    return date.isValid();
};

basicCSV.readCSV(csvFile, {dropHeader: true}, function (error, rows) {
    underlying = rows[2][0];
    var rawData = rows.slice(6,rows.length);
    var dateIndicies = [];
    for(var i = 0; i < rawData.length; i++){
        var row = rawData[i];
        if(isCBOEdate(row[0])){
            var date = CBOEdate(row[0]);
            dateObj = {"date":date.format("MM/DD/YY"),"index":i};
            dateIndicies.push(dateObj);
        };
    };
    for(var i = 0; i < dateIndicies.length; i++){
        var thisIndex = dateIndicies[i],
        nextIndex = dateIndicies[i+1]?dateIndicies[i+1]:rawData.length,
        rawSlice = rawData.slice(thisIndex.index,nextIndex.index);
        data[thisIndex.date] = [];
        for(var j = 0; j < rawSlice.length; j++){
            var row = rawSlice[j];
            if(isNumber(row[10]) && row[10]>0){
                //Calls
                if(isNumber(row[2]) && row[2]>0){
                    var optObj = {};
                    optObj.call = 1;
                    optObj.strike = parseFloat(row[10]);
                    optObj.price = parseFloat(row[2]);
                    data[thisIndex.date].push(optObj);
                };
                //Puts
                if(isNumber(row[15]) && row[15]>0){
                    var optObj = {};
                    optObj.call = 0;
                    optObj.strike = parseFloat(row[10]);
                    optObj.price = parseFloat(row[15]);
                    data[thisIndex.date].push(optObj);
                };
            };
        };
    };
});

function analyze(){
    var dates = Object.keys(data);
    date = dates[expiriesAhead];
    for(var j = 0; j<changes.length; j++){
        var change = changes[j],
        upState = underlying*(1+change),
        downState = underlying*(1-change);
        options = data[date],
        Farr = [],
        Parr = [];
        probabilities[change] = {"down":[],"neutral":[],"up":[]};
        for(var i = 0; i < options.length; i++){
            var option = options[i],
            strike = option.strike,
            price = option.price,
            call = option.call;
            Parr.push(price);
            if(call){
                var upVal = Math.max(upState-strike,0),
                neuVal = Math.max(underlying-strike,0),
                downVal = Math.max(downState-strike,0);
            }else{
                var upVal = Math.max(strike-upState,0),
                neuVal = Math.max(strike-underlying,0),
                downVal = Math.max(strike-downState,0);
            };
            var rowData = {"row":[downVal,neuVal,upVal],"call":call,"strike":strike};
            Farr.push(rowData);
        };
        var range = [];
        for(var i = 0; i < Farr.length; i++){range.push(i);};
        var com = new cmb(range,3);
        com.each(
            function(val){
                var F = $M([Farr[val[0]].row,Farr[val[1]].row,Farr[val[2]].row]),
                P = $M([Parr[val[0]],Parr[val[1]],Parr[val[2]]]);
                if(F.inv()){
                    var arrowPrices = F.inv().x(P).toArray(),
                    arrowFlat = [];
                    arrowFlat = arrowFlat.concat.apply(arrowFlat,arrowPrices);
                    var arrowMax = Math.max.apply(Math, arrowFlat),
                    arrowMin = Math.min.apply(Math, arrowFlat),
                    arrowSum = arrowFlat.reduce(function(a,b){return a+b;});
                    if(arrowMin>=0 && arrowMax>0 && arrowMax<=1 && arrowSum<=1){
                        var inducedProb = arrowFlat.map(function(num){return num/arrowSum;});
                        probabilities[change].down.push({"prob":inducedProb[0],"call1":Farr[val[0]].call,"strike1":Farr[val[0]].strike,"call2":Farr[val[1]].call,"strike2":Farr[val[1]].strike,"call3":Farr[val[2]].call,"strike3":Farr[val[2]].strike});
                        probabilities[change].neutral.push({"prob":inducedProb[1],"call1":Farr[val[0]].call,"strike1":Farr[val[0]].strike,"call2":Farr[val[1]].call,"strike2":Farr[val[1]].strike,"call3":Farr[val[2]].call,"strike3":Farr[val[2]].strike});
                        probabilities[change].up.push({"prob":inducedProb[2],"call1":Farr[val[0]].call,"strike1":Farr[val[0]].strike,"call2":Farr[val[1]].call,"strike2":Farr[val[1]].strike,"call3":Farr[val[2]].call,"strike3":Farr[val[2]].strike});
                    };
                };
            }
        );
    };
};

function print(){
    console.log("date,change,state,prob,call,strike");
    for(var i = 1; i<21; i++){
        var change = i/2,
        percent = probabilities[change];
        if(percent){
            for(var state in percent){
                var results = percent[state];
                for(var j = 0; j < results.length;j++){
                    var value = results[j].prob;
                    console.log(date+","+change+","+state+","+value+","+results[j].call1+","+results[j].strike1);
                    console.log(date+","+change+","+state+","+value+","+results[j].call2+","+results[j].strike2);
                    console.log(date+","+change+","+state+","+value+","+results[j].call3+","+results[j].strike3);
                };
            };
        };
    };
};

function exitHandler(options, err) {
    if (options.cleanup){
        analyze();
        print();
        process.exit();
    };
    if (err){console.log(err.stack)};
    if (options.exit){process.exit();};
};
process.on('exit', exitHandler.bind(null,{cleanup:true}));
process.on('SIGINT', exitHandler.bind(null, {exit:true}));
process.on('uncaughtException', exitHandler.bind(null, {exit:true}));