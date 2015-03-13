#!/usr/bin/env node --stack-size=65500
var request = require('request'),
csv = require('to-csv'),
fs = require('fs'),
baseUrl = "http://139.191.244.79/gnasystem/api001.aspx?service=InfoRM&workflow=193&request=GetData&iso3=AFG&indicators=all&format=json&{}",
output = {"indicators":[],"time-series":[],"data":[]};

request(baseUrl,parseBase);

function parseBase(e,r,b){
    if(e){throw e};
    var result = JSON.parse(b),
    countries = result["methodology"][0]["Iso3List"],
    countryLen = countries.length,
    indicators = result["indicators"],
    indicatorLen = indicators.length;
    for(var i = 0; i < indicatorLen; i++){
        var indicator = indicators[i];
        output["indicators"].push(indicator);
    };
    for(var i = 0; i < countryLen; i++){
        var iso3 = countries[i]["Iso3"],
        countryUrl = "http://139.191.244.79/gnasystem/api001.aspx?service=InfoRM&workflow=193&request=GetData&iso3="+iso3+"&indicators=all&format=json&{}";
        request(countryUrl,parseCountry);
    };
};

function parseCountry(e,r,b){
    if(e){throw e};
    var countryResult = JSON.parse(b),
    iso3 = countryResult["Country_Info"][0]["Iso3"],
    country = countryResult["Country_Info"][0]["Country"],
    trends = countryResult["Trends"],
    trendsLen = trends.length,
    trendsNHSVII = countryResult["Trends_NHSVII"],
    trendsNHSVIILen = trendsNHSVII.length,
    data = countryResult["data"],
    dataLen = data.length;
    console.log("Parsing "+country+"...");
    for (var i = 0; i < trendsLen; i++) {
        var trend = trends[i],
        obj = trend;
        obj["Iso3"] = iso3;
        obj["Country"] = country;
        obj["ShortDescription"] = "";
        obj["ParentLevel1"] = "";
        output["time-series"].push(obj);
    };
    for (var i = 0; i < trendsNHSVIILen; i++) {
        var trend = trendsNHSVII[i],
        obj = trend;
        obj["Iso3"] = iso3;
        obj["Country"] = country;
        obj["StepNumber"] = 0;
        output["time-series"].push(obj);
    };
    for (var i = 0; i < dataLen; i++){
        var datum = data[i];
        output["data"].push(datum);
    };
};

function print(){
    console.log("Writing Indicator CSV...")
    fs.writeFileSync('./indicators.csv',csv(output["indicators"]));
    console.log("Done.")
    console.log("Writing Time-Series CSV...")
    fs.writeFileSync('./time-series.csv',csv(output["time-series"]));
    console.log("Done.")
    console.log("Writing Data CSV...")
    fs.writeFileSync('./data.csv',csv(output["data"]));
    console.log("Done.")
    console.log("Web-scrape complete.")
};

function exitHandler(options, err) {
    if (options.cleanup){
        print();
        process.exit();
    };
    if (err){console.log(err.stack)};
    if (options.exit){process.exit();};
};
process.on('exit', exitHandler.bind(null,{cleanup:true}));
process.on('SIGINT', exitHandler.bind(null, {exit:true}));
process.on('uncaughtException', exitHandler.bind(null, {exit:true}));