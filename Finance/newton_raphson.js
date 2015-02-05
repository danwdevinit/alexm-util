#!/usr/bin/env node --stack-size=65500
var createDist = require( 'distributions-normal' ),
normal = createDist();
normal.variance(1);
normal.mean(0);

var sigma = 1.01,
mktPrice = 10,
strike = 770,
expiry = 15/252,
asset = 762.7,
intRate = 0.045,
err = 0.01;

function cdf(x){
    return normal.cdf([x])[0];
};

function pdf(x){
    return normal.pdf([x])[0];
};

//function cdf(x){
//    var d = 1 / (1 + 0.2316419 * Math.abs(x)),
//    a1 = 0.31938153,
//    a2 = -0.356563782,
//    a3 = 1.781477937,
//    a4 = -1.821255978,
//    a5 = 1.330274429,
//    temp = a5;
//    temp = a4 + d*temp;
//    temp = a3 + d*temp;
//    temp = a2 + d*temp;
//    temp = a1 + d*temp;
//    temp = d * temp;
//    var cdf = 1 - (1 / Math.sqrt(2*Math.PI)*Math.exp(-0.5*x*x)*temp);
//    if(x<0){cdf = 1 - cdf};
//    return cdf;
//};


function v(sigma){
    var d1 = (Math.log(asset/strike)+(intRate+0.5*sigma*sigma)*expiry)/(sigma*Math.sqrt(expiry));
    var d2 = d1 - sigma*Math.sqrt(expiry),
    priceError = (asset * cdf(d1) - strike * Math.exp(-1*intRate*expiry)*cdf(d2)) - mktPrice;
    return priceError;
};

function dv(sigma){
    var d1 = (Math.log(asset/strike)+(intRate+0.5*sigma*sigma)*expiry)/(sigma*Math.sqrt(expiry));
    //var vega = asset * Math.sqrt(expiry / Math.PI / 2)*Math.exp(-0.5*d1*d1);
    var vega = asset*Math.sqrt(expiry)*pdf(d1);
    return vega;
};

while(Math.abs(v(sigma))>err){
    if(dv(sigma)!=0){
        sigma = sigma - v(sigma)/dv(sigma);
    };
};

console.log(sigma);