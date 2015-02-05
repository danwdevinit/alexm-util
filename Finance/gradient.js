#!/usr/bin/env node --stack-size=65500
var x = -500,
fx = 0,
err = 0.1;

function f(x){
    return Math.pow(x,2)-fx;
};

function df(x){
    return 2*x;
};

while(Math.abs(f(x))>err){
    if(df(x)!=0){
        x = x - f(x)/df(x);
    };
};

console.log(x);