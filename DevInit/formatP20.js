function formatP20(num){
    return num<1000000?" < 0.1m":niceNum(Math.round(d3.round(num,-5)));
};