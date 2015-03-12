// get your requirements in order.
var u = require('underscore'),
basicCSV = require('basic-csv');
//u.nst = require('underscore.nest');
var csvFile = "./domestic-revenue-finance-and-expenditure.csv"


basicCSV.readCSV(csvFile, {dropHeader: true}, function (error, rows) {
    console.log(rows.length)
});