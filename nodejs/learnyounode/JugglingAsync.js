var http = require('http');
var bl = require('bl');

var results = [];
var count = 0;

var url1 = process.argv[2];
var url2 = process.argv[3];
var url3 = process.argv[4];

if (url1 === undefined || url2 === undefined || url3 === undefined ) {
    console.log('Please give the 3 urls as argument');
    process.exit(-1);
}

http
.get(url1, function (res) {
    //console.log('Got response:' + res.statusCode);
    res.setEncoding('utf8');
    res.pipe(bl(function (err,data){
        if (err) {
            return console.error(err);
        }
        results[0] = data.toString();
        checkAndPrintResults(++count);
    }));
});

http
.get(url2, function (res) {
    //console.log('Got response:' + res.statusCode);
    res.setEncoding('utf8');
    res.pipe(bl(function (err,data){
        if (err) {
            return console.error(err);
        }
        results[1] = data.toString();
        checkAndPrintResults(++count);
    }));
});

http
.get(url3, function (res) {
    //console.log('Got response:' + res.statusCode);
    res.setEncoding('utf8');
    res.pipe(bl(function (err,data){
        if (err) {
            return console.error(err);
        }
        results[2] = data.toString();
        checkAndPrintResults(++count);
    }));
});

function checkAndPrintResults(ct) {
    if (ct === 3) {
        for (var i = 0; i < results.length; i++) {
            console.log(results[i]);
        } 
    }
}

