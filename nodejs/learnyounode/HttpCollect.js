// Prerequisite:
//  npm install --save bl
var http = require('http');
var bl = require('bl');

var url = process.argv[2];

if (url === undefined) {
    console.log('Please give the url as argument');
    process.exit(-1);
}

http
.get(url, function (res) {
    //console.log('Got response:' + res.statusCode);
    res.setEncoding('utf8');
    res.pipe(bl(function (err,data){
        if (err) {
            return console.error(err);
        }
        console.log(data.toString().length);
        console.log(data.toString());
    }));
});