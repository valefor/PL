var http = require('http');

var url = process.argv[2];

if (url === undefined) {
    console.log('Please give the url as argument');
    process.exit(-1);
}

http
.get(url, function (res) {
    //console.log('Got response:' + res.statusCode);
    res.setEncoding('utf8');
    res.on('data', console.log);
    res.on('error', console.error);
})
.on('error', function(err) {
    console.log('Got error:' + err.message);
});