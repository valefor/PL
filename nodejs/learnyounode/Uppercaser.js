// Prerequisite: for better date output format
//  npm install --save through2-map

var http = require('http');
var map = require('through2-map');
var port = process.argv[2];
if (port === undefined) {
    console.log('Please give the port number as 1st argument');
    process.exit(-1);
}

var server = http.createServer(function (req, res) {
    if (req.method != 'POST')
        return res.end('send me a POST\n')
        
    req.pipe(map(function (chunk){
        return chunk.toString().toUpperCase()
    })).pipe(res);
});

server.listen(port)