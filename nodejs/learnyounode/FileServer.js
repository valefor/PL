// HowTo:
//  node FileServer.js 8000 ./BabySteps.js
var http = require('http');
var bl = require('bl');
var urlParser = require('url')
var fs = require('fs');

var port = process.argv[2];
var filePath = process.argv[3];
if (port === undefined || filePath === undefined) {
    console.log('Please give the port number as 1st and the file path name as 2nd argument');
    process.exit(-1);
}

var fileStream = fs.createReadStream(filePath);

var server = http.createServer(function (req, res) {
    // var urlInfo = urlParser.parse(req.url);
    
    //console.log(urlInfo);
    //fileStream.pipe(process.stdout);
    res.writeHead(200, { 'content-type': 'text/plain' });
    fileStream.pipe(res);
    /*
    fileStream.pipe(bl(function (err,data){
        if (err) {
            return console.error(err);
        }
        res.end(data.toString());
    }));
    */
});

server.listen(port)