var http = require('http');
var urlParser = require('url')
var port = process.argv[2];
if (port === undefined) {
    console.log('Please give the port number as 1st argument');
    process.exit(-1);
}

var server = http.createServer(function (req, res) {
    if (req.method != 'GET')
        return res.end('send me a GET\n')
        
    var urlInfo = urlParser.parse(req.url,true);
    var content = {};
    //console.log(urlInfo);
    if (urlInfo.pathname === '/api/parsetime') {
        var date;
        if (urlInfo.query.iso !== undefined) {
            //console.log(urlInfo.query.iso);
            date = new Date(urlInfo.query.iso);
        } else {
            date = new Date();
        }
        content.hour = date.getHours();
        content.minute = date.getMinutes();
        content.second = date.getSeconds();
    } else if (urlInfo.pathname === '/api/unixtime') {
        content.unixtime = new Date().getTime();
    } else {
        return res.end('Invalid GET request, only support "/api/parsetime" and "/api/unixtime" ')
    }
    res.writeHead(200, { 'content-type': 'application/json' });
    res.end(JSON.stringify(content));
});

server.listen(port)