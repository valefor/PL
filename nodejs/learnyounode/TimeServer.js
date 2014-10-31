// Prerequisite: for better date output format
//  npm install --save strftime

var net = require('net')
var strftime = require('strftime');

var port = process.argv[2];
if (port === undefined) {
    console.log('Please give the port number as 1st argument');
    process.exit(-1);
}

var server = net.createServer(function (socket) {
  //var date = new Date();
  //var data = date.getFullYear() + '-' + (date.getMonth()+1) + '-' + date.getDate() + ' ' + date.getHours() + ':' +date.getMinutes();

  socket.end(strftime('%F %H:%M', new Date()));
})

server.listen(port);