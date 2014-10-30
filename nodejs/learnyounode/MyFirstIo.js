var fs = require('fs');

//var buffer = fs.readFileSync(process.argv[2]);
//console.log(buffer.toString().split('\n').length - 1);

fs.readFile(process.argv[2], function (err, data) {
    // fs.readFile(file, 'utf8', callback) can also be used
    console.log(data.toString().split('\n').length - 1);
});

