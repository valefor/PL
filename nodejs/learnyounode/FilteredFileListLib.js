var path = require('path');
var fs = require('fs');

module.exports = function (dir, ext, callback) {
    fs.readdir(dir, function (err, files) {
        if (err) return callback(err);
        
        // Cooler solution
        var list = files.filter( function (file){
           return path.extname(file) == '.'+ext;
        });
        // Use path library
        /*
        files.forEach(function (file) {
            if ( path.extname(file) == '.'+ext) {
                list.push(file);
            }
        });
        */
        callback(null, list);
    });
};
