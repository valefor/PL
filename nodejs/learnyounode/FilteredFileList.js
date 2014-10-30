var fileFilter = require('./FilteredFileListLib.js');

if (process.argv.length < 4){
    console.log('Please provide dir name and extention!');
    process.exit(-1);
} else {
    if (process.argv[3].charAt(0)==='.') {
        console.log('Invalid extention!');
        process.exit(-1);
    }
}

fileFilter(process.argv[2],process.argv[3],function(err, files){
    files.forEach(function (file) {
        console.log(file);
    });
});