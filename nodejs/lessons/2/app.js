// Prerequisite:
//  npm install --save express utility superagent cheerio 

var express = require('express');
var utility = require('utility');
var superagent= require('superagent');
var cheerio = require('cheerio');

var app = express();

app
.get('/', function(req,res)
{
    res.send("Hello World!");
})
.get('/md5', function(req,res)
{
    var q = req.query.q;
    
    var md5 = "Value:"+ q + "<br/>MD5:" + utility.md5(q);

    res.send(md5);
})
.get('/spider', function(req,res,next)
{
    superagent.get('https://cnodejs.org/')
    .end(function (err, sres) {
        if (err) {
            return next(err);
        }
        
        // Load jQuery interface
        var $ = cheerio.load(sres.text);
        var items = [];
        $('#topic_list .cell').each(function (idx, element) {
            var $element = $(element);
            items.push({
                title:  $element.find('.topic_title').attr('title'),
                href:   $element.find('.topic_title').attr('href'),
                author: $element.find('img').attr('title')
            });
        });
    
        res.send(items);
    });
});

app.listen(3000, function(){
    console.log('App is listening at port 3000');
});