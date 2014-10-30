// Prerequisite:
//  npm install --save express utility superagent cheerio eventproxy 

var express = require('express');
var utility = require('utility');
var superagent= require('superagent');
var cheerio = require('cheerio');
var eventproxy = require('eventproxy');
var url = require('url');

var app = express();

var urlStr = 'https://cnodejs.org/';
var fetchDone = false;
var topicItems1 = [];
var topicItems2 = [];
var topicUrls = [];

superagent.get(urlStr)
.end(function (err,res) {
   if(err) {
       return console.error(err);
   } 
   
   var $ = cheerio.load(res.text);
   
    $('#topic_list .cell').each(function (idx, element) {
        var $element = $(element);
        var href = url.resolve(urlStr,$element.find('.topic_title').attr('href'));
        topicUrls.push(href);
        topicItems1.push({
            title:  $element.find('.topic_title').attr('title'),
            href:   $element.find('.topic_title').attr('href'),
            author: $element.find('img').attr('title')
        });
    });
   
   console.log(topicUrls);
   
   var ep = new eventproxy();
   
   ep.after('readTopicContent', topicUrls.length, function(topics){
       topicItems2 = topics.map(function (topicPair){
           var topicUrl = topicPair[0];
           var topicHtml= topicPair[1];
           var $ = cheerio.load(topicHtml);
           //console.log('Content:' + topicHtml);
           
           return ({
               title: $('.topic_full_title').text().trim(),
               href: topicUrl,
               comment1: $('.reply_content').eq(0).text().trim()
           });
       });
       
       fetchDone = true;
       console.log('Final result:');
       console.log(topicItems2);
    });

   topicUrls.forEach(function (topicUrl){
       superagent.get(topicUrl)
       .end(function (err,res){
          console.log('Fetch ' + topicUrl + ' successful');
          ep.emit('readTopicContent', [topicUrl, res.text]);
       });
    });
});





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
    if (fetchDone) {
        res.send(topicItems1);
    } else {
        res.send('Fetching is not done, try it later!');
    }
})
.get('/spider1', function(req,res,next)
{
    if (fetchDone) {
        res.send(topicItems2);
    } else {
        res.send('Fetching is not done, try it later!');
    }
});

app.listen(3000, function(){
    console.log('App is listening at port 3000');
});