var jsonServer = require('json-server')
var request = require('superagent');
var cheerio = require('cheerio')

var url = 'http://www.wordfind.com/contains/';

// Returns an Express server
var server = jsonServer.create()

// Set default middlewares (logger, static, cors and no-cache)
server.use(jsonServer.defaults())

server.get('/lookupword', function(req, res) {
    request.get(url + req.query.w, function(err, response) {
        if (err) throw err;
        var listItems = cheerio.load(response.text)('li.defLink');
        var results = [];
        for (var key in listItems) {
            if(parseInt(key) >= 0) {
                results = results.concat(listItems[key].children.map(function(aTag) {
                    return aTag.children[0].data;
                }));
            }
        }
        res.json({"wordfind": results});
    });
});

var router = jsonServer.router('db.json')
server.use(router);

console.log('Listening at 4000')
server.listen(4000)