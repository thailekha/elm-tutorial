var jsonServer = require('json-server')
var request = require('superagent');
var async = require('async');
var cheerio = require('cheerio')
var fs = require('fs');

var path = process.cwd()+"/dict.txt";
var wordfindUrl = 'http://www.wordfind.com/contains/';
var cambridgeUrl = 'http://dictionary.cambridge.org/search/english-vietnamese/direct/?q=';

var words = {};

function processData(data, words) {
    data
        .split('\n@')
        .map(function(word) {
            var defs = word.split('\n');
            defs[0] = defs[0].split(/\/*\//)
                .map(function(part) {
                    return part.trim();
                })
                .filter(function(part) {
                    return part.length > 0;
                });
            return defs.filter(function(part) {
                return part.length > 0;
            });
        })
        .forEach(function(word) {
            var vocab = word.shift();
            var wordItem = vocab[0].toLowerCase();
            var pronun = vocab[1];
            var def = word;
            def.unshift(pronun);
            words[wordItem] = def;
        });
}

processData(fs.readFileSync(path, 'utf-8'), words);

// Returns an Express server
var server = jsonServer.create()

// Set default middlewares (logger, static, cors and no-cache)
server.use(jsonServer.defaults())

function doRequest(url, cheerioQuery, toMap, cb) {
    request.get(url, function(err, response) {
        if (err) throw err;
        //var listItems = cheerio.load(response.text)('span.w');
        var listItems = cheerio.load(response.text)(cheerioQuery);
        var results = [];
        for (var key in listItems) {
            if(parseInt(key) >= 0) {
                results = results.concat(listItems[key].children.map(function(child) {
                    return {
                        "word": toMap(child).data,
                        "def": words[toMap(child).data.toLowerCase()]
                    };
                }));
            }
        }
        cb(results);
    });
}

server.get('/lookupword', function(req, res) {
    async.parallel({
        wordfind(cb) {
            doRequest(wordfindUrl + req.query.w, 'li.defLink',
                function(child) {
                    return child.children[0];
                },
                function(results) {
                    cb(null, results);
                });
        },
        cambridge(cb) {
            doRequest(cambridgeUrl + req.query.w, 'span.w', 
                function(child) {
                    return child;
                },
                function(results) {
                    cb(null, results);
                });
        }
    },
    function(err, results) {
        // {
        //     wordfind: [],
        //     cambridge:[]
        // }
        res.json(results);
    });
});

var router = jsonServer.router('db.json')
server.use(router);

console.log('Listening at 4000')
server.listen(4000)