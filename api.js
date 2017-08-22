var jsonServer = require('json-server')
var request = require('superagent');
var cheerio = require('cheerio')
var fs = require('fs');

var path = process.cwd()+"/dict.txt";
var url = 'http://www.wordfind.com/contains/';
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

server.get('/lookupword', function(req, res) {
    request.get(url + req.query.w, function(err, response) {
        if (err) throw err;
        var listItems = cheerio.load(response.text)('li.defLink');
        var results = [];
        for (var key in listItems) {
            if(parseInt(key) >= 0) {
                results = results.concat(listItems[key].children.map(function(aTag) {
                    return {
                        "word": aTag.children[0].data,
                        "def": words[aTag.children[0].data.toLowerCase()]
                    };
                }));
            }
        }
        console.log({"wordfind": results});
        res.json({"wordfind": results});
    });
});

var router = jsonServer.router('db.json')
server.use(router);

console.log('Listening at 4000')
server.listen(4000)