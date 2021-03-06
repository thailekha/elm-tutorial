var express = require('express');
var request = require('superagent');
//.debug = true
var bodyParser = require('body-parser');
var async = require('async');
var cheerio = require('cheerio');
var _ = require('lodash');
var fs = require('fs');
var express = require('express');
var path = require('path');
require('dotenv').config();

var dictPath = process.cwd() + "/assets/dict.txt";
var words = {};
var wordnikAuthUrl = 'http://api.wordnik.com/v4/account.json/authenticate/';
var wordnikToken = '';

var wordfindUrl = 'http://www.wordfind.com/contains/';
var cambridgeUrl = 'http://dictionary.cambridge.org/search/english-vietnamese/direct/?q=';
var wordnikUrl = 'https://api.wordnik.com/v4/word.json/';
var wordnikQueries = '/relatedWords?useCanonical=true&relationshipTypes=';

var server = express();

server.set('port', (process.env.PORT || 5000));

server.use(bodyParser.json()); // support json encoded bodies
server.use(bodyParser.urlencoded({ extended: true })); // support encoded bodies

//TODO: Set default middlewares (logger, static, cors and no-cache)

server.use('/',express.static(path.join(__dirname, 'dist')));

server.get('/api/lookupword', function(req, res) {
    async.parallel({
        wordfind(cb) {
            reqCheerio(wordfindUrl + req.query.w, 'li.defLink',
                function(child) {
                    return child.children[0];
                },
                function(results) {
                    cb(null, results);
                });
        },
        cambridge(cb) {
            reqCheerio(cambridgeUrl + req.query.w, 'span.w', 
                function(child) {
                    return child;
                },
                function(results) {
                    cb(null, results);
                });
        },
        synonyms(cb) {
            request
                .get(wordnikUrl + req.query.w + wordnikQueries + 'synonym' + '&api_key=' + process.env.API_KEY)
                .set('Accept', 'application/json')
                .end(function(err, res) {
                    if (err) throw err;
                    cb(null, formatWordnikResponse(res.body));
                });
        },
        antonyms(cb) {
            request
                .get(wordnikUrl + req.query.w + wordnikQueries + 'antonym' + '&api_key=' + process.env.API_KEY)
                .set('Accept', 'application/json')
                .end(function(err, res) {
                    if (err) throw err;
                    cb(null, formatWordnikResponse(res.body));
                });
        }
    },
    function(err, results) {
        // {
        //     wordfind: [],
        //     cambridge:[],
        //     synonyms: [],
        //     antonyms: []
        // }
        res.json(results);
    });
});

server.post('/api/save', function(req,res) {
    //console.log("body ", req.body);
    var text = '//================\n' + req.body.query + '\n//================\n';

    text += 'o~~~~~ Có trong ~~~~~o\n';
    text += req.body.wordfind.map(function(wordData) {
        return wordData.word + '\n' + wordData.def.join('\n')
    }).join('\n');

    text += '\n\no~~~~~ Từ loại ~~~~~o\n';
    text += req.body.cambridge.map(function(wordData) {
        return wordData.word + '\n' + wordData.def.join('\n')
    }).join('\n');

    text += '\n\no~~~~~ Đồng nghĩa ~~~~~o\n';
    text += req.body.synonyms.map(function(wordData) {
        return wordData.word
    }).join('\n');

    text += '\n\no~~~~~ Trái nghĩa ~~~~~o\n';
    text += req.body.antonyms.map(function(wordData) {
        return wordData.word
    }).join('\n');

    fs.writeFile("./dump/save.txt", text, (err) => {
        if (err) throw err;
        console.log('The file has been saved!');
        res.json({});
    });
});

server.get('/download', function(req,res) {
    res.download("./dump/save.txt", function(errDownload) {
        if(errDownload) {
            console.log(errDownload);
        }
    });
});

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

function initServerFull(server, dictionaryPath, words, wordnikAuthUrl, username, password, apikey, wordnikToken) {
    processData(fs.readFileSync(dictionaryPath, 'utf-8'), words);
    request
        .get(wordnikAuthUrl + username + '?password=' + password + '&api_key=' + apikey)
        .set('Accept', 'application/json')
        .end(function(err, res) {
            if (err) throw err;
            wordnikToken = res.body;

            server.listen(server.get('port'), function() {
              console.log('Node app is running at', server.get('port'));
            });
        });
}

function initServerTest(server) {
    server.listen(server.get('port'), function() {
      console.log('Node app is running at', server.get('port'));
    });
}

function formatWordnikResponse(body) {
    return _.flatten(body.map(function(wordnikRes) {
        return wordnikRes.words;
    })).map(function(word) {
        return {
            "word": word
        }
    });
}

function reqCheerio(url, cheerioQuery, toMap, cb) {
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

initServerFull(server, dictPath, words, wordnikAuthUrl, process.env.USERNAME, process.env.PASSWORD, process.env.API_KEY, wordnikToken);
//initServerTest(server);

server._router.stack.forEach(function(r){
  if (r.route && r.route.path){
    console.log(r.route.path)
  }
})