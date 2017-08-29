var jsonServer = require('json-server');
var request = require('superagent');
//.debug = true
var bodyParser = require('body-parser');
var async = require('async');
var cheerio = require('cheerio');
var _ = require('lodash');
var fs = require('fs');
require('dotenv').config();

var path = process.cwd() + "/dict.txt";
var words = {};
var wordnikAuthUrl = 'http://api.wordnik.com/v4/account.json/authenticate/';
var wordnikToken = '';

var wordfindUrl = 'http://www.wordfind.com/contains/';
var cambridgeUrl = 'http://dictionary.cambridge.org/search/english-vietnamese/direct/?q=';
var wordnikUrl = 'https://api.wordnik.com/v4/word.json/';
var wordnikQueries = '/relatedWords?useCanonical=true&relationshipTypes='

// Returns an Express server
var server = jsonServer.create()

server.use(bodyParser.json()); // support json encoded bodies
server.use(bodyParser.urlencoded({ extended: true })); // support encoded bodies

// Set default middlewares (logger, static, cors and no-cache)
server.use(jsonServer.defaults())

server.get('/lookupword', function(req, res) {
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

function printWordData(wordData, def) {
    var res = wordData.word + '\n' ;
    if(def) {
        wordData.def.forEach(function(d) {
            res += d + '\n';
        })
    }
    console.log(res);
    return  res;
        
}

function add(a, b, noNewline) {
    var res = a + b;
    if(!noNewline) {
        res += '\n';
    }
    return res;
}

server.post('/save', function(req,res) {
    console.log("body ", req.body);
    var text = '//================\n' + req.body.query + '\n//================\n';
    [["wordfind", "Có trong"], ["cambridge", "Từ loại"], ["synonyms", "Đồng nghĩa"], ["antonyms", "Trái nghĩa"]]
        .forEach(function(key) {
            text = add(text, 'o~~~~~ ' + key[1] + ' ~~~~~o');
            req.body[key[0]].forEach(function(wordData) {
                text = add(
                    text, 
                    printWordData(wordData, key[0] === "wordfind" || key[0] === "cambridge" ),
                    key[0] === "synonyms" || key[0] === "antonyms"
                );
            }.bind(this));
        })
    fs.writeFile("save.txt", text, (err) => {
        if (err) throw err;
        console.log('The file has been saved!');
        res.json({});
    });
});

var router = jsonServer.router('db.json')
server.use(router);

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

function initServer(server, dictionaryPath, words, wordnikAuthUrl, username, password, apikey, wordnikToken) {
    processData(fs.readFileSync(dictionaryPath, 'utf-8'), words);
    request
        .get(wordnikAuthUrl + username + '?password=' + password + '&api_key=' + apikey)
        .set('Accept', 'application/json')
        .end(function(err, res) {
            if (err) throw err;
            wordnikToken = res.body;

            console.log('Server initialized, Listening at 4000');
            server.listen(4000);
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

initServer(server, path, words, wordnikAuthUrl, process.env.USERNAME, process.env.PASSWORD, process.env.API_KEY, wordnikToken);