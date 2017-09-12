'use strict';

require('ace-css/css/ace.css');
require('font-awesome/css/font-awesome.css');

// Require index.html so it gets copied to dist
require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// console.log('Loaded by webpack: process.env.PORT:', process.env.PORT);

function getBackendUrl() {
	console.log('@\n@\n@\n', process.env.HEROK_PORT);
	if (process.env.HEROK_PORT) {
		return 'https://' + process.env.HOST;
	}
	return 'http://' + process.env.HOST + ':' + process.env.PORT;
}

// .embed() can take an optional second argument. This would be an object describing the data we need to start a program, i.e. a userID or some token
var app = Elm.Main.embed(mountNode, {
  envVar1: getBackendUrl()
});