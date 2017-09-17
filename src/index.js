'use strict';

require('ace-css/css/ace.css');
require('font-awesome/css/font-awesome.css');

// Require index.html so it gets copied to dist
require('./index.html');

var init = require('./js/mindmap.js').init;
var load = require('./js/mindmap.js').load;
var getChildNodes = require('./js/mindmap.js').getChildNodes;

//================
// Setup - start
//================

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

function getBackendUrl() {
	if (process.env.HEROK_PORT) {
		return 'https://' + process.env.HOST;
	}
	return 'http://' + process.env.HOST + ':' + process.env.PORT;
}

console.warn(Object.keys(Elm.Main));

var app = Elm.Main.embed(mountNode, {
  	envVar1: getBackendUrl()
});

//================
// Setup - end
//================

//================
// Mindmap - start
//================

function createMindmapDiv() {
	var node = document.createElement("div");
	node.setAttribute("id", "myDiagramDiv");
	node.setAttribute("style", "border: solid 1px black; width:100%; height:550px;");
	return node;
}

function getSampleData() {
	return JSON.stringify(require('../assets/sample.json'));
}

function getSampleModel() {
	return { 
		query : "rail"
	    , wordfind : [{word: "railing"}, {word: "derailment"}]
	    , cambridge : [{word: "railing"}, {word: "derailment"}]
	    , synonyms : [{word: "railing"}, {word: "derailment"}]
	    , antonyms :[{word: "railing"}, {word: "derailment"}]
    };
}

app.ports.removeMindmap.subscribe(function(msg) {
	var mindmapNode = document.getElementById("myDiagramDiv");
	if(mindmapNode) {
		document.getElementById('other').removeChild(mindmapNode);
	}
});

app.ports.dataForMindmap.subscribe(function(model) {
	console.warn("got data from elm", model);
	var nextKey = 5;
	var mindmapModel = {
		class: "go.TreeModel",
		nodeDataArray: [
			{key: 0, text: model.query, loc: "0 0"},
			{key:1, parent:0, text:"contain", brush:"skyblue", dir:"right", loc:"77 -22"},
			{key:2, parent:0, text:"family", brush:"darkseagreen", dir:"right", loc:"77 43"},
			{key:3, parent:0, text:"synonym", brush:"palevioletred", dir:"left", loc:"-20 -31"},
			{key:4, parent:0, text:"antonym", brush:"coral", dir:"left", loc:"-20 52"}
		]
	};

	mindmapModel.nodeDataArray = mindmapModel.nodeDataArray.concat(getChildNodes(nextKey, "topRight", mindmapModel.nodeDataArray[1], model.wordfind));
	mindmapModel.nodeDataArray = mindmapModel.nodeDataArray.concat(getChildNodes(nextKey, "bottomRight", mindmapModel.nodeDataArray[2], model.cambridge));
	mindmapModel.nodeDataArray = mindmapModel.nodeDataArray.concat(getChildNodes(nextKey, "topLeft", mindmapModel.nodeDataArray[3], model.synonyms));
	mindmapModel.nodeDataArray = mindmapModel.nodeDataArray.concat(getChildNodes(nextKey, "bottomLeft", mindmapModel.nodeDataArray[4], model.antonyms));

	var mindmapNode = document.getElementById("myDiagramDiv");
	if (mindmapNode) {
		console.warn('reloading mindmap');
		load(mindmapModel);
	}
	else {
		document.getElementById('other').appendChild(createMindmapDiv());
		init(mindmapModel);
	}
});

//================
// Mindmap - end
//================