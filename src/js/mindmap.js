require('../../assets/release/go.js');

function init(dataFromElm) {
  var $ = go.GraphObject.make;
  window.myDiagram =
    $(go.Diagram, "myDiagramDiv",
      {
        // when the user drags a node, also move/copy/delete the whole subtree starting with that node
        "commandHandler.copiesTree": true,
        "commandHandler.deletesTree": true,
        "draggingTool.dragsTree": true,
        initialContentAlignment: go.Spot.Center,  // center the whole graph
        "undoManager.isEnabled": true
      });

  // a node consists of some text with a line shape underneath
  myDiagram.nodeTemplate =
    $(go.Node, "Vertical",
      { selectionObjectName: "TEXT" },
      $(go.TextBlock,
        {
          name: "TEXT",
          minSize: new go.Size(30, 15),
          editable: true
        },
        // remember not only the text string but the scale and the font in the node data
        new go.Binding("text", "text").makeTwoWay(),
        new go.Binding("scale", "scale").makeTwoWay(),
        new go.Binding("font", "font").makeTwoWay()),
      $(go.Shape, "LineH",
        {
          stretch: go.GraphObject.Horizontal,
          strokeWidth: 3, height: 3,
          // this line shape is the port -- what links connect with
          portId: "", fromSpot: go.Spot.LeftRightSides, toSpot: go.Spot.LeftRightSides
        },
        new go.Binding("stroke", "brush"),
        // make sure links come in from the proper direction and go out appropriately
        new go.Binding("fromSpot", "dir", function(d) { return spotConverter(d, true); }),
        new go.Binding("toSpot", "dir", function(d) { return spotConverter(d, false); })),
      // remember the locations of each node in the node data
      new go.Binding("location", "loc", go.Point.parse).makeTwoWay(go.Point.stringify),
      // make sure text "grows" in the desired direction
      new go.Binding("locationSpot", "dir", function(d) { return spotConverter(d, false); })
    );
  // selected nodes show a button for adding children
  myDiagram.nodeTemplate.selectionAdornmentTemplate =
    $(go.Adornment, "Spot",
      $(go.Panel, "Auto",
        // this Adornment has a rectangular blue Shape around the selected node
        $(go.Shape, { fill: null, stroke: "dodgerblue", strokeWidth: 3 }),
        $(go.Placeholder, { margin: new go.Margin(4, 4, 0, 4) })
      ),
      // and this Adornment has a Button to the right of the selected node
      $("Button",
        {
          alignment: go.Spot.Right,
          alignmentFocus: go.Spot.Left,
          click: selectNode  // define click behavior for this Button in the Adornment
        },
        $(go.TextBlock, "+",  // the Button content
          { font: "bold 8pt sans-serif" })
      )
    );
  // the context menu allows users to change the font size and weight,
  // and to perform a limited tree layout starting at that node
  myDiagram.nodeTemplate.contextMenu =
    $(go.Adornment, "Vertical",
      $("ContextMenuButton",
        $(go.TextBlock, "Layout"),
        {
          click: function(e, obj) {
              var adorn = obj.part;
              adorn.diagram.startTransaction("Subtree Layout");
              layoutTree(adorn.adornedPart);
              adorn.diagram.commitTransaction("Subtree Layout");
            }
        }
      )
    );
  // a link is just a Bezier-curved line of the same color as the node to which it is connected
  myDiagram.linkTemplate =
    $(go.Link,
      {
        curve: go.Link.Bezier,
        fromShortLength: -2,
        toShortLength: -2,
        selectable: false
      },
      $(go.Shape,
        { strokeWidth: 3 },
        new go.Binding("stroke", "toNode", function(n) {
          if (n.data.brush) return n.data.brush;
          return "black";
        }).ofObject())
    );
  // the Diagram's context menu just displays commands for general functionality
  myDiagram.contextMenu =
    $(go.Adornment, "Vertical",
      $("ContextMenuButton",
        $(go.TextBlock, "Undo"),
        { click: function(e, obj) { e.diagram.commandHandler.undo(); } },
        new go.Binding("visible", "", function(o) { return o.diagram && o.diagram.commandHandler.canUndo(); }).ofObject()),
      $("ContextMenuButton",
        $(go.TextBlock, "Redo"),
        { click: function(e, obj) { e.diagram.commandHandler.redo(); } },
        new go.Binding("visible", "", function(o) { return o.diagram && o.diagram.commandHandler.canRedo(); }).ofObject()),
      $("ContextMenuButton",
        $(go.TextBlock, "Save"),
        { click: function(e, obj) { save(); } })
      // $("ContextMenuButton",
      //   $(go.TextBlock, "Load"),
      //   { click: function(e, obj) { load(); } })
    );
  myDiagram.addDiagramListener("SelectionMoved", function(e) {
    var rootX = myDiagram.findNodeForKey(0).location.x;
    myDiagram.selection.each(function(node) {
        if (node.data.parent !== 0) return; // Only consider nodes connected to the root
        var nodeX = node.location.x;
        if (rootX < nodeX && node.data.dir !== "right") {
          updateNodeDirection(node, "right");
        } else if (rootX > nodeX && node.data.dir !== "left") {
          updateNodeDirection(node, "left");
        }
        layoutTree(node);
      });
  });

  load(dataFromElm);
}

function spotConverter(dir, from) {
  if (dir === "left") {
    return (from ? go.Spot.Left : go.Spot.Right);
  } else {
    return (from ? go.Spot.Right : go.Spot.Left);
  }
}
function updateNodeDirection(node, dir) {
  myDiagram.model.setDataProperty(node.data, "dir", dir);
  // recursively update the direction of the child nodes
  var chl = node.findTreeChildrenNodes(); // gives us an iterator of the child nodes related to this particular node
  while(chl.next()) {
    updateNodeDirection(chl.value, dir);
  }
}
function selectNode(e, obj) {
  var adorn = obj.part;
  var diagram = adorn.diagram;
  diagram.startTransaction("Select Node");
  var node = adorn.adornedPart;
  diagram.model.setDataProperty(node.data, "brush", "red");
  layoutTree(node);
  diagram.commitTransaction("Select Node");
}
function layoutTree(node) {
  if (node.data.key === 0) {  // adding to the root?
    layoutAll();  // lay out everything
  } else {  // otherwise lay out only the subtree starting at this parent node
    var parts = node.findTreeParts();
    layoutAngle(parts, node.data.dir === "left" ? 180 : 0);
  }
}
function layoutAngle(parts, angle) {
  var layout = go.GraphObject.make(go.TreeLayout,
      { angle: angle,
        arrangement: go.TreeLayout.ArrangementFixedRoots,
        nodeSpacing: 5,
        layerSpacing: 20,
        setsPortSpot: false, // don't set port spots since we're managing them with our spotConverter function
        setsChildPortSpot: false });
  layout.doLayout(parts);
}
function layoutAll() {
  var root = myDiagram.findNodeForKey(0);
  if (root === null) return;
  myDiagram.startTransaction("Layout");
  // split the nodes and links into two collections
  var rightward = new go.Set(go.Part);
  var leftward = new go.Set(go.Part);
  root.findLinksConnected().each(function(link) {
      var child = link.toNode;
      if (child.data.dir === "left") {
        leftward.add(root);  // the root node is in both collections
        leftward.add(link);
        leftward.addAll(child.findTreeParts());
      } else {
        rightward.add(root);  // the root node is in both collections
        rightward.add(link);
        rightward.addAll(child.findTreeParts());
      }
    });
  // do one layout and then the other without moving the shared root node
  layoutAngle(rightward, 0);
  layoutAngle(leftward, 180);
  myDiagram.commitTransaction("Layout");
}
// Show the diagram's model in JSON format
function save() {
  console.warn('save not implemented');
}
function load(data) {
  myDiagram.model = go.Model.fromJson(data);
}

//========================

function getBaseNode(nextKey, from, wordData) {
  return {
    key: nextKey++,
    parent: from.key,
    text: wordData.word,
    brush: from.brush,
    dir: from.dir,
    loc: "0 0"
  };
}

function getChildNodes(nextKey, direction, from, wordDatas) {
  var xy = from.loc.split(' ');
  var x = parseFloat(xy[0]);
  var y = parseFloat(xy[1]);
  var reducer;
  switch(direction) {
    case "topLeft": 
      reducer = function(nodes, wordData) {
        var node = getBaseNode(nextKey, from, wordData);
        node.loc = (x - 30)  + " " + ((nodes.length === 0) ? (y - 30) : (parseFloat(nodes[nodes.length - 1].loc.split(' ')[1]) - 30));
        nodes.push(node);
        return nodes;
      }; break;

    case "topRight": 
      reducer = function(nodes, wordData) {
        var node = getBaseNode(nextKey, from, wordData);
        node.loc = (x + 30)  + " " + ((nodes.length === 0) ? (y - 30) : (parseFloat(nodes[nodes.length - 1].loc.split(' ')[1]) - 30));
        nodes.push(node);
        return nodes;
      }; break;

    case "bottomLeft":
      reducer = function(nodes, wordData) {
        var node = getBaseNode(nextKey, from, wordData);
        node.loc = (x - 30)  + " " + ((nodes.length === 0) ? (y + 30) : (parseFloat(nodes[nodes.length - 1].loc.split(' ')[1]) + 30));
        nodes.push(node);
        return nodes;
      }; break;

    case "bottomRight":
      reducer = function(nodes, wordData) {
        var node = getBaseNode(nextKey, from, wordData);
        node.loc = (x + 30)  + " " + ((nodes.length === 0) ? (y + 30) : (parseFloat(nodes[nodes.length - 1].loc.split(' ')[1]) + 30));
        nodes.push(node);
        return nodes;
      }; break;
  }
  return wordDatas.reduce(reducer, []);
}

export { init, load, getChildNodes };