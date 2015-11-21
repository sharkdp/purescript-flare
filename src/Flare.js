// module Flare
// jshint browser: true
// jshint node: true

"use strict";

exports.renderString = function(target) {
  return function(content) {
    return function() {
      document.getElementById(target).innerHTML = content;
    };
  };
};

exports.appendComponent = function(target) {
  return function(el) {
    return function() {
      document.getElementById(target).appendChild(el);
    };
  };
};

function createComponent(inputType, elementCallback, eventType, eventListener) {
  return function(id) {
    return function(initial) {
      return function(send) {
        return function() {
          var el = elementCallback(initial);
          el.id = id;
          el.className = "flare-input-" + inputType;

          var label = document.createElement("label");
          label.htmlFor = id;
          label.appendChild(document.createTextNode(id));

          var div = document.createElement("div");
          div.className = "flare-input";
          div.appendChild(label);
          div.appendChild(el);

          el.addEventListener(eventType, function(e) {
            var value = eventListener(e.target, initial);
            send(value)();
          });

          return div;
        };
      };
    };
  };
}

exports.cNumber = createComponent("number",
  function(initial) {
    var input = document.createElement("input");
    input.type = "number";
    input.step = "any";
    input.value = initial.toString();
    return input;
  },
  "input",
  function(t, initial) {
    var val = parseFloat(t.value);
    return (isNaN(val) ? initial : val);
  }
);

exports.cInt = createComponent("int",
  function(initial) {
    var input = document.createElement("input");
    input.type = "number";
    input.step = "1";
    input.value = initial.toString();
    return input;
  },
  "input",
  function(t, initial) {
    var val = parseInt(t.value);
    return (isNaN(val) ? initial : val);
  }
);

exports.cString = createComponent("string",
  function(initial) {
    var input = document.createElement("input");
    input.type = "text";
    input.value = initial;
    return input;
  },
  "input",
  function(t, initial) {
    return t.value;
  }
);

exports.cBoolean = createComponent("boolean",
  function(initial) {
    var input = document.createElement("input");
    input.type = "checkbox";
    input.checked = initial;
    return input;
  },
  "change",
  function(t, initial) {
    return t.checked;
  }
);

exports.cSelect = function(showX) {
  return function(xs) {
    return createComponent("select",
      function(initial) {
        var select = document.createElement("select");

        var x, op;
        for (var i = 0; i < xs.length + 1; i++) {
          x = (i === 0) ? initial : xs[i - 1];
          op = document.createElement("option");
          op.appendChild(document.createTextNode(showX.show(x)));
          select.appendChild(op);
        }

        return select;
      },
      "change",
      function(t, initial) {
        var ix = t.selectedIndex;
        if (ix === 0) {
          return initial;
        }
        return xs[ix - 1];
      }
    );
  };
};

// vim: ts=2:sw=2
