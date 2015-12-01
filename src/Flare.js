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

exports.removeChildren = function(target) {
  return function() {
    var el = document.getElementById(target);

    // http://stackoverflow.com/a/3955238/704831
    while (el.firstChild) {
      el.removeChild(el.firstChild);
    }
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
          el.className = "flare-input-" + inputType;

          var div = document.createElement("div");
          div.className = "flare-input";

          if (id !== "") {
            el.id = id;
            var label = document.createElement("label");
            label.htmlFor = id;
            label.appendChild(document.createTextNode(id));
            div.appendChild(label);
          }

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

function clamp(min, max, initial, value) {
  if (isNaN(value)) {
    return initial;
  } else if (value < min) {
    return min;
  } else if (value > max) {
    return max;
  }
  return value;
}

exports.cNumberRange = function(type) {
  return function(min) {
    return function(max) {
      return function(step) {
        return createComponent("number-" + type,
          function(initial) {
            var input = document.createElement("input");
            input.type = type;
            input.min = min.toString();
            input.max = max.toString();
            input.step = step.toString();
            input.value = initial.toString();
            return input;
          },
          "input",
          function(t, initial) {
            return clamp(min, max, initial, parseFloat(t.value));
          }
        );
      };
    };
  };
};

exports.cIntRange = function(type) {
  return function(min) {
    return function(max) {
      return createComponent("int-" + type,
        function(initial) {
          var input = document.createElement("input");
          input.type = type;
          input.min = min.toString();
          input.max = max.toString();
          input.step = "1";
          input.value = initial.toString();
          return input;
        },
        "input",
        function(t, initial) {
          return clamp(min, max, initial, parseInt(t.value, 10));
        }
      );
    };
  };
};

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

exports.cButton = function(label) {
  return function(initial) {
    return function(send) {
      return function() {
        var div = document.createElement("div");
        div.className = "flare-input";

        var button = document.createElement("button");
        button.id = label;
        button.className = "flare-input-button";
        button.appendChild(document.createTextNode(label));

        button.addEventListener('mousedown', function() {
          send(true)();
        });
        button.addEventListener('mouseup', function() {
          send(false)();
        });

        div.appendChild(button);
        return div;

      };
    };
  };
};

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
