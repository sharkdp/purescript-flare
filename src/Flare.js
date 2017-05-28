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

// This function maintains a global state `window.flareID` to generate unique
// DOM element IDs. It is only called from functions with a DOM effect.
function getUniqueID() {
  if (window.flareID === undefined) {
    window.flareID = 0;
  }
  window.flareID = window.flareID + 1;
  return "flare-component-" + window.flareID.toString();
}

function createComponent(inputType, elementCallback, eventType, eventListener) {
  return function(label) {
    return function(initial) {
      return function(send) {
        return function() {
          var uid = getUniqueID();
          var el = elementCallback(initial);
          el.className = "flare-input-" + inputType;
          el.id = uid;

          var div = document.createElement("div");
          div.className = "flare-input";

          if (label !== "") {
            var labelEl = document.createElement("label");
            labelEl.htmlFor = uid;
            labelEl.appendChild(document.createTextNode(label));
            div.appendChild(labelEl);
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

exports.cStringPattern = function(pattern) {
  return createComponent("string-pattern",
    function(initial) {
      var input = document.createElement("input");
      input.type = "text";
      input.pattern = pattern;
      input.required = true;
      input.value = initial;
      return input;
    },
    "input",
    function(t, initial) {
      return t.value;
    }
  );
};

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

exports.cButton = function(vPressed) {
  return function(label) {
    return function(vDefault) {
      return function(send) {
        return function() {
          var div = document.createElement("div");
          div.className = "flare-input";

          var button = document.createElement("button");
          button.id = getUniqueID();
          button.className = "flare-input-button";
          button.appendChild(document.createTextNode(label));

          button.addEventListener('mousedown', function() {
            send(vPressed)();
          });
          button.addEventListener('mouseup', function() {
            send(vDefault)();
          });

          div.appendChild(button);
          return div;

        };
      };
    };
  };
};

exports.cSelect = function(xs) {
  return function(toString) {
    return createComponent("select",
      function(initial) {
        var select = document.createElement("select");

        var x, op;
        for (var i = 0; i < xs.length + 1; i++) {
          x = (i === 0) ? initial : xs[i - 1];
          op = document.createElement("option");
          op.appendChild(document.createTextNode(toString(x)));
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

exports.cRadioGroup = function(xs) {
  return function(toString) {
    return function(label) {
      var uid = getUniqueID();
      return createComponent("radioGroup",
        function(initial) {
          var fieldset = document.createElement("fieldset");

          if (label !== "") {
            var legend = document.createElement("legend");
            legend.appendChild(document.createTextNode(label));
            fieldset.appendChild(legend);
          }

          var x, xid, op, labelEl;
          for (var i = 0; i < xs.length + 1; i++) {
            x = (i === 0) ? initial : xs[i - 1];
            xid = uid + "-" + i.toString();

            op = document.createElement("input");
            op.type = "radio";
            op.name = uid;
            op.id = xid;
            if (i === 0) {
              op.checked = "checked";
            }
            fieldset.appendChild(op);

            labelEl = document.createElement("label");
            labelEl.appendChild(document.createTextNode(toString(x)));
            labelEl.htmlFor = xid;
            fieldset.appendChild(labelEl);
          }

          return fieldset;
        },
        "change",
        function(t, initial) {
          var ix = parseInt(t.id.substr(uid.length + 1), 10);
          if (ix === 0) {
            return initial;
          }
          return xs[ix - 1];
        }
      )("");
    };
  };
};

exports.cTextarea = createComponent("textarea",
  function(initial) {
    var textarea = document.createElement("textarea");
    textarea.value = initial;
    return textarea;
  },
  "input",
  function(t, initial) {
    return t.value;
  }
);


exports.toFieldset = function(label) {
  return function(elements) {
    var fieldset = document.createElement("fieldset");

    if (label !== "") {
      var legend = document.createElement("legend");
      legend.appendChild(document.createTextNode(label));
      fieldset.appendChild(legend);
    }

    for (var i = 0; i < elements.length; i++) {
      fieldset.appendChild(elements[i]);
    }

    return fieldset;
  };
};

exports.cColor = createComponent("color",
  function(initial) {
    var input = document.createElement("input");
    input.type = "color";
    input.value = initial;
    return input;
  },
  "input",
  function(t, initial) {
    return t.value;
  }
);

function padNumber(num) {
  var str = num.toString();
  if (str.length == 1) {
    str = "0" + str;
  }
  return str;
}

exports.cDate = createComponent("date",
  function(initial) {
    var input = document.createElement("input");
    input.type = "date";
    input.value = initial.year.toString() + "-" +
                  padNumber(initial.month) + "-" +
                  padNumber(initial.day);
    return input;
  },
  "input",
  function(t, initial) {
    var parts = t.value.split("-");
    return { year: parseInt(parts[0], 10),
             month: parseInt(parts[1], 10),
             day: parseInt(parts[2])
           };
  }
);

exports.cTime = createComponent("time",
  function(initial) {
    var input = document.createElement("input");
    input.type = "time";
    input.value = padNumber(initial.hours.toString()) + ":" +
                  padNumber(initial.minutes.toString());
    return input;
  },
  "input",
  function(t, initial) {
    var parts = t.value.split(":");
    return { hours: parseInt(parts[0], 10),
             minutes: parseInt(parts[1], 10)
           };
  }
);

// vim: ts=2:sw=2
