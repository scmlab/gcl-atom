// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Atom = require("atom");
var Rebase = require("@glennsl/rebase/lib/js/src/Rebase.bs.js");
var $$Promise = require("reason-promise/lib/js/src/js/promise.js");
var ErrorSite$GclAtom = require("../Editor/ErrorSite.bs.js");
var Caml_chrome_debugger = require("bs-platform/lib/js/caml_chrome_debugger.js");

function fromCursorPosition(instance) {
  var cursor = instance[/* editor */0].getCursorBufferPosition();
  var smallestHole = /* record */Caml_chrome_debugger.record(["contents"], [undefined]);
  Rebase.$$Array.forEach((function (spec) {
          var match = smallestHole[0];
          if (match !== undefined && !match[/* range */3].containsRange(spec[/* range */3])) {
            return 0;
          } else {
            smallestHole[0] = spec;
            return /* () */0;
          }
        }), Rebase.$$Array.filter((function (spec) {
              return spec[/* range */3].containsPoint(cursor);
            }), instance[/* specifications */5]));
  return smallestHole[0];
}

function getPayloadRange(spec, instance) {
  var start = new Atom.Point(1, 0).translate(spec[/* range */3].start);
  var end_ = instance[/* editor */0].getBuffer().rangeForRow(spec[/* range */3].end.row - 1 | 0, true).end;
  return new Atom.Range(start, end_);
}

function getPayload(spec, instance) {
  var innerRange = getPayloadRange(spec, instance);
  return instance[/* editor */0].getBuffer().getTextInRange(innerRange);
}

function resolve(i, instance) {
  var specs = Rebase.$$Array.filter((function (spec) {
          return spec[/* id */0] === i;
        }), instance[/* specifications */5]);
  Rebase.$$Option.forEach((function (spec) {
          var payload = getPayload(spec, instance);
          var start = spec[/* range */3].start;
          instance[/* editor */0].getBuffer().delete(spec[/* range */3]);
          instance[/* editor */0].getBuffer().insert(start, payload.trim());
          return /* () */0;
        }), Rebase.$$Array.get(specs, 0));
  return $$Promise.resolved(/* () */0);
}

function digHole(site, instance) {
  var range = ErrorSite$GclAtom.toRange(site, instance[/* specifications */5]);
  var start = range.start;
  var indent = " ".repeat(start.column);
  var holeText = "{!\n" + (indent + ("\n" + (indent + "!}")));
  var holeRange = new Atom.Range(start, new Atom.Point(0, 1).translate(start));
  instance[/* editor */0].setTextInBufferRange(holeRange, holeText);
  var cursorPos = new Atom.Point(1, 0).translate(start);
  instance[/* editor */0].setCursorBufferPosition(cursorPos);
  return $$Promise.resolved(/* () */0);
}

exports.fromCursorPosition = fromCursorPosition;
exports.getPayloadRange = getPayloadRange;
exports.getPayload = getPayload;
exports.resolve = resolve;
exports.digHole = digHole;
/* atom Not a pure module */