// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Atom = require("atom");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Json_decode = require("@glennsl/bs-json/lib/js/src/Json_decode.bs.js");
var Caml_chrome_debugger = require("bs-platform/lib/js/caml_chrome_debugger.js");

function sum(decoder) {
  return (function (param) {
      return Json_decode.andThen((function (tag) {
                    var match = Curry._1(decoder, tag);
                    if (match.tag) {
                      return match[0];
                    } else {
                      var d = match[0];
                      return (function (param) {
                          return Json_decode.field("contents", d, param);
                        });
                    }
                  }), (function (param) {
                    return Json_decode.field("tag", Json_decode.string, param);
                  }), param);
    });
}

function point(json) {
  return new Atom.Point(Json_decode.field("line", Json_decode.$$int, json) - 1 | 0, Json_decode.field("column", Json_decode.$$int, json) - 1 | 0);
}

var range = sum((function (tag) {
        switch (tag) {
          case "Loc" :
              return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (json) {
                            var x = Json_decode.field("start", point, json);
                            var y = Json_decode.field("end", point, json);
                            return new Atom.Range(new Atom.Point(x.row, x.column), new Atom.Point(y.row, y.column + 1 | 0));
                          })]);
          case "NoLoc" :
              return /* TagOnly */Caml_chrome_debugger.variant("TagOnly", 1, [(function (param) {
                            return new Atom.Range(new Atom.Point(0, 0), new Atom.Point(0, 0));
                          })]);
          default:
            throw [
                  Json_decode.DecodeError,
                  "Unknown constructor: " + tag
                ];
        }
      }));

function maybe(decoder) {
  return sum((function (param) {
                if (param === "Just") {
                  return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (json) {
                                return Caml_option.some(Curry._1(decoder, json));
                              })]);
                } else {
                  return /* TagOnly */Caml_chrome_debugger.variant("TagOnly", 1, [(function (param) {
                                return ;
                              })]);
                }
              }));
}

exports.sum = sum;
exports.point = point;
exports.range = range;
exports.maybe = maybe;
/* range Not a pure module */