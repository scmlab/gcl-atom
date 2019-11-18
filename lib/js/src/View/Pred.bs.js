// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Rebase = require("@glennsl/rebase/lib/js/src/Rebase.bs.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Json_decode = require("@glennsl/bs-json/lib/js/src/Json_decode.bs.js");
var Decoder$GclAtom = require("../Decoder.bs.js");
var Caml_chrome_debugger = require("bs-platform/lib/js/caml_chrome_debugger.js");

function parensIf(p, s) {
  if (p) {
    return "(" + (s + ")");
  } else {
    return s;
  }
}

function decode(json) {
  return Decoder$GclAtom.fields((function (tag) {
                  switch (tag) {
                    case "Bol" :
                        return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (param) {
                                      return Json_decode.map((function (x) {
                                                    return /* Bool */Caml_chrome_debugger.variant("Bool", 1, [x]);
                                                  }), Json_decode.bool, param);
                                    })]);
                    case "Num" :
                        return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (param) {
                                      return Json_decode.map((function (x) {
                                                    return /* Num */Caml_chrome_debugger.variant("Num", 0, [x]);
                                                  }), Json_decode.$$int, param);
                                    })]);
                    default:
                      throw [
                            Json_decode.DecodeError,
                            "Unknown constructor: " + tag
                          ];
                  }
                }))(json);
}

function toString(param) {
  if (param.tag) {
    return Pervasives.string_of_bool(param[0]);
  } else {
    return String(param[0]);
  }
}

var Lit = {
  decode: decode,
  toString: toString
};

function decode$1(json) {
  return Decoder$GclAtom.fields((function (tag) {
                  switch (tag) {
                    case "ConstE" :
                        return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (param) {
                                      return Json_decode.map((function (x) {
                                                    return /* Const */Caml_chrome_debugger.variant("Const", 1, [x]);
                                                  }), Json_decode.string, param);
                                    })]);
                    case "HoleE" :
                        return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (param) {
                                      return Json_decode.map((function (param) {
                                                    return /* Hole */Caml_chrome_debugger.variant("Hole", 4, [
                                                              param[0],
                                                              param[1]
                                                            ]);
                                                  }), (function (param) {
                                                    return Json_decode.pair(Json_decode.$$int, (function (param) {
                                                                  return Json_decode.array(decodeSubst, param);
                                                                }), param);
                                                  }), param);
                                    })]);
                    case "LitE" :
                        return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (param) {
                                      return Json_decode.map((function (x) {
                                                    return /* Lit */Caml_chrome_debugger.variant("Lit", 2, [x]);
                                                  }), decode, param);
                                    })]);
                    case "OpE" :
                        return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (param) {
                                      return Json_decode.map((function (param) {
                                                    return /* Op */Caml_chrome_debugger.variant("Op", 3, [
                                                              param[0],
                                                              param[1]
                                                            ]);
                                                  }), (function (param) {
                                                    return Json_decode.pair(decode$1, (function (param) {
                                                                  return Json_decode.array(decode$1, param);
                                                                }), param);
                                                  }), param);
                                    })]);
                    case "VarE" :
                        return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (param) {
                                      return Json_decode.map((function (x) {
                                                    return /* Var */Caml_chrome_debugger.variant("Var", 0, [x]);
                                                  }), Json_decode.string, param);
                                    })]);
                    default:
                      throw [
                            Json_decode.DecodeError,
                            "Unknown constructor: " + tag
                          ];
                  }
                }))(json);
}

function decodeSubst(json) {
  return Json_decode.dict(decode$1, json);
}

function intercalate(toString, sep, xs) {
  var intercalate$prime = function (xs) {
    if (xs) {
      var xs$1 = xs[1];
      var x = xs[0];
      if (xs$1) {
        return Curry._1(toString, x) + (sep + intercalate$prime(xs$1));
      } else {
        return Curry._1(toString, x);
      }
    } else {
      return "";
    }
  };
  return intercalate$prime(Rebase.List.fromArray(xs));
}

function toString$1(param) {
  switch (param.tag | 0) {
    case /* Var */0 :
    case /* Const */1 :
        return param[0];
    case /* Lit */2 :
        return toString(param[0]);
    case /* Op */3 :
        return toString$1(param[0]) + ("(" + (intercalate(toString$1, ", ", param[1]) + ")"));
    case /* Hole */4 :
        return "[" + (String(param[0]) + "]");
    
  }
}

var Expr = {
  decode: decode$1,
  decodeSubst: decodeSubst,
  intercalate: intercalate,
  toString: toString$1
};

function decode$2(param) {
  return Json_decode.map((function (tag) {
                switch (tag) {
                  case "Eq" :
                      return /* Eq */0;
                  case "GEq" :
                      return /* GEq */2;
                  case "GTh" :
                      return /* GTh */4;
                  case "LEq" :
                      return /* LEq */1;
                  case "LTh" :
                      return /* LTh */3;
                  default:
                    throw [
                          Json_decode.DecodeError,
                          "Unknown constructor: " + tag
                        ];
                }
              }), Json_decode.string, param);
}

function toString$2(param) {
  switch (param) {
    case /* Eq */0 :
        return "=";
    case /* LEq */1 :
        return "<=";
    case /* GEq */2 :
        return ">=";
    case /* LTh */3 :
        return "<";
    case /* GTh */4 :
        return ">";
    
  }
}

var BinRel = {
  decode: decode$2,
  toString: toString$2
};

function decode$3(json) {
  return Decoder$GclAtom.fields((function (tag) {
                  switch (tag) {
                    case "Conj" :
                        return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (param) {
                                      return Json_decode.map((function (param) {
                                                    return /* Conj */Caml_chrome_debugger.variant("Conj", 2, [
                                                              param[0],
                                                              param[1]
                                                            ]);
                                                  }), (function (param) {
                                                    return Json_decode.pair(decode$3, decode$3, param);
                                                  }), param);
                                    })]);
                    case "Disj" :
                        return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (param) {
                                      return Json_decode.map((function (param) {
                                                    return /* Disj */Caml_chrome_debugger.variant("Disj", 1, [
                                                              param[0],
                                                              param[1]
                                                            ]);
                                                  }), (function (param) {
                                                    return Json_decode.pair(decode$3, decode$3, param);
                                                  }), param);
                                    })]);
                    case "Hole" :
                        return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (param) {
                                      return Json_decode.map((function (p) {
                                                    return /* Hole */Caml_chrome_debugger.variant("Hole", 6, [p]);
                                                  }), Json_decode.$$int, param);
                                    })]);
                    case "Implies" :
                        return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (param) {
                                      return Json_decode.map((function (param) {
                                                    return /* Implies */Caml_chrome_debugger.variant("Implies", 0, [
                                                              param[0],
                                                              param[1]
                                                            ]);
                                                  }), (function (param) {
                                                    return Json_decode.pair(decode$3, decode$3, param);
                                                  }), param);
                                    })]);
                    case "Lit" :
                        return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (param) {
                                      return Json_decode.map((function (p) {
                                                    return /* Lit */Caml_chrome_debugger.variant("Lit", 5, [p]);
                                                  }), Json_decode.bool, param);
                                    })]);
                    case "Neg" :
                        return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (param) {
                                      return Json_decode.map((function (p) {
                                                    return /* Neg */Caml_chrome_debugger.variant("Neg", 4, [p]);
                                                  }), decode$3, param);
                                    })]);
                    case "Term" :
                        return /* Contents */Caml_chrome_debugger.variant("Contents", 0, [(function (param) {
                                      return Json_decode.map((function (param) {
                                                    return /* Term */Caml_chrome_debugger.variant("Term", 3, [
                                                              param[0],
                                                              param[1],
                                                              param[2]
                                                            ]);
                                                  }), (function (param) {
                                                    return Json_decode.tuple3(decode$2, decode$1, decode$1, param);
                                                  }), param);
                                    })]);
                    default:
                      throw [
                            Json_decode.DecodeError,
                            "Unknown constructor: " + tag
                          ];
                  }
                }))(json);
}

function toStringPrec(n, param) {
  switch (param.tag | 0) {
    case /* Implies */0 :
        return parensIf(n > 0, toStringPrec(1, param[0]) + (" → " + toStringPrec(0, param[1])));
    case /* Disj */1 :
        return parensIf(n > 1, toStringPrec(1, param[0]) + (" ⋁ " + toStringPrec(2, param[1])));
    case /* Conj */2 :
        return parensIf(n > 2, toStringPrec(2, param[0]) + (" ⋀ " + toStringPrec(3, param[1])));
    case /* Term */3 :
        return parensIf(n > 3, toString$1(param[1]) + (" " + (toString$2(param[0]) + (" " + toString$1(param[2])))));
    case /* Neg */4 :
        return parensIf(n > 4, "¬ " + toStringPrec(4, param[0]));
    case /* Lit */5 :
        if (param[0]) {
          return "true";
        } else {
          return "false";
        }
    case /* Hole */6 :
        return "?" + String(param[0]);
    
  }
}

function toString$3(param) {
  return toStringPrec(0, param);
}

exports.parensIf = parensIf;
exports.Lit = Lit;
exports.Expr = Expr;
exports.BinRel = BinRel;
exports.decode = decode$3;
exports.toStringPrec = toStringPrec;
exports.toString = toString$3;
/* No side effect */