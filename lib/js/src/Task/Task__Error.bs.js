// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Rebase = require("@glennsl/rebase/lib/js/src/Rebase.bs.js");
var $$Promise = require("reason-promise/lib/js/src/js/promise.js");
var P$GclAtom = require("../Util/P.bs.js");
var Spec$GclAtom = require("../Spec.bs.js");
var Syntax$GclAtom = require("../GCL/Syntax.bs.js");
var Decoration$GclAtom = require("../Editor/Decoration.bs.js");
var GCL__Error$GclAtom = require("../GCL/GCL__Error.bs.js");
var Caml_chrome_debugger = require("bs-platform/lib/js/caml_chrome_debugger.js");

function handle(site, param) {
  switch (param) {
    case /* MissingBound */0 :
        return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                  /* AddDecorations */Caml_chrome_debugger.variant("AddDecorations", 2, [(function (param, param$1) {
                          return Decoration$GclAtom.markSite(site, param, param$1);
                        })]),
                  /* :: */Caml_chrome_debugger.simpleVariant("::", [
                      /* Display */Caml_chrome_debugger.variant("Display", 6, [
                          /* Error */Caml_chrome_debugger.variant("Error", 1, ["Bound Missing"]),
                          /* Plain */Caml_chrome_debugger.variant("Plain", 1, ["Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\""])
                        ]),
                      /* [] */0
                    ])
                ]);
    case /* MissingAssertion */1 :
        return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                  /* AddDecorations */Caml_chrome_debugger.variant("AddDecorations", 2, [(function (param, param$1) {
                          return Decoration$GclAtom.markSite(site, param, param$1);
                        })]),
                  /* :: */Caml_chrome_debugger.simpleVariant("::", [
                      /* Display */Caml_chrome_debugger.variant("Display", 6, [
                          /* Error */Caml_chrome_debugger.variant("Error", 1, ["Assertion Missing"]),
                          /* Plain */Caml_chrome_debugger.variant("Plain", 1, ["Assertion before the DO construct is missing"])
                        ]),
                      /* [] */0
                    ])
                ]);
    case /* MissingLoopInvariant */2 :
        return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                  /* AddDecorations */Caml_chrome_debugger.variant("AddDecorations", 2, [(function (param, param$1) {
                          return Decoration$GclAtom.markSite(site, param, param$1);
                        })]),
                  /* :: */Caml_chrome_debugger.simpleVariant("::", [
                      /* Display */Caml_chrome_debugger.variant("Display", 6, [
                          /* Error */Caml_chrome_debugger.variant("Error", 1, ["Loop Invariant Missing"]),
                          /* Plain */Caml_chrome_debugger.variant("Plain", 1, ["Loop invariant before the DO construct is missing"])
                        ]),
                      /* [] */0
                    ])
                ]);
    case /* ExcessBound */3 :
        return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                  /* AddDecorations */Caml_chrome_debugger.variant("AddDecorations", 2, [(function (param, param$1) {
                          return Decoration$GclAtom.markSite(site, param, param$1);
                        })]),
                  /* :: */Caml_chrome_debugger.simpleVariant("::", [
                      /* Display */Caml_chrome_debugger.variant("Display", 6, [
                          /* Error */Caml_chrome_debugger.variant("Error", 1, ["Excess Bound"]),
                          /* Plain */Caml_chrome_debugger.variant("Plain", 1, ["Unnecessary bound annotation at this assertion"])
                        ]),
                      /* [] */0
                    ])
                ]);
    case /* MissingPrecondition */4 :
        return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                  /* Display */Caml_chrome_debugger.variant("Display", 6, [
                      /* Error */Caml_chrome_debugger.variant("Error", 1, ["Precondition Missing"]),
                      /* Plain */Caml_chrome_debugger.variant("Plain", 1, ["The first statement of the program should be an assertion"])
                    ]),
                  /* [] */0
                ]);
    case /* MissingPostcondition */5 :
        return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                  /* Display */Caml_chrome_debugger.variant("Display", 6, [
                      /* Error */Caml_chrome_debugger.variant("Error", 1, ["Postcondition Missing"]),
                      /* Plain */Caml_chrome_debugger.variant("Plain", 1, ["The last statement of the program should be an assertion"])
                    ]),
                  /* [] */0
                ]);
    case /* DigHole */6 :
        return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                  /* WithInstance */Caml_chrome_debugger.variant("WithInstance", 0, [(function (instance) {
                          return P$GclAtom.let_(Spec$GclAtom.digHole(site, instance), (function (param) {
                                        var match = instance[/* history */6];
                                        if (match !== undefined && !(typeof match === "number" || match.tag !== /* Refine */1)) {
                                          return $$Promise.resolved(/* :: */Caml_chrome_debugger.simpleVariant("::", [
                                                        /* DispatchLocal */Caml_chrome_debugger.variant("DispatchLocal", 4, [/* Save */1]),
                                                        /* :: */Caml_chrome_debugger.simpleVariant("::", [
                                                            /* DispatchLocal */Caml_chrome_debugger.variant("DispatchLocal", 4, [/* Refine */2]),
                                                            /* [] */0
                                                          ])
                                                      ]));
                                        } else {
                                          return $$Promise.resolved(/* :: */Caml_chrome_debugger.simpleVariant("::", [
                                                        /* DispatchLocal */Caml_chrome_debugger.variant("DispatchLocal", 4, [/* Save */1]),
                                                        /* [] */0
                                                      ]));
                                        }
                                      }));
                        })]),
                  /* [] */0
                ]);
    
  }
}

var StructError = {
  handle: handle
};

function handle$1(error) {
  var kind = error[1];
  var site = error[0];
  if (typeof kind === "number") {
    if (kind === /* LexicalError */0) {
      return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                /* AddDecorations */Caml_chrome_debugger.variant("AddDecorations", 2, [(function (param, param$1) {
                        return Decoration$GclAtom.markSite(site, param, param$1);
                      })]),
                /* :: */Caml_chrome_debugger.simpleVariant("::", [
                    /* Display */Caml_chrome_debugger.variant("Display", 6, [
                        /* Error */Caml_chrome_debugger.variant("Error", 1, ["Lexical Error"]),
                        /* Plain */Caml_chrome_debugger.variant("Plain", 1, [GCL__Error$GclAtom.Site.toString(site)])
                      ]),
                    /* [] */0
                  ])
              ]);
    } else {
      return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                /* AddDecorations */Caml_chrome_debugger.variant("AddDecorations", 2, [(function (param, param$1) {
                        return Decoration$GclAtom.markSite(site, param, param$1);
                      })]),
                /* :: */Caml_chrome_debugger.simpleVariant("::", [
                    /* Display */Caml_chrome_debugger.variant("Display", 6, [
                        /* Error */Caml_chrome_debugger.variant("Error", 1, ["Not Loaded"]),
                        /* Plain */Caml_chrome_debugger.variant("Plain", 1, ["Please load the file first"])
                      ]),
                    /* [] */0
                  ])
              ]);
    }
  } else {
    switch (kind.tag | 0) {
      case /* SyntacticError */0 :
          return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                    /* AddDecorations */Caml_chrome_debugger.variant("AddDecorations", 2, [(function (param, param$1) {
                            return Decoration$GclAtom.markSite(site, param, param$1);
                          })]),
                    /* :: */Caml_chrome_debugger.simpleVariant("::", [
                        /* Display */Caml_chrome_debugger.variant("Display", 6, [
                            /* Error */Caml_chrome_debugger.variant("Error", 1, ["Parse Error"]),
                            /* Plain */Caml_chrome_debugger.variant("Plain", 1, [Rebase.$$String.joinWith("\n", Rebase.List.fromArray(kind[0]))])
                          ]),
                        /* [] */0
                      ])
                  ]);
      case /* StructError */1 :
          return handle(site, kind[0]);
      case /* TypeError */2 :
          var match = kind[0];
          switch (match.tag | 0) {
            case /* NotInScope */0 :
                return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                          /* AddDecorations */Caml_chrome_debugger.variant("AddDecorations", 2, [(function (param, param$1) {
                                  return Decoration$GclAtom.markSite(site, param, param$1);
                                })]),
                          /* :: */Caml_chrome_debugger.simpleVariant("::", [
                              /* Display */Caml_chrome_debugger.variant("Display", 6, [
                                  /* Error */Caml_chrome_debugger.variant("Error", 1, ["Type Error"]),
                                  /* Plain */Caml_chrome_debugger.variant("Plain", 1, ["The definition " + (match[0] + " is not in scope")])
                                ]),
                              /* [] */0
                            ])
                        ]);
            case /* UnifyFailed */1 :
                return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                          /* AddDecorations */Caml_chrome_debugger.variant("AddDecorations", 2, [(function (param, param$1) {
                                  return Decoration$GclAtom.markSite(site, param, param$1);
                                })]),
                          /* :: */Caml_chrome_debugger.simpleVariant("::", [
                              /* Display */Caml_chrome_debugger.variant("Display", 6, [
                                  /* Error */Caml_chrome_debugger.variant("Error", 1, ["Type Error"]),
                                  /* Plain */Caml_chrome_debugger.variant("Plain", 1, ["Cannot unify: " + (Syntax$GclAtom.Type.toString(match[0]) + ("\nwith        : " + Syntax$GclAtom.Type.toString(match[1])))])
                                ]),
                              /* [] */0
                            ])
                        ]);
            case /* RecursiveType */2 :
                return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                          /* AddDecorations */Caml_chrome_debugger.variant("AddDecorations", 2, [(function (param, param$1) {
                                  return Decoration$GclAtom.markSite(site, param, param$1);
                                })]),
                          /* :: */Caml_chrome_debugger.simpleVariant("::", [
                              /* Display */Caml_chrome_debugger.variant("Display", 6, [
                                  /* Error */Caml_chrome_debugger.variant("Error", 1, ["Type Error"]),
                                  /* Plain */Caml_chrome_debugger.variant("Plain", 1, ["Recursive type variable: " + (Syntax$GclAtom.Type.toString(/* Var */Caml_chrome_debugger.variant("Var", 3, [match[0]])) + ("\nin type             : " + Syntax$GclAtom.Type.toString(match[1])))])
                                ]),
                              /* [] */0
                            ])
                        ]);
            case /* NotFunction */3 :
                return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                          /* AddDecorations */Caml_chrome_debugger.variant("AddDecorations", 2, [(function (param, param$1) {
                                  return Decoration$GclAtom.markSite(site, param, param$1);
                                })]),
                          /* :: */Caml_chrome_debugger.simpleVariant("::", [
                              /* Display */Caml_chrome_debugger.variant("Display", 6, [
                                  /* Error */Caml_chrome_debugger.variant("Error", 1, ["Type Error"]),
                                  /* Plain */Caml_chrome_debugger.variant("Plain", 1, ["The type " + (Syntax$GclAtom.Type.toString(match[0]) + " is not a function type")])
                                ]),
                              /* [] */0
                            ])
                        ]);
            
          }
      case /* CannotReadFile */3 :
          return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                    /* AddDecorations */Caml_chrome_debugger.variant("AddDecorations", 2, [(function (param, param$1) {
                            return Decoration$GclAtom.markSite(site, param, param$1);
                          })]),
                    /* :: */Caml_chrome_debugger.simpleVariant("::", [
                        /* Display */Caml_chrome_debugger.variant("Display", 6, [
                            /* Error */Caml_chrome_debugger.variant("Error", 1, ["Cannot Read File"]),
                            /* Plain */Caml_chrome_debugger.variant("Plain", 1, ["Cannot read file of path: " + kind[0]])
                          ]),
                        /* [] */0
                      ])
                  ]);
      
    }
  }
}

exports.StructError = StructError;
exports.handle = handle$1;
/* Promise Not a pure module */