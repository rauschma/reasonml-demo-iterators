// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var Jest                                = require("@glennsl/bs-jest/src/jest.js");
var Gen$ReasonmlDemoIterators           = require("../src/lib/gen/gen.bs.js");
var TreeIteration$ReasonmlDemoIterators = require("../src/TreeIteration.bs.js");

var myStrTree = /* Node */[
  "a",
  /* Node */[
    "b",
    /* Empty */0,
    /* Empty */0
  ],
  /* Node */[
    "c",
    /* Node */[
      "d",
      /* Empty */0,
      /* Empty */0
    ],
    /* Empty */0
  ]
];

Jest.test("ofTree", (function () {
        return Jest.Expect[/* toEqual */12](/* :: */[
                    "a",
                    /* :: */[
                      "b",
                      /* :: */[
                        "c",
                        /* :: */[
                          "d",
                          /* [] */0
                        ]
                      ]
                    ]
                  ], Jest.Expect[/* expect */0](Gen$ReasonmlDemoIterators.to_list(TreeIteration$ReasonmlDemoIterators.ofTree(myStrTree))));
      }));

exports.myStrTree = myStrTree;
/*  Not a pure module */
