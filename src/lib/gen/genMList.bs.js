// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var Block                   = require("bs-platform/lib/js/block.js");
var Curry                   = require("bs-platform/lib/js/curry.js");
var Caml_array              = require("bs-platform/lib/js/caml_array.js");
var Pervasives              = require("bs-platform/lib/js/pervasives.js");
var CamlinternalOO          = require("bs-platform/lib/js/camlinternalOO.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function _make(max_chunk_size, gen) {
  return /* record */[
          /* start */[/* Suspend */Block.__(2, [gen])],
          /* chunk_size */8,
          /* max_chunk_size */max_chunk_size
        ];
}

function _incr_chunk_size(mlist) {
  if (mlist[/* chunk_size */1] < mlist[/* max_chunk_size */2]) {
    mlist[/* chunk_size */1] = (mlist[/* chunk_size */1] << 1);
    return /* () */0;
  } else {
    return 0;
  }
}

function _read_chunk(mlist, gen) {
  var match = Curry._1(gen, /* () */0);
  if (match) {
    var x = match[0];
    if (mlist[/* max_chunk_size */2] === 1) {
      var tail = [/* Suspend */Block.__(2, [gen])];
      return /* Cons1 */Block.__(1, [
                x,
                tail
              ]);
    } else {
      var r = [1];
      var a = Caml_array.caml_make_vect(mlist[/* chunk_size */1], x);
      var tail$1 = [/* Suspend */Block.__(2, [gen])];
      var stop = /* false */0;
      var node = /* Cons */Block.__(0, [
          a,
          r,
          tail$1
        ]);
      while(!stop && r[0] < mlist[/* chunk_size */1]) {
        var match$1 = Curry._1(gen, /* () */0);
        if (match$1) {
          Caml_array.caml_array_set(a, r[0], match$1[0]);
          r[0] = r[0] + 1 | 0;
        } else {
          tail$1[0] = /* Nil */0;
          stop = /* true */1;
        }
      };
      _incr_chunk_size(mlist);
      return node;
    }
  } else {
    return /* Nil */0;
  }
}

function of_gen(gen) {
  var mlist = _make(4096, gen);
  var _fill = function (_prev) {
    while(true) {
      var prev = _prev;
      var node = _read_chunk(mlist, gen);
      if (typeof node === "number") {
        prev[0] = /* Nil */0;
        return /* () */0;
      } else {
        switch (node.tag | 0) {
          case 0 : 
              prev[0] = node;
              _prev = node[2];
              continue ;
              case 1 : 
              prev[0] = node;
              _prev = node[1];
              continue ;
              case 2 : 
              throw [
                    Caml_builtin_exceptions.assert_failure,
                    [
                      "genMList.ml",
                      75,
                      19
                    ]
                  ];
          
        }
      }
    };
  };
  _fill(mlist[/* start */0]);
  return mlist;
}

function of_gen_lazy($staropt$star, $staropt$star$1, gen) {
  var max_chunk_size = $staropt$star ? $staropt$star[0] : 2048;
  var caching = $staropt$star$1 ? $staropt$star$1[0] : /* true */1;
  if (caching) {
    var max_chunk_size$1 = Pervasives.max(max_chunk_size, 2);
    return _make(max_chunk_size$1, gen);
  } else {
    var gen$1 = gen;
    return /* record */[
            /* start */[/* Suspend */Block.__(2, [gen$1])],
            /* chunk_size */1,
            /* max_chunk_size */1
          ];
  }
}

function to_gen(l) {
  var cur = [l[/* start */0]];
  var i = [0];
  var next = function (_param) {
    while(true) {
      var match = cur[0][0];
      if (typeof match === "number") {
        return /* None */0;
      } else {
        switch (match.tag | 0) {
          case 0 : 
              if (i[0] === match[1][0]) {
                cur[0] = match[2];
                i[0] = 0;
                _param = /* () */0;
                continue ;
                
              } else {
                var y = Caml_array.caml_array_get(match[0], i[0]);
                i[0] = i[0] + 1 | 0;
                return /* Some */[y];
              }
              break;
          case 1 : 
              cur[0] = match[1];
              return /* Some */[match[0]];
          case 2 : 
              var node = _read_chunk(l, match[0]);
              cur[0][0] = node;
              _param = /* () */0;
              continue ;
              
        }
      }
    };
  };
  return next;
}

var class_tables = [
  0,
  0,
  0
];

function to_clonable(l) {
  var make = function (node, i) {
    var cur = [node];
    var i$1 = [i];
    var next = function (_param) {
      while(true) {
        var match = cur[0][0];
        if (typeof match === "number") {
          return /* None */0;
        } else {
          switch (match.tag | 0) {
            case 0 : 
                if (i$1[0] === match[1][0]) {
                  cur[0] = match[2];
                  i$1[0] = 0;
                  _param = /* () */0;
                  continue ;
                  
                } else {
                  var y = Caml_array.caml_array_get(match[0], i$1[0]);
                  i$1[0] = i$1[0] + 1 | 0;
                  return /* Some */[y];
                }
                break;
            case 1 : 
                cur[0] = match[1];
                return /* Some */[match[0]];
            case 2 : 
                var node = _read_chunk(l, match[0]);
                cur[0][0] = node;
                _param = /* () */0;
                continue ;
                
          }
        }
      };
    };
    if (!class_tables[0]) {
      var $$class = CamlinternalOO.create_table([
            "clone",
            "gen"
          ]);
      var env = CamlinternalOO.new_variable($$class, "");
      var ids = CamlinternalOO.get_method_labels($$class, [
            "gen",
            "clone"
          ]);
      var gen = ids[0];
      var clone = ids[1];
      CamlinternalOO.set_methods($$class, /* array */[
            gen,
            (function (self$1) {
                return self$1[env][3];
              }),
            clone,
            (function (self$1) {
                var env$1 = self$1[env];
                return Curry._2(env$1[0], env$1[1][0], env$1[2][0]);
              })
          ]);
      var env_init = function (env$1) {
        var self = CamlinternalOO.create_object_opt(0, $$class);
        self[env] = env$1;
        return self;
      };
      CamlinternalOO.init_class($$class);
      class_tables[0] = env_init;
    }
    return Curry._1(class_tables[0], [
                make,
                cur,
                i$1,
                next
              ]);
  };
  return make(l[/* start */0], 0);
}

exports.of_gen      = of_gen;
exports.of_gen_lazy = of_gen_lazy;
exports.to_gen      = to_gen;
exports.to_clonable = to_clonable;
/* No side effect */
