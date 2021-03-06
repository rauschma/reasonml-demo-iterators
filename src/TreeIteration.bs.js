// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");

function iterTree(f, _tree) {
  while(true) {
    var tree = _tree;
    if (tree) {
      Curry._1(f, tree[0]);
      iterTree(f, tree[1]);
      _tree = tree[2];
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function iterTreeCps(f, tree, cont) {
  if (tree) {
    var right = tree[2];
    var left = tree[1];
    return Curry._2(f, tree[0], (function () {
                  return iterTreeCps(f, left, (function () {
                                return iterTreeCps(f, right, cont);
                              }));
                }));
  } else {
    return Curry._1(cont, /* () */0);
  }
}

function ofTree(tr) {
  var next = [/* None */0];
  var visitTree = function (t, cont) {
    if (t) {
      var right = t[2];
      var left = t[1];
      next[0] = /* Some */[(function () {
            return visitTree(left, (function () {
                          return visitTree(right, cont);
                        }));
          })];
      return /* Some */[t[0]];
    } else {
      return Curry._1(cont, /* () */0);
    }
  };
  next[0] = /* Some */[(function () {
        return visitTree(tr, (function () {
                      return /* None */0;
                    }));
      })];
  return (function () {
      var match = next[0];
      if (match) {
        next[0] = /* None */0;
        return Curry._1(match[0], /* () */0);
      } else {
        return /* None */0;
      }
    });
}

exports.iterTree    = iterTree;
exports.iterTreeCps = iterTreeCps;
exports.ofTree      = ofTree;
/* No side effect */
