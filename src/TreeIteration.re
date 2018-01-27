type tree('a) =
  | Empty
  | Node('a, tree('a), tree('a));

let rec iterTree = (~f: ('a) => unit, tree: tree('a)) => {
  switch (tree) {
  | Empty => ()
  | Node(x, left, right) =>
    f(x);
    iterTree(~f, left);
    iterTree(~f, right);
  }
};

let rec iterTreeCps = (~f: ('a, unit=>unit) => unit,
tree: tree('a), cont: unit => unit) => {
  switch (tree) {
  | Empty => cont()
  | Node(x, left, right) =>
    f(x, () => {
      iterTreeCps(~f, left, () => {
        iterTreeCps(~f, right, cont);
      });
    });
  }
};

let ofTree = (tr: tree('a)): Gen.t('a) => {
  let next = ref(None);
  
  let rec visitTree = (t: tree('a), cont: unit => option('a)) => {
    switch (t) {
    | Empty => cont()
    | Node(x, left, right) =>
      next := Some(() => {
        visitTree(left, () => {
          visitTree(right, cont);
        });
      });
      Some(x);
    }
  };
  
  next := Some(() => visitTree(tr, () => None));
  
  () =>
    switch (next^) {
    | Some(func) =>
      next := None;
      func();
    /* After the last node of the tree, */
    /* we always return None */
    | None => None
    };
};









