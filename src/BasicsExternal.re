type gen('a) = unit => option('a);

let ofList = (l: list('a)): gen('a) => {
  let current = ref(l);
  () => {
    switch (current^) {
    | [head, ...tail] =>
      current := tail;
      Some(head)
    | [] => None
    }
  };
};

let toList = (g: gen('a)): list('a) => {
  let rec toListRec = (acc) =>
    switch (g()) {
    | Some(x) => toListRec([x, ...acc])
    | None => acc
    };
  List.rev(toListRec([]));
};

let length = (g: gen('a)): int => {
  let rec aux = (len) =>
    switch (g()) {
      | Some(_) => aux(len+1)
      | None => len
      };
  aux(0);
};

let map = (~f: 'a => 'b, in_: gen('a)): gen('b) => {
  let out = () =>
    switch (in_()) {
    | Some(x) => Some(f(x))
    | None => None
    };
  out;
};

let filter = (~f: 'a => bool, in_: gen('a)): gen('a) => {
  let rec out = () =>
    switch (in_()) {
    | Some(x) =>
      if (f(x)) Some(x) else out();
    | None => None
    };
  out;
};

let iter = (~f: 'a => unit, g: gen('a)): unit => {
  let rec aux = () =>
    switch (g()) {
      | Some(x) =>
        f(x);
        aux(); /* continue */
      | None =>
        (); /* done */
      };
  aux();
};