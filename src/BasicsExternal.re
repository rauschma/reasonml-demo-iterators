type gen('a) = unit => option('a);

let ofList = (l: list('a)): gen('a) => {
  let current = ref(l);
  () => {
    switch (current^) {
    | [] => None
    | [head, ...tail] =>
      current := tail;
      Some(head)
    }
  };
};

let toList = (g: gen('a)): list('a) => {
  let rec toListRec = (acc) =>
    switch (g()) {
    | None => acc
    | Some(x) => toListRec([x, ...acc])
    };
  List.rev(toListRec([]));
};

let length = (g: gen('a)): int => {
  let rec aux = (len) =>
    switch (g()) {
    | None => len
    | Some(_) => aux(len+1)
    };
  aux(0);
};

let map = (~f: 'a => 'b, in_: gen('a)): gen('b) => {
  let out = () =>
    switch (in_()) {
    | None => None
    | Some(x) => Some(f(x))
    };
  out;
};

let filter = (~f: 'a => bool, in_: gen('a)): gen('a) => {
  let rec out = () =>
    switch (in_()) {
    | None => None
    | Some(x) when f(x) => Some(x)
    | _ => out()
    };
  out;
};

let iter = (~f: 'a => unit, g: gen('a)): unit => {
  let rec aux = () =>
    switch (g()) {
    | None =>
      (); /* done */
    | Some(x) =>
      f(x);
      aux(); /* continue */
    };
  aux();
};