type sequence('a) = ('a => unit) => unit;

let ofList = (theList: list('a)) =>
  (f: 'a => unit) => {
    let rec aux = (l) =>
      switch (l) {
      | [] => ()
      | [head, ...tail] =>
        f(head);
        aux(tail);
      };
    aux(theList);
  };

let toList = (seq: sequence('a)) => {
  let result = ref([]);
  seq((x) => {
    result := [x, ...result^];
  });
  List.rev(result^);
};

let length = (seq: sequence('a)) => {
  let result = ref(0);
  seq((_) => { result := result^ + 1 });
  result^;
};

let map = (~f: 'a => 'b, seqIn: sequence('a)): sequence('b) => {
  let seqOut = (out: 'b => unit) => {
    seqIn((in_: 'a) => out(f(in_)));
  };
  seqOut;
};

let filter = (~f: 'a => bool, seqIn: sequence('a)): sequence('a) => {
  let seqOut = (out: 'b => unit) =>
    seqIn((in_: 'a) =>
      switch (f(in_)) {
      | false => ();
      | true => out(in_);
      }
    );
  seqOut;
};

let iter = (~f: 'a => unit, seq: sequence('a)): unit => {
  seq(f);
};
