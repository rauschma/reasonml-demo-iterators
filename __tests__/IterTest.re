open Jest;
open Expect;

let iter = (~f: 'a => unit, theList: list('a)) => {
  let rec aux = (l) =>
    switch (l) {
    | [] => ()
    | [head, ...tail] =>
      f(head);
      aux(tail);
    };
  aux(theList);
};

test("iter", () => {
  let result = ref([]);
  let f = (x) => result := [x, ...result^];
  iter(~f, ["a", "b", "c"]);
  expect(result^) |> toEqual(["c", "b", "a"]);
});