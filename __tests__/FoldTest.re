open Jest;
open Expect;

let len = (gen: Gen.t('a)) => {
  let foldFunc = (state: int, _x: 'a) =>
    state + 1;
  Gen.fold(foldFunc, 0, gen);
};

test("len 0", () => {
  let gen = Gen.of_list([]);
  expect(len(gen)) |> toBe(0);
});

test("len 2", () => {
  let gen = Gen.of_list(["a", "b"]);
  expect(len(gen)) |> toBe(2);
});