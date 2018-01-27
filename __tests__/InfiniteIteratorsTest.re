open Jest;
open Expect;

let naturalNumbers = (): Gen.t(int) => {
  let n = ref(0);
  () => {
    let cur = n^;
    n := n^ + 1;
    Some(cur);
  };
};

test("Gen.take", () => {
  let take5 = Gen.take(5, naturalNumbers());
  expect(Gen.to_list(take5))
    |> toEqual([0, 1, 2, 3, 4]);
});

test("Gen.zip", () => {
  let abc = Gen.of_list(["a", "b", "c"]);
  let pairs = Gen.zip(naturalNumbers(), abc);
  expect(Gen.to_list(pairs))
    |> toEqual([(0, "a"), (1, "b"), (2, "c")]);
});