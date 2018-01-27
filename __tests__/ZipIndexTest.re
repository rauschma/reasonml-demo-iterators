open Jest;
open Expect;

test("Gen.zip_index", () => {
  let gen = Gen.of_list(["a", "b", "c"]);
  let zippedGen = Gen.zip_index(gen);
  expect(Gen.to_list(zippedGen))
    |> toEqual([
      (0, "a"),
      (1, "b"),
      (2, "c"),
    ]);
});