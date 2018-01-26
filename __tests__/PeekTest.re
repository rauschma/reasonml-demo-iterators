open Jest;
open Expect;

test("peek", () => {
  let gen = Gen.of_list(["a", "b"]);
  let peekGen = Gen.peek(gen);
  expect(Gen.to_list(peekGen))
    |> toEqual([
      ("a", Some("b")),
      ("b", None),
    ]);
});