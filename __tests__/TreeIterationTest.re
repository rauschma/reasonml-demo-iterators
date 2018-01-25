open Jest;
open Expect;

let myStrTree = TreeIteration.(
  Node("a",
    Node("b", Empty, Empty),
    Node("c",
      Node("d", Empty, Empty),
      Empty
    )
));

test("ofTree", () => {
  expect(Gen.to_list(TreeIteration.ofTree(myStrTree))) |> toEqual(["a", "b", "c", "d"]);
});