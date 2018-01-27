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
  let treeGen = TreeIteration.ofTree(myStrTree);
  expect(Gen.to_list(treeGen))
    |> toEqual(["a", "b", "c", "d"]);
});

test("ofTree, beyond last leaf", () => {
  let node = TreeIteration.Node("X", Empty, Empty);
  let gen = TreeIteration.ofTree(node);
  /* The third call reads beyond the end of the iteration */
  expect((gen(), gen(), gen()))
    |> toEqual((Some("X"), None, None));
});