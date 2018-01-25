open Jest;
open Expect;

test("ofList, toList", () => {
  let seq = BasicsInternal.ofList(["a", "b", "c"]);
  expect(BasicsInternal.toList(seq)) |> toEqual(["a", "b", "c"]);
});

test("length", () => {
  let seq = BasicsInternal.ofList(["a", "b", "c"]);
  expect(BasicsInternal.length(seq)) |> toBe(3);
});

test("map", () => {
  let seqIn = BasicsInternal.ofList([1, 2, 3]);
  let seqOut = BasicsInternal.map(~f = x=>2*x, seqIn);
  expect(BasicsInternal.toList(seqOut)) |> toEqual([2, 4, 6]);
});

test("filter", () => {
  let seqIn = BasicsInternal.ofList([1, -2, 3, -4]);
  let seqOut = BasicsInternal.filter(~f = x=>x >= 0, seqIn);
  expect(BasicsInternal.toList(seqOut)) |> toEqual([1, 3]);
});
