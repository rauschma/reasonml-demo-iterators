open Jest;
open Expect;

test("ofList, toList", () => {
  let seq = BasicsExternal.ofList(["a", "b", "c"]);
  expect(BasicsExternal.toList(seq)) |> toEqual(["a", "b", "c"]);
});

test("length", () => {
  let seq = BasicsExternal.ofList(["a", "b", "c"]);
  expect(BasicsExternal.length(seq)) |> toBe(3);
});

test("map", () => {
  let seqIn = BasicsExternal.ofList([1, 2, 3]);
  let seqOut = BasicsExternal.map(~f = x=>2*x, seqIn);
  expect(BasicsExternal.toList(seqOut)) |> toEqual([2, 4, 6]);
});

test("filter", () => {
  let seqIn = BasicsExternal.ofList([1, -2, 3, -4]);
  let seqOut = BasicsExternal.filter(~f = x=>x >= 0, seqIn);
  expect(BasicsExternal.toList(seqOut)) |> toEqual([1, 3]);
});
