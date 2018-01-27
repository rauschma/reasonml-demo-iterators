open Jest;
open Expect;

test("ofList, toList", () => {
  let gen = BasicsExternal.ofList(["a", "b", "c"]);
  expect(BasicsExternal.toList(gen)) |> toEqual(["a", "b", "c"]);
});

test("length", () => {
  let gen = BasicsExternal.ofList(["a", "b", "c"]);
  expect(BasicsExternal.length(gen)) |> toBe(3);
});

test("map", () => {
  let genIn = BasicsExternal.ofList([1, 2, 3]);
  let genOut = BasicsExternal.map(~f = x=>2*x, genIn);
  expect(BasicsExternal.toList(genOut)) |> toEqual([2, 4, 6]);
});

test("filter", () => {
  let genIn = BasicsExternal.ofList([1, -2, 3, -4]);
  let genOut = BasicsExternal.filter(~f = x=>x >= 0, genIn);
  expect(BasicsExternal.toList(genOut)) |> toEqual([1, 3]);
});
