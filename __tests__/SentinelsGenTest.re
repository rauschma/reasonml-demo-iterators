open Jest;
open Expect;

let join1 = (~sep: string = "", g: Gen.t(string)) => {
  Gen.fold(
    (state, (str, lookahead)) =>
      /* No separator after the last element */
      switch lookahead {
      | None => state ++ str
      | _ => state ++ str ++ sep
      },
    "",
    Gen.peek(g)
  );
};

/* You could examine if state is empty. But then the separator wouldn’t work for empty strings in the sequence */

test("join1", () => {
  expect(join1(~sep="|", Gen.of_list(["a", "b", "c"])))
    |> toBe("a|b|c")
});


let join2 = (~sep: string = "", g: Gen.t(string)) => {
  Gen.fold(
    (state, (index, str)) =>
      /* No separator before the first element */
      if (index == 0) {
        state ++ str;
      } else {
        state ++ sep ++ str;
      },
    "",
    Gen.zip_index(g)
  );
};

test("join2", () => {
  expect(join2(~sep="|", Gen.of_list(["a", "b", "c"])))
    |> toBe("a|b|c")
});






/* Similar: Gen.intersperse() */
