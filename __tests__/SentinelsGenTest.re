open Jest;
open Expect;

let joinViaIndex = (~sep: string = "", g: Gen.t(string)) => {
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

/* You could examine if state is empty. But then the separator wouldn’t work for empty strings in the sequence */

test("joinViaIndex", () => {
  expect(joinViaIndex(~sep="|", Gen.of_list(["a", "b", "c"])))
    |> toBe("a|b|c")
});

let joinViaLookAhead = (~sep: string = "", g: Gen.t(string)) => {
  Gen.fold(
    (state, (str, lookAhead)) =>
      /* No separator after the last element */
      switch lookAhead {
      | None => state ++ str
      | _ => state ++ str ++ sep
      },
    "",
    Gen.peek(g)
  );
};

test("joinViaLookAhead", () => {
  expect(joinViaLookAhead(~sep="|", Gen.of_list(["a", "b", "c"])))
    |> toBe("a|b|c")
});


/* Similar problem: Gen.intersperse() */
