open Jest;
open Expect;

let joinViaIndex = (~sep: string = "", gen: Gen.t(string)) => {
  let foldFunc = (state, (index, str)) =>
    if (index == 0) {
      /* No separator before the first element */
      state ++ str;
    } else {
      state ++ sep ++ str;
    };
  Gen.fold(foldFunc, "", Gen.zip_index(gen));
};

/* You could examine if state is empty. But then the separator wouldn’t work for empty strings in the sequence */

test("joinViaIndex", () => {
  let gen = Gen.of_list(["a", "b", "c"]);
  expect(joinViaIndex(~sep="|", gen))
    |> toBe("a|b|c");
});

let joinViaLookAhead = (~sep: string = "", gen: Gen.t(string)) => {
  let foldFunc = (state, (str, lookAhead)) =>
    switch lookAhead {
    /* No separator after the last element */
    | None => state ++ str
    | _ => state ++ str ++ sep
    };
  Gen.fold(foldFunc, "", Gen.peek(gen));
};

test("joinViaLookAhead", () => {
  let gen = Gen.of_list(["a", "b", "c"]);
  expect(joinViaLookAhead(~sep="|", gen))
    |> toBe("a|b|c");
});


/* Similar problem: Gen.intersperse() */
