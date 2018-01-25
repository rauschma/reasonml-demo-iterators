open Jest;
open Expect;

let lineArray = Js.String.splitByRe([%re "/\\r?\\n/g"], Js.String.trim({js|
## Intro
This article is
about something.
## Details
These are the details
about something.
## Conclusion
This article was about something.
|js}));

let chunkList = ParseChunks.([
  { title: "Intro", body: [
    "## Intro",
    "This article is",
    "about something.",
  ]},
  { title: "Details", body: [
    "## Details",
    "These are the details",
    "about something.",
  ]},
  { title: "Conclusion", body: [
    "## Conclusion",
    "This article was about something.",
  ]},
]);

test("linesToChunks", () => {
  let linesGen = Gen.of_array(lineArray);
  let chunkGen = ParseChunks.linesToChunks(linesGen);
  expect(Gen.to_list(chunkGen))
    |> toEqual(chunkList);
});

test("linesToChunksImp", () => {
  let linesGen = Gen.of_array(lineArray);
  let chunkGen = ParseChunks.linesToChunksImp(linesGen);
  expect(Gen.to_list(chunkGen))
    |> toEqual(chunkList);
});
