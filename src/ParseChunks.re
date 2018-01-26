/* :::::::::: Shared :::::::::: */

let titleRegex = [%re "/^## (.*)$/"];

type chunk = {
  title: string,
  body: list(string),
};

let extractTitle = (line: string) =>
  switch (Js.Re.exec(line, titleRegex)) {
  | Some(result) =>
    let captures = Js.Re.captures(result);
    Js.Nullable.to_opt(captures[1]);
  | None => None
  };

let appendLine = (line: string, {body} as ch: chunk) => {
  {...ch, body: [line, ...body]};
};

let reverseLines = ({body} as ch: chunk) => {
  {...ch, body: List.rev(body)};
};

/* :::::::::: Functional implementation :::::::::: */

let toOptChunk = ({body} as ch: chunk) =>
  if (List.length(body) == 0) {
    /* Chunk is empty, discard it */
    None;
  } else {
    /* Lines are in reverse order, change that */
    Some(reverseLines(ch));
  };

/*TMD.begin unfoldScanFunc*/
let unfoldScanFunc = (state: chunk, line: string) => {
  switch (extractTitle(line)) {
  | None =>
    let nextState = appendLine(line, state);
    let output = None;
    (nextState, output)
  | Some(title) =>
    let nextState = {title, body: [line]};
    let output = toOptChunk(state);
    (nextState, output);
  };
};
/*TMD.end unfoldScanFunc*/

let linesToChunks = (lines: Gen.t(string)): Gen.t(chunk) => {
  /* Add a trigger for the last chunk */
  let sentinel = Gen.singleton("## Will be discarded");
  let linesWithSentinel = Gen.append(lines, sentinel);
  let firstState = {title: "PREFIX", body: []};
  let genWithOpts = Gen.unfold_scan(unfoldScanFunc,
    firstState, linesWithSentinel);
  /* Remove None, unwrap Some() */
  Gen.filter_map(x => x, genWithOpts);
};

/* :::::::::: Imperative implementation :::::::::: */

let linesToChunksImp = (lines: Gen.t(string)): Gen.t(chunk) => {
  let lineOpt = lines();
  let init =
    switch (lineOpt) {
    | None =>
      None;
    | Some(line) =>
      let title = switch (extractTitle(line)) {
        | None => "PREFIX"
        | Some(t) => t
        };
      Some({title, body: [line]});
    };
  let curChunkOpt = ref(init);
  let rec aux = () => {
    switch (curChunkOpt^) {
    | None => None
    | Some(curChunk) =>
      let lineOpt = lines();
      switch lineOpt {
      | None =>
        curChunkOpt := None;
        Some(reverseLines(curChunk));
      | Some(line) =>
        switch (extractTitle(line)) {
        | None =>
          curChunkOpt := Some(appendLine(line, curChunk));
          aux(); /* Keep going until chunk is finished */
        | Some(title) =>
          curChunkOpt := Some({title, body: [line]});
          Some(reverseLines(curChunk));
        };
      };        
    }
  };
  () => aux();
};
