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
    None;
  } else {
    Some(reverseLines(ch));
  };

let linesToChunks = (lines: Gen.t(string)): Gen.t(chunk) => {
  let func = (state: chunk, line: string) => {
    switch (extractTitle(line)) {
    | None => (appendLine(line, state), None)
    | Some(title) =>
      let nextState = {title, body: [line]};
      (nextState, toOptChunk(state));
    };
  };
  let linesWithSentinel = Gen.append(lines, Gen.singleton("## Will be discarded"));
  let genWithOpts = Gen.unfold_scan(func, {title: "PREFIX", body: []}, linesWithSentinel);
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
