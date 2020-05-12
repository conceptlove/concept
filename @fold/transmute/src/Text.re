type url =
  | Url(string);

type inline =
  | Many(list(inline))
  | String(string)
  | Link(inline, url)
  | Bold(inline)
  | Italic(inline);

type listType =
  | Ordered
  | Unordered;

type block =
  | Many(list(block))
  | Heading(int, inline)
  | Paragraph(inline)
  | List(listType, list(block));

let rec formatInline = (inline: inline) =>
  switch (inline) {
  | Many([]) => ""
  | Many([v, ...rest]) => formatInline(v) ++ formatInline(Many(rest))
  | String(str) => str
  | Bold(rest) => "**" ++ formatInline(rest) ++ "**"
  | Italic(rest) => "_" ++ formatInline(rest) ++ "_"
  | _ => "<nothing>"
  };

let formatListType = (t: listType) =>
  switch (t) {
  | Ordered => "1."
  | Unordered => "*"
  };

let rec formatBlock = (block: block) =>
  switch (block) {
  | Many([]) => ""
  | Many([v, ...rest]) => formatBlock(v) ++ "\n" ++ formatBlock(Many(rest))

  | List(_, []) => ""
  | List(t, [v, ...rest]) =>
    formatListType(t)
    ++ " "
    ++ formatBlock(v)
    ++ formatBlock(List(t, rest))
  | Heading(1, inline) => "# " ++ formatInline(inline) ++ "\n"
  | Paragraph(inline) => formatInline(inline) ++ "\n"
  | _ => "<nothing>"
  };
