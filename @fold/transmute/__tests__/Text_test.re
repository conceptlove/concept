open Jest;
open Expect;
open Text;

let expected = (expected: string, node: block) =>
  expect(node |> formatBlock |> String.trim) |> toBe(expected |> String.trim);

let doc =
  Many([
    Heading(
      1,
      Many([
        String("An example of "),
        Bold(String("Text")),
        String(" usage"),
      ]),
    ),
    Paragraph(String("A simple paragraph.")),
    List(
      Unordered,
      [
        Paragraph(String("First item")),
        Paragraph(String("Second item")),
        Paragraph(String("Third item")),
      ],
    ),
  ]);

describe("formatBlock", () => {
  test("heading", () =>
    Heading(1, String("Hello")) |> expected("# Hello")
  );

  test("paragraph", () =>
    Paragraph(String("Hello")) |> expected("Hello")
  );

  test("doc", () =>
    doc
    |> expected(
         {|
# An example of **Text** usage

A simple paragraph.

* First item
* Second item
* Third item
|},
       )
  );
});
