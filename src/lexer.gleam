import gleam/int
import gleam/iterator
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string

pub opaque type Lexer {
  Lexer(data: String, col: Int, row: Int)
}

pub type Token {
  Token(text: String, location: Location, kind: TokenKind)
}

pub type TokenKind {
  Ident
  Number

  Plus
  Sub
  Mult
  Div
  Assign

  LParen
  RParen
  Lambda
  LambdaDot

  EOF
  Invalid
}

pub fn token_kind_to_string(kind: TokenKind) {
  case kind {
    Assign -> "Assign"
    Div -> "Div"
    EOF -> "EOF"
    Ident -> "Ident"
    Invalid -> "Invalid"
    LParen -> "LParen"
    Lambda -> "Lambda"
    LambdaDot -> "LambdaDot"
    Mult -> "Mult"
    Number -> "Number"
    Plus -> "Plus"
    RParen -> "RParen"
    Sub -> "Sub"
  }
}

pub fn expect_to_be(token: Token, kind: TokenKind) -> Option(Token) {
  case token.kind == kind {
    True -> option.Some(token)
    False -> option.None
  }
}

pub type Location {
  Location(col: Int, row: Int)
}

pub fn new(data: String) -> Lexer {
  Lexer(data:, col: 1, row: 1)
}

fn is_whitespace(c: String) -> Bool {
  case c {
    "\t" | "\n" | " " -> True
    _ -> False
  }
}

fn is_alpha(c: String) -> Bool {
  string.to_utf_codepoints(c)
  |> list.all(fn(c) {
    let s = string.utf_codepoint_to_int(c)
    { 65 <= s && s <= 90 } || { 97 <= s && s <= 122 }
  })
}

fn is_num(c: String) -> Bool {
  case c {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn is_special(c: String) -> Bool {
  !{ is_num(c) || is_alpha(c) || is_whitespace(c) }
}

fn advance(
  lexer: Lexer,
  while condition: fn(String) -> Bool,
) -> #(String, Lexer) {
  advance_acc(lexer, condition, "")
}

fn advance_acc(
  lexer: Lexer,
  condition: fn(String) -> Bool,
  acc: String,
) -> #(String, Lexer) {
  case string.pop_grapheme(lexer.data) {
    Ok(#(popped, rest)) ->
      case condition(popped) {
        True -> {
          let #(col, row) = case popped {
            "\n" -> #(1, lexer.row + 1)
            _ -> #(lexer.col + 1, lexer.row)
          }
          advance_acc(Lexer(data: rest, row:, col:), condition, acc <> popped)
        }
        False -> #(acc, lexer)
      }
    Error(..) -> #(acc, lexer)
  }
}

fn get_location(lexer: Lexer) -> Location {
  Location(col: lexer.col, row: lexer.row)
}

fn parse_alpha(lexer: Lexer) -> #(Token, Lexer) {
  let location = get_location(lexer)
  let #(text, lexer) = advance(lexer, while: is_alpha)
  #(Token(location:, text:, kind: Ident), lexer)
}

fn parse_num(lexer: Lexer) -> #(Token, Lexer) {
  let location = get_location(lexer)
  let #(text, lexer) = advance(lexer, while: is_num)
  #(Token(text:, location:, kind: Number), lexer)
}

fn parse_special(lexer: Lexer, popped) -> #(Token, Lexer) {
  let location = get_location(lexer)

  let advance_by_1 = fn(lexer: Lexer) {
    let assert Ok(#(_, rest)) = string.pop_grapheme(lexer.data)
    Lexer(data: rest, col: lexer.col + 1, row: lexer.row)
  }

  case popped {
    // Single special chars
    "." -> #(
      Token(text: popped, location:, kind: LambdaDot),
      advance_by_1(lexer),
    )
    "(" -> #(Token(text: popped, location:, kind: LParen), advance_by_1(lexer))
    ")" -> #(Token(text: popped, location:, kind: RParen), advance_by_1(lexer))
    "@" | "λ" -> #(
      Token(text: popped, location:, kind: Lambda),
      advance_by_1(lexer),
    )
    "+" -> #(Token(text: popped, location:, kind: Plus), advance_by_1(lexer))
    "-" -> #(Token(text: popped, location:, kind: Sub), advance_by_1(lexer))
    "*" -> #(Token(text: popped, location:, kind: Mult), advance_by_1(lexer))
    "/" -> #(Token(text: popped, location:, kind: Div), advance_by_1(lexer))

    // Mutli character operator
    _ -> {
      let #(text, lexer) = advance(lexer, while: is_special)
      let token = case text {
        "<-" -> Token(text:, location:, kind: Assign)

        text -> Token(text:, location:, kind: Invalid)
      }
      #(token, lexer)
    }
  }
}

type CharacterType {
  Alpha(String)
  Num(String)
  Special(String)
}

fn character_type(c: String) -> CharacterType {
  case is_alpha(c), is_num(c), is_special(c) {
    True, _, _ -> Alpha(c)
    False, True, _ -> Num(c)
    False, False, True -> Special(c)
    _, _, _ -> panic as "Impossible state"
  }
}

pub fn next_token(lexer: Lexer) -> #(Token, Lexer) {
  let #(_, lexer) = advance(lexer, while: is_whitespace)
  let res = string.first(lexer.data)

  case res {
    Ok(first) -> {
      case character_type(first) {
        Alpha(_) -> parse_alpha(lexer)
        Num(_) -> parse_num(lexer)
        Special(special) -> parse_special(lexer, special)
      }
    }
    Error(..) -> #(
      Token(text: "", location: get_location(lexer), kind: EOF),
      lexer,
    )
  }
}

pub fn into_iter(lexer: Lexer) -> iterator.Iterator(Token) {
  iterator.unfold(lexer, fn(lexer) {
    let #(token, lexer) = next_token(lexer)
    case token.kind {
      EOF(..) -> iterator.Done
      _ -> iterator.Next(token, lexer)
    }
  })
}
