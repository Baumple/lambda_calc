import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lexer.{type Lexer, type Token, type TokenKind, Token}
import pprint
import simplifile

pub type UnexpectedTokenError {
  UnexpectedTokenError(
    expected: List(TokenKind),
    got: TokenKind,
    location: lexer.Location,
  )
}

pub type UnclosedParenError {
  UnclosedParenError
}

pub type Error {
  UnexpectedToken(UnexpectedTokenError)
  UnclosedParen(UnclosedParenError)
  EOFReached(expected: List(TokenKind))
  Nill
}

fn new_unexpected_token(
  expected expected: List(TokenKind),
  got got: Token,
) -> Error {
  UnexpectedToken(UnexpectedTokenError(
    expected:,
    got: got.kind,
    location: got.location,
  ))
}

fn usage() {
  io.println("Usage: gleam run <input.lmb>")
}

type ASTNode {
  ConstantNode(Constant)
  VariableNode(Variable)
  AbstractionNode(Abstraction)
  ApplicationNode(Application)
}

type Constant {
  Constant(value: Int)
}

type Variable {
  Variable(name: String)
}

type Abstraction {
  Abstraction(variable: Variable, expression: ASTNode)
}

type Application {
  Application(abstraction: ASTNode, value: ASTNode)
}

fn expect_token(
  lexer: Lexer,
  expected: List(TokenKind),
  process: fn(Token, Lexer) -> Result(#(Option(ASTNode), Lexer), Error),
) -> Result(#(Option(ASTNode), Lexer), Error) {
  let #(token, lexer) = lexer.next_token(lexer)

  case list.contains(expected, token.kind) {
    True -> process(token, lexer)
    False -> Error(new_unexpected_token(expected:, got: token))
  }
}

fn parse_abstraction(lexer: Lexer) -> Result(#(Option(ASTNode), Lexer), Error) {
  io.println("Parsing abstraction")

  use var_name, lexer <- expect_token(lexer, [lexer.Ident])
  use _, lexer <- expect_token(lexer, [lexer.LambdaDot])

  let body_res = parse_expression(lexer)
  use #(body, lexer) <- result.try(body_res)
  let body = option.to_result(body, EOFReached([]))
  use body <- result.try(body)

  Ok(#(
    Some(
      AbstractionNode(Abstraction(
        variable: Variable(var_name.text),
        expression: body,
      )),
    ),
    lexer,
  ))
}

fn reduce_expressions(exps: List(ASTNode)) -> Result(ASTNode, Error) {
  case exps {
    [AbstractionNode(..) as exp1, exp2, ..rest] -> {
      let temp = ApplicationNode(Application(exp1, exp2))
      use reduced <- result.try(reduce_expressions([temp, ..rest]))
      Ok(reduced)
    }

    [VariableNode(_) as exp1, exp2, ..rest] -> {
      let temp = ApplicationNode(Application(exp1, exp2))
      use reduced <- result.try(reduce_expressions([temp, ..rest]))
      Ok(reduced)
    }
    [exp] -> Ok(exp)

    [ApplicationNode(_) as exp1, exp2, ..rest] -> {
      let temp = ApplicationNode(Application(exp1, exp2))
      use reduced <- result.try(reduce_expressions([temp, ..rest]))
      Ok(reduced)
    }
    [ConstantNode(_), _, ..] ->
      panic as "Reducing a constant is not yet implemented"
    [] -> panic as "Passed an empty list of expressions to be reduced"
  }
}

fn parse_expression(lexer: Lexer) -> Result(#(Option(ASTNode), Lexer), Error) {
  let #(token, lexer) = lexer.next_token(lexer)
  case token.kind {
    lexer.Lambda -> parse_abstraction(lexer)

    lexer.Ident -> Ok(#(Some(VariableNode(Variable(token.text))), lexer))

    lexer.Number ->
      Ok(#(
        Some(
          ConstantNode(Constant(token.text |> int.parse |> result.unwrap(-1))),
        ),
        lexer,
      ))

    lexer.LParen -> {
      use #(expressions, lexer) <- result.try(parse_expressions(lexer))
      use reduced <- result.try(reduce_expressions(expressions))
      Ok(#(Some(reduced), lexer))
    }

    lexer.RParen -> Ok(#(None, lexer))

    lexer.LambdaDot -> todo as "Unexpected"

    lexer.EOF -> Ok(#(None, lexer))

    _ -> panic as "TF"
  }
}

fn parse_expressions(lexer: Lexer) -> Result(#(List(ASTNode), Lexer), Error) {
  impl_parse_expressions(lexer, [])
  |> result.map(fn(exps) { #(list.reverse(exps.0), exps.1) })
}

fn impl_parse_expressions(
  lexer: Lexer,
  nodes: List(ASTNode),
) -> Result(#(List(ASTNode), Lexer), Error) {
  use node <- result.try(parse_expression(lexer))
  case node {
    #(Some(node), lexer) -> {
      impl_parse_expressions(lexer, [node, ..nodes])
    }
    #(None, lexer) -> Ok(#(nodes, lexer))
  }
}

fn print_expect_message(err: UnexpectedTokenError) {
  io.println("Encountered unexpected token.\nExpected one of the following: ")
  list.each(err.expected, fn(kind) {
    io.println(" - " <> lexer.token_kind_to_string(kind))
  })
  io.println("But got: " <> lexer.token_kind_to_string(err.got))
}

fn handle_error(error: Error) {
  case error {
    UnclosedParen(_err) -> {
      io.println("Encountered unclosed parenthesis.")
    }
    EOFReached(expected) -> {
      io.println("Expected one of the following tokens..")
      list.each(expected, fn(kind) {
        io.println(" - " <> lexer.token_kind_to_string(kind))
      })
      io.println("..but reached end of file")
    }
    UnexpectedToken(err) -> print_expect_message(err)
    Nill -> io.println("Nil deez nuts")
  }
}

fn execute(node: ASTNode) -> Int {
  case node {
    AbstractionNode(Abstraction(variable, expression)) -> {
    }
    ApplicationNode(Application(abstraction, value)) -> todo
    ConstantNode(Constant(value)) -> todo
    VariableNode(Variable(name)) -> todo
  }
}

pub fn main() {
  let args = argv.load().arguments
  use file <- result.try(case args {
    [file] -> Ok(file)
    _ -> {
      io.println_error("Missing argument")
      usage()
      Error(Nil)
    }
  })

  use contents <- result.try(simplifile.read(file) |> result.nil_error)
  let _ =
    lexer.new(contents)
    |> io.debug
    |> parse_expressions
    |> result.map(fn(exps) { exps.0 })
    |> result.try(reduce_expressions)
    |> result.map_error(handle_error)
    |> pprint.debug
    |> execute

  Ok(Nil)
}
