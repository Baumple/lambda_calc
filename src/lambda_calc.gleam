import argv
import ast.{
  type ASTNode, type Abstraction, type Application, type Constant, type Variable,
  Abstraction, AbstractionNode, Application, ApplicationNode, Constant,
  ConstantNode, Variable, VariableNode,
}
import error.{
  type Error, type UnexpectedTokenError, EOFReached, Nill, UnclosedParen,
  UnexpectedToken, UnexpectedTokenError,
}
import gleam/io
import gleam/list
import gleam/result
import lexer
import pprint
import simplifile

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

fn apply(func: Abstraction, value: Int) {
  todo
}

fn execute(node: ASTNode) -> Int {
  case node {
    ApplicationNode(Application(func, value)) -> {
      case func {
        AbstractionNode(abstraction) -> apply(abstraction, execute(value))
        ApplicationNode(_) -> todo
        ConstantNode(Constant(value)) -> value
        VariableNode(_var_name) -> todo
      }
    }
    AbstractionNode(Abstraction(variable, expression)) -> {
      todo
    }
    ConstantNode(Constant(value)) -> todo
    VariableNode(Variable(name)) -> todo
  }
}

fn usage() {
  io.println("Usage: gleam run <input.lmb>")
}

fn print_expect_message(err: UnexpectedTokenError) {
  io.println("Encountered unexpected token.\nExpected one of the following: ")
  list.each(err.expected, fn(kind) {
    io.println(" - " <> lexer.token_kind_to_string(kind))
  })
  io.println("But got: " <> lexer.token_kind_to_string(err.got))
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
    |> ast.from_lexer
    |> result.map_error(handle_error)
    |> pprint.debug
    |> result.try(fn(ast) {
      ast
      |> ast.to_mermaid
      |> ast.flowchart_to_image("image.jpg")
    })

  Ok(Nil)
}
