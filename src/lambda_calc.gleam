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

fn beta_reduce(node: ASTNode, from from: Variable, to to: ASTNode) -> ASTNode {
  case node {
    ApplicationNode(_) as application -> evaluate(application)
    AbstractionNode(abstraction) -> {
      AbstractionNode(
        Abstraction(..abstraction, in: beta_reduce(abstraction.in, from:, to:)),
      )
    }
    VariableNode(variable) as default -> {
      case variable == from {
        True -> to
        False -> default
      }
    }

    ConstantNode(_) -> todo
  }
}

pub fn evaluate(node: ASTNode) -> ASTNode {
  case node {
    ApplicationNode(application) as default -> {
      case application.abstraction, application.value {
        // Abstraction Value
        AbstractionNode(abstraction), value -> {
          beta_reduce(abstraction.in, from: abstraction.bind, to: value)
        }

        ApplicationNode(_) as application, value ->
          evaluate(
            ApplicationNode(Application(
              abstraction: evaluate(application),
              value: value,
            )),
          )

        ConstantNode(_), _ -> default

        VariableNode(_), _ -> default
      }
    }
    AbstractionNode(_) -> node
    VariableNode(_) -> node
    ConstantNode(_) -> node
  }

}

fn usage() {
  io.println("Usage:      gleam run <input.lmb>")
  io.println("Options:    -e --export Export a mermaid diagram of the ast")
}

fn print_expect_message(err: UnexpectedTokenError) {
  io.println("Encountered unexpected token.\nExpected one of the following: ")
  list.each(err.expected, fn(kind) {
    io.println(" - " <> lexer.token_kind_to_string(kind))
  })
  io.println("But got: " <> lexer.token_kind_to_string(err.got))
}

pub fn main() {
  list.each([1, 2], fn(_a) { Nil })
  let args = argv.load().arguments
  let export_ast = list.contains(args, "-e") || list.contains(args, "--export")

  use file <- result.try(case args {
    [file, ..] -> Ok(file)
    _ -> {
      io.println_error("Missing argument\n")
      usage()
      Error(Nil)
    }
  })

  case simplifile.read(file) {
    Ok(contents) -> {
      use ast <- result.try(
        lexer.new(contents)
        |> ast.from_lexer
        |> result.map_error(handle_error),
      )

      case export_ast {
        True ->
          ast.to_mermaid_flowchart(ast)
          |> ast.flowchart_to_image("image.jpg")
        False -> Ok(Nil)
      }
    }

    Error(simpli_error) -> {
      simplifile.describe_error(simpli_error) |> io.print
      io.println(":  '" <> file <> "'")
      Ok(Nil)
    }
  }
}
