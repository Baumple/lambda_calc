import argv
import ast.{
  type ASTNode, type Abstraction, type Application, type Variable, Abstraction,
  AbstractionNode, Application, ApplicationNode, ConstantNode, Variable,
  VariableNode,
}
import error.{
  type Error, type UnexpectedTokenError, EOFReached, Nill, UnclosedParen,
  UnexpectedToken, UnexpectedTokenError,
}
import gleam/function
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

fn replace_variable(
  root current_node: ASTNode,
  from from: Variable,
  to to: ASTNode,
) -> ASTNode {
  case current_node {
    ApplicationNode(application) ->
      ApplicationNode(Application(
        abstraction: replace_variable(root: application.abstraction, from:, to:),
        value: replace_variable(root: application.value, from:, to:),
      ))

    AbstractionNode(abstraction) ->
      AbstractionNode(
        Abstraction(
          ..abstraction,
          body: replace_variable(root: abstraction.body, from:, to:),
        ),
      )

    VariableNode(v) as vn ->
      case v == from {
        True -> to
        False -> vn
      }

    ConstantNode(_) as cn -> cn
  }
}

fn debug_ast(ast: ASTNode) {
  ast.to_string(ast) |> io.println
  ast
}

fn evaluate_application(application: Application) -> ASTNode {
  let abstraction = evaluate(application.abstraction)
  let value = application.value

  case abstraction {
    ApplicationNode(application) ->
      ApplicationNode(Application(evaluate(application.abstraction), value))
      |> evaluate

    AbstractionNode(abstraction) -> {
      let body = abstraction.body
      let variable = abstraction.bound_ident

      replace_variable(root: body, from: variable, to: value)
      |> evaluate
    }

    ConstantNode(_) as cn ->
      ApplicationNode(Application(abstraction: cn, value: evaluate(value)))

    VariableNode(_) as vn ->
      ApplicationNode(Application(abstraction: vn, value: evaluate(value)))
  }
}

pub fn evaluate(node: ASTNode) -> ASTNode {
  case node {
    ApplicationNode(application) -> evaluate_application(application)

    AbstractionNode(abstraction) ->
      AbstractionNode(
        Abstraction(..abstraction, body: evaluate(abstraction.body)),
      )

    ConstantNode(_) as cn -> cn

    VariableNode(_) as vn -> vn
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

      io.println(ast.to_string(ast))

      // let assert Ok(_) =
      evaluate(ast)
      |> function.tap(fn(ast) {
        ast.to_string(ast)
        |> io.println
      })
      // |> ast.to_mermaid_flowchart
      // |> ast.flowchart_to_image("output")

      case export_ast {
        True ->
          ast.to_mermaid_flowchart(ast)
          |> ast.flowchart_to_image("image")
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
