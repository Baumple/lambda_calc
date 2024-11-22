import argv
import ast.{
  type ASTNode, type Abstraction, type Application, type Assignment,
  type Variable, Abstraction, AbstractionNode, Application, ApplicationNode,
  Assignment, AssignmentNode, ConstantNode, Variable, VariableNode,
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

fn replace_variable(
  root current_node: ASTNode,
  from from: Variable,
  to to: ASTNode,
) -> ASTNode {
  case current_node {
    AssignmentNode(assignment) ->
      AssignmentNode(
        Assignment(
          ..assignment,
          in: replace_variable(assignment.in, from:, to:),
        ),
      )

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

    VariableNode(v) as vn -> {
      case v == from {
        True -> to
        False -> vn
      }
    }

    ConstantNode(_) as cn -> cn
  }
}

fn evaluate_application(application: Application) -> ASTNode {
  let abstraction = evaluate(application.abstraction)
  let value = application.value

  case abstraction {
    AssignmentNode(_) -> panic as "Cannot apply value to an Assignment"

    AbstractionNode(abstraction) -> {
      let body = evaluate(abstraction.body)
      let variable = abstraction.bound_ident

      replace_variable(root: body, from: variable, to: value)
      |> evaluate
    }

    ApplicationNode(_) ->
      ApplicationNode(Application(evaluate(abstraction), value))

    // BUG: "n f" is evaluated to "n f" which is evaluated recursively again
    VariableNode(_) as vn ->
      ApplicationNode(Application(abstraction: vn, value: evaluate(value)))

    ConstantNode(_) as cn ->
      ApplicationNode(Application(abstraction: cn, value:))
  }
}

pub fn evaluate(node: ASTNode) -> ASTNode {
  case node {
    AssignmentNode(assignment) ->
      replace_variable(
        assignment.in,
        from: assignment.variable,
        to: assignment.expression,
      )
      |> evaluate

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

      io.println("Input: " <> ast.to_string(ast))

      let evaluated = evaluate(ast)
      io.print("Evaluated: ")
      ast.debug(evaluated)

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
