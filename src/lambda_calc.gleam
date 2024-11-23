import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import lambda_calc/ast.{
  type ASTNode, type Abstraction, type Application, type Assignment,
  type Variable, Abstraction, AbstractionNode, Application, ApplicationNode,
  Assignment, AssignmentNode, ConstantNode, Variable, VariableNode,
}
import lambda_calc/ast/mermaid
import lambda_calc/lexer
import lambda_calc/syntax_error.{
  type SyntaxError, type UnexpectedTokenError, AssignmentWithoutBody,
  AssignmentWithoutBoundExpression, EOFReached, ExpectedExpressions,
  InvalidToken, UnclosedParen, UnexpectedToken, UnexpectedTokenError,
}
import simplifile

const error_color = "\u{001b}[1;91m"

const reset_color = "\u{001b}[0m"

fn print_error_message(message: String) {
  io.println(error_color <> "ERROR: " <> reset_color <> message)
}

fn print_expect_message(err: UnexpectedTokenError) {
  print_error_message(
    "Encountered unexpected token.\nExpected one of the following: ",
  )
  list.each(err.expected, fn(kind) {
    io.println(" - " <> lexer.token_kind_to_string(kind))
  })
  io.println("But got: " <> lexer.token_kind_to_string(err.got))
}

fn handle_error(error: SyntaxError) {
  case error {
    UnclosedParen -> print_error_message("Encountered unclosed parenthesis.")

    AssignmentWithoutBoundExpression(location) ->
      print_error_message(
        "Missing an expression that is bound to the identifier at ("
        <> int.to_string(location.row)
        <> ":"
        <> int.to_string(location.col)
        <> ")",
      )

    AssignmentWithoutBody(location) ->
      print_error_message(
        "Expected expressions after assignment at ("
        <> int.to_string(location.row)
        <> ":"
        <> int.to_string(location.col)
        <> ")\n  Assignment needs to be followed by an expression",
      )

    ExpectedExpressions(location) ->
      print_error_message(
        "Expected expressions at ("
        <> int.to_string(location.row)
        <> ":"
        <> int.to_string(location.col)
        <> ")",
      )

    // TODO: Make location more accurate
    InvalidToken(text, location) ->
      print_error_message(
        "Invalid Token '"
        <> text
        <> "' at "
        <> int.to_string(location.row)
        <> ":"
        <> int.to_string(location.col)
        <> ")",
      )

    EOFReached(expected) -> {
      print_error_message("Expected one of the following tokens..")
      list.each(expected, fn(kind) {
        io.println(" - " <> lexer.token_kind_to_string(kind))
      })
      io.println("..but reached end of file")
    }

    UnexpectedToken(err) -> print_expect_message(err)
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
          expression: replace_variable(assignment.expression, from:, to:),
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
  let abstraction = evaluate_ast(application.abstraction)
  let value = application.value

  case abstraction {
    AssignmentNode(_) -> panic as "Cannot apply value to an Assignment"

    AbstractionNode(abstraction) -> {
      let body = evaluate_ast(abstraction.body)
      let variable = abstraction.bound_ident

      replace_variable(root: body, from: variable, to: value)
      |> evaluate_ast
    }

    ApplicationNode(_) ->
      ApplicationNode(Application(evaluate_ast(abstraction), value))

    VariableNode(_) as vn ->
      ApplicationNode(Application(abstraction: vn, value: evaluate_ast(value)))

    ConstantNode(_) as cn ->
      ApplicationNode(Application(abstraction: cn, value:))
  }
}

pub fn evaluate(input: String) -> Result(ASTNode, SyntaxError) {
  lexer.new(input)
  |> ast.from_lexer
  |> result.map(evaluate_ast)
}

@internal
pub fn evaluate_ast(node: ASTNode) -> ASTNode {
  case node {
    AssignmentNode(assignment) ->
      replace_variable(
        assignment.in,
        from: assignment.variable,
        to: assignment.expression,
      )
      |> evaluate_ast

    ApplicationNode(application) -> evaluate_application(application)

    AbstractionNode(abstraction) ->
      AbstractionNode(
        Abstraction(..abstraction, body: evaluate_ast(abstraction.body)),
      )

    ConstantNode(_) as cn -> cn

    VariableNode(_) as vn -> vn
  }
}

fn usage() {
  io.println("Usage:      gleam run <input.lmb>")
  io.println("Options:    -e --export Export a mermaid diagram of the ast")
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

      let evaluated = evaluate_ast(ast)
      io.print("Evaluated: ")
      ast.debug(evaluated)

      case export_ast {
        True ->
          mermaid.to_mermaid_flowchart(ast)
          |> mermaid.flowchart_to_image("image")
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
