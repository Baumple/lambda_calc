import error.{
  type Error, type UnexpectedTokenError, EOFReached, UnexpectedToken,
  UnexpectedTokenError,
}
import gleam/erlang/charlist
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lexer.{type Lexer, type Token, type TokenKind, Token}
import simplifile

pub type ASTNode {
  AssignmentNode(Assignment)
  ApplicationNode(Application)
  AbstractionNode(Abstraction)
  VariableNode(Variable)
  ConstantNode(Constant)
}

/// An assignment of an expression to a variable/identifier such as 
/// `succ <- @x.x`
pub type Assignment {
  Assignment(
    /// The identifier the expression is assigned to
    variable: Variable,
    /// The expression that is assigned to the variable
    expression: ASTNode,
    /// The body in which the binding is valid
    in: ASTNode,
  )
}

pub type Constant {
  Constant(value: Int)
}

pub type Variable {
  Variable(name: String)
}

pub type Abstraction {
  Abstraction(bound_ident: Variable, body: ASTNode)
}

pub type Application {
  Application(abstraction: ASTNode, value: ASTNode)
}

pub opaque type MermaidFlowchart {
  MermaidFlowchart(String)
}

pub fn debug(ast: ASTNode) {
  to_string(ast)
  |> io.println
  ast
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

fn combine_expressions(exps: List(ASTNode)) -> Result(ASTNode, Error) {
  case exps {
    [ConstantNode(_), _, ..] ->
      panic as "Reducing a constant is not yet implemented"

    [AssignmentNode(_), _, ..] -> panic as "Tried to reduce an AssignmentNode"

    [exp1, exp2, ..rest] -> {
      let temp = ApplicationNode(Application(exp1, exp2))
      use reduced <- result.try(combine_expressions([temp, ..rest]))
      Ok(reduced)
    }

    [exp] -> Ok(exp)

    [] -> panic as "Passed an empty list of expressions to be reduced"
  }
}

fn parse_abstraction(lexer: Lexer) -> Result(#(Option(ASTNode), Lexer), Error) {
  use var_name, lexer <- expect_token(lexer, [lexer.Ident])
  use _, lexer <- expect_token(lexer, [lexer.LambdaDot])

  let body_res = parse_expression(lexer)
  use #(body, lexer) <- result.try(body_res)
  let body = option.to_result(body, EOFReached([]))
  use body <- result.try(body)

  Ok(#(
    Some(
      AbstractionNode(Abstraction(bound_ident: Variable(var_name.text), body:)),
    ),
    lexer,
  ))
}

fn parse_ident(
  lexer: Lexer,
  ident: Variable,
) -> Result(#(Option(ASTNode), Lexer), Error) {
  // check the next token, whether it is an Assign
  let #(token, next_lexer) = lexer.next_token(lexer)
  case next_token {
    Token(kind: lexer.Assign, ..) -> {
      // Parse the expression that will be bound to the identifier
      use #(expression, lexer) <- result.try(parse_expression(next_lexer))
      todo
    }
    _ -> Ok(#(Some(VariableNode(ident)), lexer))
  }
}

fn parse_expression(lexer: Lexer) -> Result(#(Option(ASTNode), Lexer), Error) {
  let #(token, lexer) = lexer.next_token(lexer)
  case token.kind {
    lexer.Lambda -> parse_abstraction(lexer)

    lexer.Ident -> parse_ident(lexer, Variable(token.text))

    // TODO: Error handling
    lexer.Number ->
      Ok(#(
        Some(
          ConstantNode(Constant(token.text |> int.parse |> result.unwrap(-1))),
        ),
        lexer,
      ))

    lexer.LParen -> {
      use #(expressions, lexer) <- result.try(parse_expressions(lexer))
      use reduced <- result.try(combine_expressions(expressions))
      Ok(#(Some(reduced), lexer))
    }

    lexer.RParen -> Ok(#(None, lexer))

    lexer.EOF -> Ok(#(None, lexer))

    _ ->
      panic as {
        "Encountered unexpected token: "
        <> lexer.token_kind_to_string(token.kind)
      }
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

pub fn from_lexer(lexer: Lexer) -> Result(ASTNode, Error) {
  parse_expressions(lexer)
  |> result.try(fn(res) { combine_expressions(res.0) })
}

pub fn to_string(root node: ASTNode) -> String {
  case node {
    AssignmentNode(assignment) ->
      "("
      <> assignment.variable.name
      <> " <- "
      <> to_string(assignment.expression)
      <> to_string(assignment.in)
      <> ")"

    AbstractionNode(abstraction) ->
      "(λ"
      <> abstraction.bound_ident.name
      <> "."
      <> to_string(abstraction.body)
      <> ")"

    ApplicationNode(application) ->
      "("
      <> to_string(application.abstraction)
      <> " "
      <> to_string(application.value)
      <> ")"

    ConstantNode(Constant(value)) -> int.to_string(value)

    VariableNode(Variable(name)) -> name
  }
}

/// Creates a mermaid link between two nodes.
/// The resulting string will look like this:
///
/// - `N<count_a> --- |<node relation>| N<count_b>\n`
///
/// node_count_a: Node number of first node
/// node_count_b: Node number of connected node
/// node_relation: What node_b is to node_a
fn create_link(
  from node_count_a: Int,
  to node_count_b: Int,
  with node_relation: String,
) -> String {
  "N"
  <> int.to_string(node_count_a)
  <> " --- |"
  <> node_relation
  <> "| "
  <> "N"
  <> int.to_string(node_count_b)
  <> "\n"
}

@external(erlang, "os", "cmd")
fn run_cmd(command: charlist.Charlist) -> String

pub fn flowchart_to_image(
  fl: MermaidFlowchart,
  file_name: String,
) -> Result(Nil, Nil) {
  let MermaidFlowchart(fl) = fl

  use _ <- result.try(
    simplifile.write(contents: fl, to: file_name) |> result.nil_error,
  )

  let cmd =
    { "mmdc -i " <> file_name <> " -o " <> file_name }
    |> charlist.from_string

  run_cmd(cmd)

  Ok(Nil)
}

/// Turns the ast into a mermaid flowchart string
/// so it can be viewed in the browser via [mermaid.live](https://mermaid.live)
pub fn to_mermaid_flowchart(ast_node: ASTNode) -> MermaidFlowchart {
  let doc = "flowchart TD\n"
  let #(node, _) = to_mermaid_flowchart_impl(ast_node, 0)
  MermaidFlowchart(doc <> node)
}

/// TODO: Make code prettier, i guess
fn to_mermaid_flowchart_impl(ast_node: ASTNode, counter: Int) -> #(String, Int) {
  case ast_node {
    AssignmentNode(assignment) -> {
      let current_counter = counter
      let tree = "N" <> int.to_string(current_counter) <> "[Assignment]\n"

      let bound_var = assignment.variable
      let expression = assignment.expression
      let in = assignment.in

      let bound_var_counter = counter + 1
      let #(node, counter) =
        to_mermaid_flowchart_impl(VariableNode(bound_var), bound_var_counter)
      let tree = tree <> node

      let expression_counter = counter + 1
      let #(node, counter) =
        to_mermaid_flowchart_impl(expression, expression_counter)
      let tree = tree <> node

      let in_counter = counter + 1
      let #(node, counter) = to_mermaid_flowchart_impl(in, in_counter)
      let tree = tree <> node

      let tree =
        tree
        <> create_link(
          from: current_counter,
          to: bound_var_counter,
          with: "Identifier",
        )

      let tree =
        tree
        <> create_link(
          from: current_counter,
          to: expression_counter,
          with: "Bound Expression",
        )

      let tree =
        tree <> create_link(from: current_counter, to: in_counter, with: "In")

      #(tree, counter)
    }

    ApplicationNode(application) -> {
      let current_counter = counter
      let tree = "N" <> int.to_string(current_counter) <> "[Application]\n"

      let abstraction = application.abstraction
      let value = application.value

      let abstraction_counter = counter + 1
      let #(node, counter) =
        to_mermaid_flowchart_impl(abstraction, abstraction_counter)
      let tree = tree <> node

      let value_counter = counter + 1
      let #(node, counter) = to_mermaid_flowchart_impl(value, value_counter)
      let tree = tree <> node

      let tree =
        tree
        <> create_link(
          from: current_counter,
          to: abstraction_counter,
          with: "Abstraction",
        )

      let tree =
        tree
        <> create_link(from: current_counter, to: value_counter, with: "Value")

      #(tree, counter)
    }

    AbstractionNode(abstraction) -> {
      let current_counter = counter
      let tree = "N" <> int.to_string(current_counter) <> "(Abstraction)\n"

      let ident = VariableNode(abstraction.bound_ident)
      let in = abstraction.body

      let body_counter = counter + 1
      let #(node, counter) = to_mermaid_flowchart_impl(in, body_counter)
      let tree = tree <> node

      let bind_counter = counter + 1
      let #(node, counter) = to_mermaid_flowchart_impl(ident, bind_counter)
      let tree = tree <> node

      let tree =
        tree
        <> create_link(from: current_counter, to: body_counter, with: "Body")

      let tree =
        tree
        <> create_link(from: current_counter, to: bind_counter, with: "Bind")

      #(tree, counter)
    }

    VariableNode(var) -> {
      let tree =
        "N" <> int.to_string(counter) <> "([Variable: " <> var.name <> "])\n"

      #(tree, counter + 1)
    }

    ConstantNode(constant) -> {
      let tree =
        "N"
        <> int.to_string(counter)
        <> "([Variable: "
        <> constant.value |> int.to_string
        <> "])\n"

      #(tree, counter + 1)
    }
  }
}
