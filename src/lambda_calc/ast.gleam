import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lambda_calc/lexer.{type Lexer, type Token, type TokenKind, Token}
import lambda_calc/syntax_error.{
  type SyntaxError, type UnexpectedTokenError, AssignmentWithoutBody,
  AssignmentWithoutBoundExpression, EOFReached, ExpectedExpressions,
  InvalidToken, UnclosedParen, UnexpectedToken, UnexpectedTokenError,
}

/// The possible AST nodes that may occur.
pub type ASTNode {
  /// An Assignment
  AssignmentNode(Assignment)
  /// An Application
  ApplicationNode(Application)
  /// An Abstraction (i.e. a declaration of an anonymous function)
  AbstractionNode(Abstraction)
  /// A Variable node
  VariableNode(Variable)
  /// TODO: A constant node
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

/// A Constant such as the number `2`
pub type Constant {
  /// A Constant such as the number `2`
  Constant(value: Int)
}

/// A Variable such as `x` or `value`
pub type Variable {
  Variable(name: String)
}

/// An Abstraction (i.e. anonymous function declaration `\x.x`)
/// * `bound_ident` - The identifier that is bound by the Abstraction
/// * `body` - The body/expression that is returned by the anonymous function.
pub type Abstraction {
  Abstraction(bound_ident: Variable, body: ASTNode)
}

/// An Application of two lambda expressions
/// * `abstraction` - The lambda expression that is applied to the value.
/// * `value` - The value that the function will be applied to.
pub type Application {
  Application(left_side: ASTNode, right_side: ASTNode)
}

/// Takes in an ast, converts it into a text representation, prints it to the
/// console and then returns it again.
pub fn debug(ast: ASTNode) {
  to_string(ast)
  |> io.println
  ast
}

/// Creates an `SyntaxError(UnexpectedToken(..))` error
fn new_unexpected_token(
  expected expected: List(TokenKind),
  got got: Token,
) -> SyntaxError {
  UnexpectedToken(UnexpectedTokenError(
    expected:,
    got: got.kind,
    location: got.location,
  ))
}

/// ParseResult of a parsing operation
type ParseResult {
  ParseResult(
    /// An optional expression. When None there was nothing left to parse.
    expression: Option(ASTNode),
    /// The updated lexer
    lexer: Lexer,
  )
}

fn combine_expressions(exps: List(ASTNode)) -> Result(ASTNode, SyntaxError) {
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

    [] -> panic as "Should be impossible state"
  }
}

/// Checks if the next token of the lexer matches the expected ones.
/// If not, returns an error variant.
fn expect_token(
  lexer: Lexer,
  expected: List(TokenKind),
  process: fn(Token, Lexer) -> Result(ParseResult, SyntaxError),
) -> Result(ParseResult, SyntaxError) {
  let #(token, lexer) = lexer.next_token(lexer)

  case list.contains(expected, token.kind) {
    True -> process(token, lexer)
    False -> Error(new_unexpected_token(expected:, got: token))
  }
}

/// Parses an abstraction.
fn parse_abstraction(lexer: Lexer) -> Result(ParseResult, SyntaxError) {
  use var_name, lexer <- expect_token(lexer, [lexer.Ident])
  use _, lexer <- expect_token(lexer, [lexer.LambdaDot])

  let body_res = parse_expression(lexer)
  use ParseResult(expression: body, lexer:) <- result.try(body_res)
  let body = option.to_result(body, EOFReached([]))
  use body <- result.try(body)

  let parsed =
    ParseResult(
      expression: Some(
        AbstractionNode(Abstraction(bound_ident: Variable(var_name.text), body:)),
      ),
      lexer:,
    )
  Ok(parsed)
}

/// Parses an identifier. If the token after an identifier is a `lexer.Assign`
/// parses an `AssignmentNode` otherwise it parses a `VariableNode`.
fn parse_ident(
  lexer: Lexer,
  variable: Variable,
) -> Result(ParseResult, SyntaxError) {
  let initial_lexer = lexer

  case lexer.next_token(initial_lexer) {
    #(Token(kind: lexer.Assign, ..), lexer) -> parse_assignment(lexer, variable)
    _ -> Ok(ParseResult(Some(VariableNode(variable)), initial_lexer))
  }
}

fn parse_assignment(
  lexer: Lexer,
  ident: Variable,
) -> Result(ParseResult, SyntaxError) {
  // Parse the expression that will be bound to the identifier
  use ParseResult(expression: bound_expression, lexer:) <- result.try(
    parse_expression(lexer),
  )
  // check whether there is an expression (assignments are required to have
  // an assigned body)
  use valid_expression <- result.try(option.to_result(
    bound_expression,
    AssignmentWithoutBody(lexer.get_location(lexer)),
  ))

  // the expression the assignment is valid in
  use ParseResult(expression: in_body, lexer:) <- result.try(parse_expression(
    lexer,
  ))

  // Ensure the assignment is followed by a body in which the identifier is
  // can be used.
  use valid_in_body <- result.try(option.to_result(
    in_body,
    AssignmentWithoutBoundExpression(lexer.get_location(lexer)),
  ))

  let assignment =
    AssignmentNode(Assignment(
      variable: ident,
      expression: valid_expression,
      in: valid_in_body,
    ))

  Ok(ParseResult(Some(assignment), lexer))
}

// TODO: SyntaxError handling (somewhat done)
fn parse_expression(lexer: Lexer) -> Result(ParseResult, SyntaxError) {
  let #(token, lexer) = lexer.next_token(lexer)
  case token.kind {
    lexer.Lambda -> parse_abstraction(lexer)

    lexer.Ident -> parse_ident(lexer, Variable(token.text))

    lexer.Number ->
      Ok(ParseResult(
        Some(
          ConstantNode(Constant(token.text |> int.parse |> result.unwrap(-1))),
        ),
        lexer,
      ))

    lexer.LParen -> {
      use #(expressions, lexer) <- result.try(parse_expressions(lexer))
      case expressions {
        [_, ..] -> {
          use reduced <- result.try(combine_expressions(expressions))
          Ok(ParseResult(Some(reduced), lexer))
        }
        [] -> Error(ExpectedExpressions(lexer.get_location(lexer)))
      }
    }

    lexer.RParen | lexer.EOF -> Ok(ParseResult(None, lexer))

    _ -> Error(InvalidToken(token.text, lexer.get_location(lexer)))
  }
}

/// Parses a list of chained expressions.
fn parse_expressions(
  lexer: Lexer,
) -> Result(#(List(ASTNode), Lexer), SyntaxError) {
  impl_parse_expressions(lexer, [])
  |> result.map(fn(exps) { #(list.reverse(exps.0), exps.1) })
}

fn impl_parse_expressions(
  lexer: Lexer,
  nodes: List(ASTNode),
) -> Result(#(List(ASTNode), Lexer), SyntaxError) {
  use node <- result.try(parse_expression(lexer))
  case node {
    ParseResult(Some(node), lexer) ->
      impl_parse_expressions(lexer, [node, ..nodes])
    ParseResult(None, lexer) -> Ok(#(nodes, lexer))
  }
}

/// Prevents an empty expression stack to combined
fn prevent_empty(
  parse_result: #(List(ASTNode), Lexer),
) -> Result(#(List(ASTNode), Lexer), SyntaxError) {
  case parse_result.0 {
    [_, ..] -> Ok(parse_result)
    [] -> Error(ExpectedExpressions(lexer.get_location(parse_result.1)))
  }
}

/// Checks whether all parenthesis have been matched. Otherwise returns an
/// error.
fn check_parenthesis(lexer: Lexer) -> Result(Nil, SyntaxError) {
  impl_check_parenthesis(lexer, 0)
}

fn impl_check_parenthesis(lexer: Lexer, n: Int) -> Result(Nil, SyntaxError) {
  let #(token, lexer) = lexer.next_token(lexer)
  case token {
    Token(kind: lexer.LParen, ..) -> impl_check_parenthesis(lexer, n + 1)
    Token(kind: lexer.RParen, ..) -> impl_check_parenthesis(lexer, n - 1)
    Token(kind: lexer.EOF, ..) ->
      case n {
        0 -> Ok(Nil)
        _ -> Error(UnclosedParen)
      }
    _ -> impl_check_parenthesis(lexer, n)
  }
}

/// Takes in a lexer and tries to turn the returned tokens into an ast.
pub fn from_lexer(lexer: Lexer) -> Result(ASTNode, SyntaxError) {
  use _ <- result.try(check_parenthesis(lexer))

  parse_expressions(lexer)
  |> result.try(prevent_empty)
  |> result.try(fn(res) { combine_expressions(res.0) })
}

/// Turns an ast into a slightly more readable text representation.
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
      <> to_string(application.left_side)
      <> " "
      <> to_string(application.right_side)
      <> ")"

    ConstantNode(Constant(value)) -> int.to_string(value)

    VariableNode(Variable(name)) -> name
  }
}
