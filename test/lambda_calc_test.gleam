import gleeunit
import gleeunit/should
import lambda_calc
import lambda_calc/ast
import lambda_calc/lexer
import lambda_calc/syntax_error as error

pub fn main() {
  gleeunit.main()
}

fn test_evaluate(input: String, expected: String) {
  let ast_node =
    lambda_calc.evaluate(input)
    |> should.be_ok

  lexer.new(expected)
  |> ast.from_lexer
  |> should.be_ok
  |> should.equal(ast_node)
}

// gleeunit test functions end in `_test`
pub fn evaluate_test() {
  test_evaluate("(\\x.x) y", "y")
  test_evaluate("(λs.(λz.z)) 4", "\\z.z")
  test_evaluate("(\\n.\\f.\\x.(f (n f x)))(\\f.\\x.x)", "\\f.\\x.(f x)")
  test_evaluate("(λw.λy.λx.(y (w y x))) (λs.(λz.z))", "λy.λx.(y x)")
  test_evaluate(
    "(λw.λy.λx.(y (w y x))) (λs.(λz.(s z)))",
    "λy.λx.(y (y x))",
  )
  test_evaluate(
    "((λn.λf.λx.(n (λg.λh.h (g f))) (λu.x) (λu.u))) (@f.@x.x)",
    "@f.@x.x",
  )

  test_evaluate(
    "(λn.(λf.(λx.(((n (λg.(λh.(h (g f))))) (λu.x)) (λu.u)))))",
    "(λn.(λf.(λx.(((n (λg.(λh.(h (g f))))) (λu.x)) (λu.u)))))",
  )
}

fn test_ast(input: String, expected: ast.ASTNode) {
  let input =
    input
    |> lexer.new
    |> ast.from_lexer
    |> should.be_ok
  should.equal(input, expected)
}

pub fn ast_fails_on_invalid_input_test() {
  let check = fn(input: String, expected: error.SyntaxError) {
    lexer.new(input)
    |> ast.from_lexer
    |> should.equal(Error(expected))
  }

  check("", error.ExpectedExpressions(lexer.Location(col: 1, row: 1)))
  check(
    "@x(y)",
    error.UnexpectedToken(error.UnexpectedTokenError(
      expected: [lexer.LambdaDot],
      got: lexer.LParen,
      location: lexer.Location(col: 3, row: 1),
    )),
  )
  check("(x", error.UnclosedParen)
  check("@f.(@e.()", error.UnclosedParen)
}

pub fn ast_builds_on_valid_input_test() {
  let variable = fn(name: String) { ast.VariableNode(ast.Variable(name)) }
  let left_side = fn(name: String, body: ast.ASTNode) {
    ast.AbstractionNode(ast.Abstraction(
      bound_ident: ast.Variable(name),
      body: body,
    ))
  }

  let expected =
    left_side(
      "f",
      left_side(
        "x",
        ast.ApplicationNode(ast.Application(
          ast.VariableNode(ast.Variable("f")),
          ast.VariableNode(ast.Variable("x")),
        )),
      ),
    )

  test_ast("λf.λx.(f x)", expected)

  let expected =
    left_side("f", left_side("x", left_side("y", variable("y"))))
  test_ast("λf.λx.λy.y", expected)
}

pub fn evaluate_variables_test() {
  let variable = fn(name: String) { ast.VariableNode(ast.Variable(name)) }
  let left_side = fn(name: String, body: ast.ASTNode) {
    ast.AbstractionNode(ast.Abstraction(
      bound_ident: ast.Variable(name),
      body: body,
    ))
  }
  let application = fn(a: ast.ASTNode, b: ast.ASTNode) {
    ast.ApplicationNode(ast.Application(a, b))
  }

  let make_input = fn(input) { lambda_calc.evaluate(input) |> should.be_ok }

  let input =
    make_input(
      "id <- (@x.x)
id",
    )

  let expected = left_side("x", variable("x"))
  should.equal(input, expected)

  let input =
    make_input(
      "succ <- (@n.@f.@x.(f (n f x)))
(succ (@f.@x.(f x)))
",
    )
  let expected =
    left_side(
      "f",
      left_side(
        "x",
        application(variable("f"), application(variable("f"), variable("x"))),
      ),
    )

  should.equal(input, expected)

  let input =
    make_input(
      "succ <- (@n.@f.@x.(f (n f x)))
zero <- (@f.@x.x)
one <- (succ zero)
two <- (succ (succ zero))
(succ two)
",
    )

  let expected =
    left_side(
      "f",
      left_side(
        "x",
        application(
          variable("f"),
          application(variable("f"), application(variable("f"), variable("x"))),
        ),
      ),
    )
  should.equal(input, expected)
}

pub fn evaluate_function_without_used_variable_test() {
  test_evaluate("λf.(λx.x) y", "λx.x")
  test_evaluate("λf.λx.(f (((λf.λx.x) f) x))", "λf.λx.(f x)")
}
