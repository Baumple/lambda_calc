import ast
import gleam/io
import gleeunit
import gleeunit/should
import lambda_calc
import lexer
import pprint

pub fn main() {
  gleeunit.main()
}

fn test_evaluate(input: String, expected: String) {
  let ast_node =
    lexer.new(input)
    |> ast.from_lexer
    |> should.be_ok

  let expected_ast =
    lexer.new(expected)
    |> ast.from_lexer
    |> should.be_ok

  lambda_calc.evaluate(ast_node)
  |> should.equal(expected_ast)
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

pub fn ast_test() {
  let variable = fn(name: String) { ast.VariableNode(ast.Variable(name)) }
  let abstraction = fn(name: String, body: ast.ASTNode) {
    ast.AbstractionNode(ast.Abstraction(
      bound_ident: ast.Variable(name),
      body: body,
    ))
  }

  let expected =
    abstraction(
      "f",
      abstraction(
        "x",
        ast.ApplicationNode(ast.Application(
          ast.VariableNode(ast.Variable("f")),
          ast.VariableNode(ast.Variable("x")),
        )),
      ),
    )

  test_ast("λf.λx.(f x)", expected)

  let expected =
    abstraction("f", abstraction("x", abstraction("y", variable("y"))))
  test_ast("λf.λx.λy.y", expected)
}

pub fn evaluate_function_without_used_variable_test() {
  test_evaluate("λf.(λx.x) y", "λx.x")
  test_evaluate("λf.λx.(f (((λf.λx.x) f) x))", "λf.λx.(f x)")
}
