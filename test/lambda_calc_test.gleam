import ast
import gleeunit
import gleeunit/should
import lambda_calc
import lexer

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
  // test_evaluate("(λw.\\y.\\x.(y(wyx))) (λs.(λz.z))", "λf.λx.f x")
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
    ast.AbstractionNode(ast.Abstraction(bind: ast.Variable(name), in: body))
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
