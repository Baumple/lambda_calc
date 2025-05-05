# lambda_calc
A (hopefully) simple evaluator for the lambda calculus with some "extras"
(mainly assignments). It currently tries to evaluate the most inner values
first before it does the outer ones.

[![Package Version](https://img.shields.io/hexpm/v/lambda_calc)](https://hex.pm/packages/lambda_calc)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/lambda_calc/)

```sh
gleam add lambda_calc
```
```gleam
import lambda_calc
import lambda_calc/lexer
import lambda_calc/ast
import result

pub fn main() {
  use ast <- result.try(
    lexer.new("(@f.@x.(f z)) y")
    |> ast.from_lexer
    |> result.map(handle_error)
  )

  let evaluated = lambda_calc.evaluate(ast) // -> @x.(y z)

  Ok(())
}
```

Further documentation can be found at <https://hexdocs.pm/lambda_calc>.

## Roadmap
- [ ] Assignments -> Expressions to variables
- [ ] More sound parser
- [ ] Step by step evaluation
- [ ] Ensure correctness of evaluation
- [ ] Mathmatical operations (+-\*/)
- [ ] Documentation
- [ ] Typed lambda calculus

## Resources:
- Lambda-Calc Papers:
  - [[https://groups.seas.harvard.edu/courses/cs152/2022sp/lectures/lec07-lambdacalc.pdf]]
  - [[https://personal.utdallas.edu/~gupta/courses/apl/lambda.pdf]]
