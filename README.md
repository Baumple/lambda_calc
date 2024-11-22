# lambda_calc

[![Package Version](https://img.shields.io/hexpm/v/lambda_calc)](https://hex.pm/packages/lambda_calc)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/lambda_calc/)

```sh
gleam add lambda_calc
```
```gleam
import lambda_calc
import lambda_calc/lexer
import lambda_calc/ast

pub fn main() {
  let evaluated =
    lexer.new("(@f.@x.(f z)) y")
    |> ast.from_lexer
    |> result.unwrap_lazy(or: fn() { panic })
    |> lambda_calc.evaluate // -> @x.(y z)
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
