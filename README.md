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

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```

## Roadmap
- [ ] alpha conversion
- [ ] descriptive error messages
- [ ] code clean up
- [ ] split mermaid code generator into separate module
- [x] assignments
