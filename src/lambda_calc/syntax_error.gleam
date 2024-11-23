import lambda_calc/lexer.{type TokenKind}

pub type UnexpectedTokenError {
  UnexpectedTokenError(
    expected: List(TokenKind),
    got: TokenKind,
    location: lexer.Location,
  )
}

pub type UnclosedParenError {
  UnclosedParenError
}

pub type SyntaxError {
  UnclosedParen
  UnexpectedToken(UnexpectedTokenError)
  ExpectedExpressions(lexer.Location)
  InvalidToken(String, lexer.Location)
  AssignmentWithoutBoundExpression(lexer.Location)
  AssignmentWithoutBody(lexer.Location)
  EOFReached(expected: List(TokenKind))
}
