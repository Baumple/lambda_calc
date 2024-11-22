import lexer.{type TokenKind}

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

pub type Error {
  UnexpectedToken(UnexpectedTokenError)
  EOFReached(expected: List(TokenKind))
  ExpectedExpressions(lexer.Location)
  InvalidToken(String, lexer.Location)
  AssignmentWithoutBoundExpression(lexer.Location)
  AssignmentWithoutBody(lexer.Location)
  UnclosedParen
}
