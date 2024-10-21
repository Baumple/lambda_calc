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
  UnclosedParen(UnclosedParenError)
  EOFReached(expected: List(TokenKind))
  Nill
}
