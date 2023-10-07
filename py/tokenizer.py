from dataclasses import dataclass
from enum import Enum


class TokenType(str, Enum):
    L_PAREN = "("
    R_PAREN = ")"
    PLUS = "+"
    MINUS = "-"
    ASTERISK = "*"
    SLASH = "/"
    NUMBER = "n"
    CARET = "^"
    EOF = "\0"


@dataclass
class Token:
    ty: TokenType
    value: int = None


class Tokenizer:
    def __init__(self, source: str) -> None:
        self.source = source
        self.tok_i = 0

    def scan_token(self) -> Token:
        token = Token(ty=TokenType.EOF)
        if self.is_at_end():
            return token

        c = self.advance()
        if c == " ":
            c = self.skip_whitespace()

        if c == "^":
            return Token(ty=TokenType.CARET)
        if c == "(":
            return Token(ty=TokenType.L_PAREN)
        if c == ")":
            return Token(ty=TokenType.R_PAREN)
        if c == "+":
            return Token(ty=TokenType.PLUS)
        if c == "-":
            return Token(ty=TokenType.MINUS)
        if c == "*":
            return Token(ty=TokenType.ASTERISK)
        if c == "/":
            return Token(ty=TokenType.SLASH)
        if c in "0123456789":
            tok = self.parse_number()
            return tok

    def skip_whitespace(self) -> str:
        c = self.peek()
        while c in "\n\t\r ":
            c = self.advance()
        print("ended on: ", c)
        return c

    def parse_number(self) -> Token:
        start = self.tok_i - 1

        while not self.is_at_end():
            c = self.peek()
            if c not in "0123456789":
                break
            c = self.advance()

        str_val = self.source[start : self.tok_i]
        tok = Token(ty=TokenType.NUMBER, value=float(str_val))
        return tok

    def is_at_end(self) -> bool:
        return self.tok_i >= len(self.source) - 1

    def advance(self) -> str:
        c = self.source[self.tok_i]
        self.tok_i += 1
        return c
    
    def peek(self) -> str:
        return self.source[self.tok_i]
