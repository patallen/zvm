from typing import Callable
from dataclasses import dataclass
from enum import Enum, auto

from vm import Op
from tokenizer import Tokenizer, Token, TokenType


class ParseError:
    def __init__(self, token: Token, message: str):
        self.token = token
        self.message = message

    def __str__(self):
        if self.token.ty == TokenType.EOF:
            at_text = "at end"
        else:
            at_text = f"at '{self.token.value}'"
        return f"[line {self.token.line}] ERROR: {self.message} {at_text}"


class Parse:
    def __init__(self, source: str) -> None:
        self.tokenizer = Tokenizer(source)
        self.current = None
        self.previous = None
        self.errors = []
        self.has_error = False
        self.panic_mode = False

    def advance(self) -> Token:
        self.previous = self.current
        self.current = self.tokenizer.scan_token()
        return self.current

    def error_at_current(self, message: str) -> None:
        self.error_at(self.current, message)

    def error_at(self, token: Token, message: str) -> None:
        error = ParseError(token, message)
        if not self.panic_mode:
            print(error)

        self.errors.append(error)
        self.has_error = True
        self.panic_mode = True
