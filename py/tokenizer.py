from dataclasses import dataclass
from enum import Enum, auto


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


class State(Enum):
    START = auto()
    NUM_INT = auto()
    NUM_FLOAT = auto()


class Tokenizer:
    def __init__(self, source: str) -> None:
        self.source = source
        self.start = 0
        self.index = 0

    def make_token(self, ty: TokenType):
        self.index += 1
        str_val = self.source[self.start : self.index]
        self.start = self.index
        return Token(ty=ty, value=str_val)

    def scan_token(self) -> Token:
        token = Token(ty=TokenType.EOF)
        state = State.START

        while True:
            if state == State.START:
                if self.is_at_end():
                    break
                c = self.source[self.index]
                if c == " ":
                    self.index += 1
                    self.start = self.index
                    continue
                if c == "^":
                    return self.make_token(ty=TokenType.CARET)
                if c == "(":
                    return self.make_token(ty=TokenType.L_PAREN)
                if c == ")":
                    return self.make_token(ty=TokenType.R_PAREN)
                if c == "+":
                    return self.make_token(ty=TokenType.PLUS)
                if c == "-":
                    return self.make_token(ty=TokenType.MINUS)
                if c == "*":
                    return self.make_token(ty=TokenType.ASTERISK)
                if c == "/":
                    return self.make_token(ty=TokenType.SLASH)
                if c in "0123456789":
                    state = State.NUM_INT
                    self.index += 1
                    continue
            elif state == State.NUM_INT:
                if self.is_at_end():
                    return self.make_token(TokenType.NUMBER)
                c = self.source[self.index]
                if c == ".":
                    state = State.NUM_FLOAT
                    self.index += 1
                    continue
                if c in "0123456789":
                    self.index += 1
                    continue
                else:
                    self.index -= 1
                    return self.make_token(TokenType.NUMBER)
            elif state == State.NUM_FLOAT:
                if self.is_at_end():
                    return self.make_token(TokenType.NUMBER)
                c = self.source[self.index]
                if c in "0123456789":
                    self.index += 1
                    continue
                else:
                    self.index -= 1
                    return self.make_token(TokenType.NUMBER)
            else:
                raise Exception("UNEXPECTD STATE")
        return token

    def is_at_end(self) -> bool:
        return self.index >= len(self.source)

    def advance(self) -> str:
        c = self.source[self.index]
        self.index += 1
        return c

    def peek(self) -> str:
        return self.source[self.index]

    def dump_state(self):
        start = self.start
        index = self.index
        source = self.source
        print(f"{start=} {index=} {source=}")
