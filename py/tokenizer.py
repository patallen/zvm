from dataclasses import dataclass
from enum import Enum, auto


class TokenType(str, Enum):
    KW_NULL = "n"
    KW_FALSE = "f"
    KW_TRUE = "t"
    IDENT = "i"
    NUMBER = "#"
    STRING = "s"
    INVALID = "X"
    L_PAREN = "("
    R_PAREN = ")"
    PLUS = "+"
    MINUS = "-"
    SLASH = "/"
    STAR = "*"
    STAR_STAR = "**"
    BANG = "!"
    BANG_EQ = "!="
    EQ = "="
    EQ_EQ = "=="
    LT = "<"
    LT_EQ = "<="
    GT = ">"
    GT_EQ = ">="
    EOF = "\0"


@dataclass
class Token:
    ty: TokenType
    line: int
    start: int
    value: int = None


class State(Enum):
    EQUALS = auto()
    BANG = auto()
    LESS = auto()
    GREATER = auto()
    IDENT = auto()
    START = auto()
    STAR = auto()
    NUM_INT = auto()
    NUM_FLOAT = auto()


KEYWORDS = {
    "true": TokenType.KW_TRUE,
    "false": TokenType.KW_FALSE,
    "null": TokenType.KW_NULL,
}


class Tokenizer:
    def __init__(self, source: str) -> None:
        self.source = source
        self.start = 0
        self.index = 0
        self.line = 1

    def make_token(self, ty: TokenType):
        self.index += 1
        str_val = self.source[self.start : self.index]
        token = Token(ty=ty, value=str_val, start=self.start, line=self.line)
        self.start = self.index
        return token

    def make_ident(self):
        ident = self.source[self.start : self.index]
        if op := KEYWORDS.get(ident):
            return self.make_token(ty=op)

        return self.make_token(TokenType.IDENT)

    def scan_token(self) -> Token:
        token = Token(ty=TokenType.EOF, line=self.line, start=self.start)
        state = State.START

        while True:
            if state == State.START:
                if self.is_at_end():
                    break
                c = self.source[self.index]
                if c in " \t\r":
                    self.index += 1
                    self.start = self.index
                    continue
                if c in "\n":
                    self.index += 1
                    self.line += 1
                    self.start = self.index
                    continue
                if c.isalpha():
                    state = State.IDENT
                    self.index += 1
                    continue
                if c == "(":
                    return self.make_token(ty=TokenType.L_PAREN)
                if c == ")":
                    return self.make_token(ty=TokenType.R_PAREN)
                if c == "+":
                    return self.make_token(ty=TokenType.PLUS)
                if c == "-":
                    return self.make_token(ty=TokenType.MINUS)
                if c == "/":
                    return self.make_token(ty=TokenType.SLASH)
                if c == "!":
                    state = State.BANG
                    self.index += 1
                    continue
                if c == "<":
                    state = State.LESS
                    self.index += 1
                    continue
                if c == ">":
                    state = State.GREATER
                    self.index += 1
                    continue
                if c == "=":
                    state = State.EQUALS
                    self.index += 1
                    continue
                if c == "*":
                    state = State.STAR
                    self.index += 1
                    continue
                if c in "0123456789":
                    state = State.NUM_INT
                    self.index += 1
                    continue
                else:
                    return self.make_token(ty=TokenType.INVALID)
            elif state == State.NUM_INT:
                if self.is_at_end():
                    return self.make_token(ty=TokenType.NUMBER)
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
                    return self.make_token(ty=TokenType.NUMBER)
            elif state == State.NUM_FLOAT:
                if self.is_at_end():
                    return self.make_token(ty=TokenType.NUMBER)
                c = self.source[self.index]
                if c in "0123456789":
                    self.index += 1
                    continue
                else:
                    self.index -= 1
                    return self.make_token(ty=TokenType.NUMBER)
            elif state == State.BANG:
                return self.multi_inst("=", TokenType.BANG, TokenType.BANG_EQ)
            elif state == State.GREATER:
                return self.multi_inst("=", TokenType.GT, TokenType.GT_EQ)
            elif state == State.EQUALS:
                return self.multi_inst("=", TokenType.EQ, TokenType.EQ_EQ)
            elif state == State.LESS:
                return self.multi_inst("=", TokenType.LT, TokenType.LT_EQ)
            elif state == State.STAR:
                return self.multi_inst("*", TokenType.STAR, TokenType.STAR_STAR)
            elif state == State.IDENT:
                if self.is_at_end():
                    return self.make_ident()
                c = self.source[self.index]
                if c.isalnum():
                    self.index += 1
                    continue
                else:
                    self.index -= 1
                    return self.make_ident()
            else:
                raise Exception(f"UNEXPECTD STATE: {state}")
        return token

    def multi_inst(self, char: str, base: TokenType, expanded: TokenType) -> Token:
        if self.is_at_end():
            return self.make_token(ty=base)
        c = self.source[self.index]
        if c == char:
            return self.make_token(ty=expanded)
        else:
            self.index -= 1
            return self.make_token(ty=base)

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
