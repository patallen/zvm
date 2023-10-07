from typing import Callable
from dataclasses import dataclass
from enum import Enum, auto

from vm import Op
from tokenizer import Tokenizer, TokenType, Token


@dataclass
class OpInfo:
    prec: int
    assoc: str
    func: Callable[..., int]


def add(a, b):
    return a + b


def sub(a, b):
    return a - b


def mult(a, b):
    return a * b


def div(a, b):
    return a / b


def power(a, b):
    return a**b


PRECEDENCES = {
    TokenType.PLUS: OpInfo(1, "l", add),
    TokenType.MINUS: OpInfo(1, "l", sub),
    TokenType.ASTERISK: OpInfo(2, "l", mult),
    TokenType.SLASH: OpInfo(2, "l", div),
    TokenType.CARET: OpInfo(3, "r", power),
}


@dataclass
class Expr:
    def __init__(self, lhs, rhs, op):
        self.lhs = lhs
        self.rhs = rhs
        self.op = op

    def __str__(self):
        return f"({self.op.value} {self.lhs} {self.rhs})"

    def eval(self):
        try:
            lhs = self.lhs.eval()
        except AttributeError:
            lhs = self.lhs

        try:
            rhs = self.rhs.eval()
        except AttributeError:
            rhs = self.rhs

        print(lhs, rhs)
        return PRECEDENCES[self.op].func(lhs, rhs)

    def print_code(self):
        try:
            lhs = self.lhs.print_code()
        except AttributeError:
            lhs = self.lhs
            print(f"{lhs}", end=" ")

        try:
            rhs = self.rhs.print_code()
        except AttributeError:
            rhs = self.rhs
            print(f"{rhs}", end=" ")

        print(self.op.value, end=" ")

    def is_expr(self):
        return True


class Parser:
    def __init__(self, tokenizer: Tokenizer) -> None:
        self.tokenizer = tokenizer
        self.current = None
        self.previous = None
        self.bytecode = []

    def advance(self) -> Token:
        self.previous = self.current
        self.current = self.tokenizer.scan_token()
        return self.current

    def compute_expression(self, min_prec: int) -> Expr:
        lhs = self.compute_atom()
        while True:
            current = self.current
            if (
                current.ty == TokenType.EOF
                or current.ty not in PRECEDENCES
                or PRECEDENCES[current.ty].prec < min_prec
            ):
                break

            assert current.ty in PRECEDENCES

            info = PRECEDENCES[current.ty]
            next_min_prec = info.prec + 1 if info.assoc == "l" else info.prec

            self.advance()
            rhs = self.compute_expression(next_min_prec)

            lhs = self.compute_op(current.ty, lhs, rhs)
        return lhs

    def compute_atom(self):
        tok = self.current
        if tok.ty == TokenType.L_PAREN:
            expr = self.compute_expression(1)
            if self.current.ty != TokenType.R_PAREN:
                raise Exception("expected closing paren")
            self.advance()
            return expr

        if tok.ty == TokenType.EOF:
            raise Exception("Source ended unexpectedly")

        if tok.ty in PRECEDENCES:
            raise Exception(f"Expected an atom, got: {tok.ty}")

        if tok.ty == TokenType.NUMBER:
            self.advance()
            self.bytecode.extend([Op.PUSH, tok.value])
            return tok.value

    def compute_op(self, op, lhs, rhs):
        self.bytecode.append(op)
        return Expr(lhs, rhs, op)

    def parse(self) -> Expr:
        self.advance()
        expr = self.compute_expression(1)
        self.bytecode.append(Op.RET)
        return expr


if __name__ == "__main__":
    tokenizer = Tokenizer("1 + 2 * 3 - 2 / 2 ^ 2")
    parser = Parser(tokenizer)
    parser.advance()
    res = parser.parse()
    print(parser.bytecode)
