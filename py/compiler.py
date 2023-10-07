from dataclasses import dataclass
from chunk import Chunk
from tokenizer import TokenType
from vm import Op
from parse import Parse


@dataclass
class OpInfo:
    prec: int
    assoc: str


PRECEDENCES = {
    TokenType.PLUS: OpInfo(1, "l"),
    TokenType.MINUS: OpInfo(1, "l"),
    TokenType.ASTERISK: OpInfo(2, "l"),
    TokenType.SLASH: OpInfo(2, "l"),
    TokenType.CARET: OpInfo(3, "r"),
}


class Compiler:
    def __init__(self, source: str):
        self.parse = Parse(source)
        self.chunk = Chunk(source)

    def load_chunk(self, chunk: Chunk):
        self.chunk = chunk

    def compile(self) -> Chunk:
        self.parse.advance()
        self.compute_expression(1)
        return self.chunk

    def compute_expression(self, min_prec: int):
        if self.parse.current.ty == TokenType.EOF:
            return

        if self.parse.current.ty == TokenType.INVALID:
            self.parse.error_at_current("Invalid token")
            return

        lhs = self.compute_atom()
        while True:
            current = self.parse.current
            if (
                current.ty == TokenType.EOF
                or current.ty not in PRECEDENCES
                or PRECEDENCES[current.ty].prec < min_prec
            ):
                break

            assert current.ty in PRECEDENCES

            info = PRECEDENCES[current.ty]
            next_min_prec = info.prec + 1 if info.assoc == "l" else info.prec

            self.parse.advance()
            rhs = self.compute_expression(next_min_prec)

            lhs = self.compute_op(current.ty, lhs, rhs)
        return

    def compute_atom(self):
        current = self.parse.current
        if current.ty == TokenType.MINUS:
            self.parse.advance()
            self.compute_expression(1)
            self.chunk.write_byte(Op.NEGATE)
            return
        if current.ty == TokenType.L_PAREN:
            self.parse.advance()
            self.compute_expression(1)
            if self.parse.current.ty != TokenType.R_PAREN:
                self.parse.error_at_current(f"Expected closing paren")
            self.parse.advance()
            return

        if current.ty == TokenType.R_PAREN:
            self.parse.error_at_current(f"Invalid token")

        if current.ty == TokenType.EOF:
            self.parse.error_at_current("Source ended unexpectedly.")

        if current.ty in PRECEDENCES:
            self.parse.error_at_current(f"Expected an atom")

        if current.ty == TokenType.NUMBER:
            self.parse.advance()
            index = self.chunk.add_const(float(current.value))
            self.chunk.write_byte(Op.CONST)
            self.chunk.write_byte(index)

    def compute_op(self, op, lhs, rhs):
        op_val = None
        match op:
            case TokenType.ASTERISK:
                op_val = Op.MULT
            case TokenType.SLASH:
                op_val = Op.DIV
            case TokenType.PLUS:
                op_val = Op.ADD
            case TokenType.MINUS:
                op_val = Op.SUB
            case TokenType.CARET:
                op_val = Op.POW
            case _:
                raise Exception(f"{op} is not an arithmetic op")
        self.chunk.write_byte(op_val)


if __name__ == "__main__":
    cp = Compiler("1 + 2 * 3 + 2 / 12")
    cp.compile()
