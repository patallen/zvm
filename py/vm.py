import typing
from enum import Enum, auto

if typing.TYPE_CHECKING:
    from chunk import Chunk


class Op(int, Enum):
    PUSH = auto()
    POP = auto()
    NEGATE = auto()
    ADD = auto()
    SUB = auto()
    MULT = auto()
    DIV = auto()
    POW = auto()
    CONST = auto()
    CALL = auto()
    JMP = auto()
    JMPNZ = auto()
    RET = auto()


class VM:
    def __init__(self, chunk: "Chunk") -> None:
        self.chunk = chunk
        self.sp = 0
        self.ip = 0
        self.stack = []
        self.current = None

    def read_byte(self):
        byte = self.chunk.code[self.ip]
        self.ip += 1
        return byte

    def run(self):
        while self.ip < len(self.chunk.code):
            op_byte = self.read_byte()

            match Op(op_byte):
                case Op.RET:
                    self.ip = self.stack.pop()
                    break
                case Op.CONST:
                    self.stack.append(self.chunk.get_const(self.read_byte()))
                case Op.POP:
                    self.stack.pop()
                case Op.ADD:
                    a = self.stack.pop()
                    b = self.stack.pop()
                    self.stack.append(a + b)
                case Op.SUB:
                    b = self.stack.pop()
                    a = self.stack.pop()
                    self.stack.append(a - b)
                case Op.MULT:
                    b = self.stack.pop()
                    a = self.stack.pop()
                    self.stack.append(a * b)
                case Op.DIV:
                    b = self.stack.pop()
                    a = self.stack.pop()
                    self.stack.append(a / b)
                case Op.POW:
                    b = self.stack.pop()
                    a = self.stack.pop()
                    self.stack.append(a**b)
                case Op.NEGATE:
                    self.stack.append(-self.stack.pop())
                case _:
                    raise NotImplementedError(f"{Op(op_byte).name} is not implemented")
