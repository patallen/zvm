from dataclasses import dataclass
from vm import Op

Number = int | float


@dataclass
class Chunk:
    source: str
    code: list[int]
    constants: list[float]

    def __init__(self, source, code=None, constants=None):
        self.source = source
        self.code = code or []
        self.constants = constants or []

    def add_const(self, value: Number) -> int:
        self.constants.append(value)
        return len(self.constants) - 1

    def get_const(self, index: int) -> Number:
        return self.constants[index]

    def write_byte(self, byte: int) -> None:
        self.code.append(byte)

    def read_byte(self, index: int) -> int:
        return self.code[index]

    def read_op(self, index: int) -> Op:
        return Op(self.read_byte(index))
