from typing import Callable
from dataclasses import dataclass
from enum import Enum, auto

from vm import Op
from tokenizer import Tokenizer, Token


class Parse:
    def __init__(self, source: str) -> None:
        self.tokenizer = Tokenizer(source)
        self.current = None
        self.previous = None

    def advance(self) -> Token:
        self.previous = self.current
        self.current = self.tokenizer.scan_token()
        print(f"current = {self.current}")
        return self.current
