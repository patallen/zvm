from compiler import Compiler
from vm import VM


def repl():
    while True:
        print("> ", end="")
        cp = Compiler(input())
        chunk = cp.compile()
        vm = VM(chunk)
        print(chunk.code)
        vm.run()
        print(vm.stack.pop())


repl()
