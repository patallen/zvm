import readline
from compiler import Compiler
from vm import VM


def repl():
    readline.set_pre_input_hook(readline.redisplay)

    while True:
        cp = Compiler(input("> "))
        chunk = cp.compile()
        if cp.parse.has_error:
            continue
        vm = VM(chunk)
        vm.run()
        if vm.stack:
            print(vm.stack.pop())


try:
    repl()
except KeyboardInterrupt:
    exit(0)
