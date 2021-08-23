from subprocess import Popen, PIPE

def run_tests():
    release_binary = "./target/release/regretti"

    tests = [
        [release_binary, "tests/basic.reg"],
        [release_binary, "tests/math.reg"],
        [release_binary, "tests/assign.reg"],
        [release_binary, "tests/prints.reg"],
        [release_binary, "tests/shell_escape.reg"],
        [release_binary, "tests/hugealloc.reg"],
    ]

    for test in tests:
        p = Popen(test, stdout=PIPE)
        p.communicate()[0]
        if p.returncode == 0:
            print("[✔] Test `{}` passed".format(test[1]))
        else:
            print("[✖] Test `{}` failed".format(test[1]))

if __name__ == '__main__':
    run_tests()