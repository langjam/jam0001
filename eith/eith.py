#!/usr/bin/env python
import argparse
from interpreter import Interpreter


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Interpret Eith programs.')
    parser.add_argument(
        'input', help='Path to Eith source file.')

    args = parser.parse_args()

    with open(args.input, 'r+') as f:
        Interpreter(f).run()
