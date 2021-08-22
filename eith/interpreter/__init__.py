from typing import List, Tuple
from .comment import CommentInterpreterMixin
from .core import CoreInterpreterMixin


class Interpreter(CommentInterpreterMixin, CoreInterpreterMixin):
    """Top-level Eith interpreter.
    """

    def __init__(self, file):
        self.file = file
        self.stack = []
        self.pc = 0
        self.has_more = True
        self.loop_idx = None
        self.raw_lines = [l.strip() for l in self.file.readlines()]

    def run(self):
        """Run program.
        """

        while self.has_more:
            self.step()
        self.file.seek(0)
        self.file.write('\n'.join(self.raw_lines)+'\n')
        self.file.truncate()

    def step(self):
        """Steps one instruction.
        """

        # Ignore on EOF
        if self.pc >= len(self.raw_lines):
            self.has_more = False
            return

        raw_line = self.raw_lines[self.pc]
        line = raw_line.upper()
        tokens = line.split()

        self.interp_tokens(tokens)

        self.pc += 1

    def interp_tokens(self, tokens: List[str]):
        """Interprets line of tokens.
        """

        if len(tokens) == 0:
            return

        if tokens[0] == '\\':
            # Comment, no-op
            pass
        elif tokens[0] == 'IF':
            # IF ... (ELSE ...) THEN
            end_idx = self.__find_end_idx(tokens, 0, 'IF', 'THEN')
            self.interp_if(tokens[:end_idx])
            self.interp_tokens(tokens[end_idx:])
        elif tokens[0] == 'DO':
            # DO ... LOOP
            end_idx = self.__find_end_idx(tokens, 0, 'DO', 'LOOP')
            self.interp_do_loop(tokens[:end_idx])
            self.interp_tokens(tokens[end_idx:])
        elif tokens[0] == 'BEGIN':
            # BEGIN ... UNTIL
            end_idx = self.__find_end_idx(tokens, 0, 'BEGIN', 'UNTIL')
            self.interp_begin_until(tokens[:end_idx])
            self.interp_tokens(tokens[end_idx:])
        elif tokens[0] == '."':
            # Print string
            end_idx = 1
            while end_idx < len(tokens):
                if tokens[end_idx][-1] == '"':
                    break
                end_idx += 1
            end_idx += 1
            self.interp_dot_string(tokens[:end_idx])
            self.interp_tokens(tokens[end_idx:])
        else:
            # Single token
            self.interp_single_token(tokens[0])
            self.interp_tokens(tokens[1:])

    def interp_single_token(self, token: str):
        """Interprets single token.
        """

        # === Comment words ===
        if token == 'IS-COMMENT':
            self.interp_is_comment()
        elif token == 'IS-COMMENT-RANGE':
            self.interp_is_comment_range()
        elif token == 'READ-COMMENT':
            self.interp_read_comment()
        elif token == 'COMMENT':
            self.interp_comment()
        elif token == 'UNCOMMENT':
            self.interp_uncomment()
        elif token == 'TOGGLE-COMMENT':
            self.interp_toggle_comment()
        elif token == 'COMMENT-RANGE':
            self.interp_comment_range()
        elif token == 'UNCOMMENT-RANGE':
            self.interp_uncomment_range()
        elif token == 'TOGGLE-COMMENT-RANGE':
            self.interp_toggle_comment_range()

        # === Core words ===
        elif token == '*':
            self.interp_mult()
        elif token == '*/':
            self.interp_mult_div()
        elif token == '*/MOD':
            self.interp_mult_div_mod()
        elif token == '+':
            self.interp_add()
        elif token == '-':
            self.interp_sub()
        elif token == '.':
            self.interp_dot()
        elif token == '/':
            self.interp_div()
        elif token == '/MOD':
            self.interp_div_mod()
        elif token == '0<':
            self.interp_zero_lt()
        elif token == '0=':
            self.interp_zero_eq()
        elif token == '1+':
            self.interp_one_plus()
        elif token == '1-':
            self.interp_one_minus()
        elif token == '2*':
            self.interp_two_mult()
        elif token == '2/':
            self.interp_two_div()
        elif token == '2DROP':
            self.interp_two_drop()
        elif token == '2DUP':
            self.interp_two_dup()
        elif token == '2OVER':
            self.interp_two_over()
        elif token == '2SWAP':
            self.interp_two_swap()
        elif token == '<':
            self.interp_lt()
        elif token == '=':
            self.interp_eq()
        elif token == '>':
            self.interp_gt()
        elif token == '?DUP':
            self.interp_if_dup()
        elif token == "ABS":
            self.interp_abs()
        elif token == 'AND':
            self.interp_and()
        elif token == 'BL':
            self.interp_bl()
        elif token == 'CR':
            self.interp_cr()
        elif token == 'DEPTH':
            self.interp_depth()
        elif token == 'DROP':
            self.interp_drop()
        elif token == 'DUP':
            self.interp_dup()
        elif token == 'EMIT':
            self.interp_emit()
        elif token == 'KEY':
            self.interp_key()
        elif token == 'I':
            self.interp_i()
        elif token == 'INVERT':
            self.interp_invert()
        elif token == 'J':
            self.interp_j()
        elif token == 'LSHIFT':
            self.interp_lshift()
        elif token == 'MAX':
            self.interp_max()
        elif token == 'MIN':
            self.interp_min()
        elif token == 'MOD':
            self.interp_mod()
        elif token == 'NEGATE':
            self.interp_negate()
        elif token == 'OR':
            self.interp_or()
        elif token == 'OVER':
            self.interp_over()
        elif token == 'ROT':
            self.interp_rot()
        elif token == 'RSHIFT':
            self.interp_rshift()
        elif token == 'SPACE':
            self.interp_space()
        elif token == 'SPACES':
            self.interp_spaces()
        elif token == 'SWAP':
            self.interp_swap()
        elif token == 'XOR':
            self.interp_xor()
        elif token.isdigit() or len(token) >= 2 and token[0] == '-' and token[1:].isdigit():
            self.interp_number(token)
        else:
            raise RuntimeError(f'unexpected symbol "{token}"')

    def __find_end_idx(self, tokens: List[str], idx: int, start_token: str, end_token: str) -> int:
        counter = 1
        while counter != 0 and idx < len(tokens) - 1:
            idx += 1
            if tokens[idx] == start_token:
                counter += 1
            elif tokens[idx] == end_token:
                counter -= 1
        return idx + 1
