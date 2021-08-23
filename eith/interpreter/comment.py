from .errors import NotCommentException


class CommentInterpreterMixin:
    """Eith comment word interpreter mixin.
    """

    def interp_is_comment(self):
        idx = self.stack.pop() - 1
        self.stack.append(-1 if self.raw_lines[idx][:2] == '\ ' else 0)

    def interp_is_comment_range(self):
        b = self.stack.pop() - 1
        a = self.stack.pop() - 1
        is_comment = True
        for i in range(a, b):
            is_comment = is_comment and self.raw_lines[i][:2] == '\ '
        self.stack.append(-1 if is_comment else 0)

    def interp_read_comment(self):
        idx = self.stack.pop() - 1
        if self.raw_lines[idx][:2] == '\ ':
            a = int(self.raw_lines[idx][2:])
            self.stack.append(a)
        else:
            raise NotCommentException

    def interp_comment(self):
        idx = self.stack.pop() - 1
        self.raw_lines[idx] = f'\ {self.raw_lines[idx]}'

    def interp_uncomment(self):
        idx = self.stack.pop() - 1
        if self.raw_lines[idx][:2] == '\ ':
            self.raw_lines[idx] = self.raw_lines[idx][2:]
        else:
            raise NotCommentException

    def interp_toggle_comment(self):
        idx = self.stack.pop() - 1
        if self.raw_lines[idx][:2] == '\ ':
            self.raw_lines[idx] = self.raw_lines[idx][2:]
        else:
            self.raw_lines[idx] = f'\ {self.raw_lines[idx]}'

    def interp_comment_range(self):
        b = self.stack.pop() - 1
        a = self.stack.pop() - 1
        for i in range(a, b):
            self.raw_lines[i] = f'\ {self.raw_lines[i]}'

    def interp_uncomment_range(self):
        b = self.stack.pop() - 1
        a = self.stack.pop() - 1
        for i in range(a, b):
            if self.raw_lines[i][:2] == '\ ':
                self.raw_lines[i] = self.raw_lines[i][2:]
            else:
                raise NotCommentException

    def toggle_comment_range(self):
        b = self.stack.pop() - 1
        a = self.stack.pop() - 1
        for i in range(a, b):
            if self.raw_lines[i][:2] == '\ ':
                self.raw_lines[i] = self.raw_lines[i][2:]
            else:
                self.raw_lines[i] = f'\ {self.raw_lines[i]}'
