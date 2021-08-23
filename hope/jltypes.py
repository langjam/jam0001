from dataclasses import dataclass
from environment import Environment

@dataclass
class Value:
    value: object = None
    _comment: object = None

    def default_comment(self):
        return JlComment("something")

    def get_comment(self):
        if self._comment is not None:
            return self._comment
        return self.default_comment()

    def set_comment(self, comment):
        self._comment = comment

    def __str__(self):
        return str(self.value)

    def __eq__(self, other):
        if type(self) != type(other):
            res = False
        else:
            res = self.value == other.value
        return JlBool(res, JlComment(f"{self.get_comment().value} is equal to {other.get_comment().value}"))

    def not_(self):
        raise TypeError()

    
@dataclass(eq=False)
class JlComment(Value):
    def default_comment(self):
        return JlComment(f"a comment")

    def __str__(self):
        return '/*' + self.value + '*/'

    def __add__(self, other):
        if not isinstance(other, JlComment):
            raise TypeError()
        return JlComment(self.value + other.value)

@dataclass(eq=False)
class JlNumber(Value):
    def default_comment(self):
        return JlComment(f"the number {self}")

    def build_comment(self, name, other):
        return JlComment(f"the {name} of {self.get_comment().value} and {other.get_comment().value}")

    def __str__(self):
        return f"{self.value:g}"
    
    def __add__(self, other):
        if not isinstance(other, JlNumber):
            raise TypeError()
        return JlNumber(self.value + other.value,
                        self.build_comment("sum", other))

    def __sub__(self, other):
        if not isinstance(other, JlNumber):
            raise TypeError()
        return JlNumber(self.value - other.value,
                        self.build_comment("difference", other))

    def __mul__(self, other):
        if not isinstance(other, JlNumber):
            raise TypeError()
        return JlNumber(self.value * other.value,
                        self.build_comment("product", other))

    def __truediv__(self, other):
        if not isinstance(other, JlNumber):
            raise TypeError()
        return JlNumber(self.value / other.value,
                        self.build_comment("quotient", other))

    def __mod__(self, other):
        if not isinstance(other, JlNumber):
            raise TypeError()
        return JlNumber(self.value % other.value,
                        self.build_comment("modulus", other))

    def __lt__(self, other):
        if not isinstance(other, JlNumber):
            raise TypeError()
        return JlBool(self.value < other.value, 
                      JlComment(f"{self.get_comment().value} is less than {other.get_comment().value}"))

    def __gt__(self, other):
        if not isinstance(other, JlNumber):
            raise TypeError()
        return JlBool(self.value > other.value, 
                      JlComment(f"{self.get_comment().value} is greater than {other.get_comment().value}"))
    
    def __neg__(self):
        return JlNumber(-self.value,
                       JlComment(f"the negative of{self.get_comment().value}"))


@dataclass(eq=False)
class JlString(Value):
    def default_comment(self):
        return JlComment(f"the string \"{self.value}\"")

    def __add__(self, other):
        if not isinstance(other, JlString):
            raise TypeError()
        return JlString(self.value + other.value,
                        JlComment(f"{self.get_comment().value} concatenated with {other.get_comment().value}"))



@dataclass(eq=False)
class JlUnit(Value):
    def default_comment(self):
        return JlComment("the unit")

    def __str__(self):
        return "()"


@dataclass(eq=False)
class JlBool(Value):
    def default_comment(self):
        return JlComment(f"the boolean \"{self.value}\"")

    def __str__(self):
        return str(self.value)

    def __and__(self, other):
        if not isinstance(other, JlBool):
            raise TypeError()
        return JlBool(self.value and other.value,
                      JlComment(f"{self.get_comment().value} and {other.get_comment().value}"))

    def __or__(self, other):
        if not isinstance(other, JlBool):
            raise TypeError()
        return JlBool(self.value or other.value,
                      JlComment(f"{self.get_comment().value} or {other.get_comment().value}"))

    def not_(self):
        return JlBool(not self.value,
                      JlComment(f"{self.get_comment().value}, not"))

class JlCallable(Value):
    pass


class JlPrimitive(JlCallable):
    def __init__(self, callback, arity=None, comment=None):
        super().__init__(None, comment)
        self.callback = callback
        self.arity = arity

    def __eq__(self, other):
        return JlBool(self is other,
                      JlComment(f"{self.get_comment().value} is equal to {other.get_comment().value}"))
    def __repr__(self):
        return f"JlPrimitive({self.get_comment()})"

    def call(self, interpreter, args):
        return self.callback(*args)

    def get_arity(self):
        return self.arity


class JlClosure(JlCallable):
    def __init__(self, environment, params, body, comment=None):
        super().__init__(None, comment)
        self.environment = environment
        self.params = params
        self.body = body

    def __str__(self):
        return "JlClosure({self.get_comment()})"

    def call(self, interpreter, args):
        env = Environment(self.environment)
        for p, a in zip(self.params, args):
            env.put(p, a, 0)
        return interpreter.eval_with_env(self.body, env)

    def get_arity(self):
        return len(self.params)


class JlList(Value):
    def default_comment(self):
        if len(self.value) == 0:
            return JlComment("an empty list")
        vals = " and ".join([v.get_comment().value for v in self.value])
        return JlComment(f"a list of {vals}")

    def __str__(self):
        vals = ", ".join(map(str, self.value))
        return "[" + vals + "]"
        
