from typing import Any, List
from dataclasses import dataclass


class FunctionDescriprtion:
    def __init__(self, name, header, args, body, corresponding_comment=None):
        self.name = name
        self.header = header
        self.args = {arg.spelling: arg.type.spelling for arg in args}
        self.body = body
        self.comment = corresponding_comment


def pretty_ratio(n: int) -> str:
    numerator = n if n >= 0 else f"({n})"
    return f"(OCoeff ({numerator} % 1))"


@dataclass
class NamedFunctionCall:
    function_name: str
    args: List[Any]

    def pretty(self) -> str:
        return ""


@dataclass
class SillyBinop:
    operator: str
    lhs: any
    rhs: any

    def pretty(self) -> str:
        match self.operator:
            case "+":
                return f"(OSum {self.lhs.pretty()} {self.rhs.pretty()})"
            case "*":
                return f"(OProd {self.lhs.pretty()} {self.rhs.pretty()})"
            case "-":
                return SillyBinop(
                    "+", self.lhs, SillyUnop("-", True, self.rhs)
                ).pretty()
            case _:
                raise ValueError("Unsupported operator")

    def __str__(self):
        return f"({str(self.lhs)}{self.operator}{str(self.rhs)})"


@dataclass
class SillyUnop:
    operator: str
    prefix: bool  # Avoid boolean blindness
    operand: Any

    def pretty(self) -> str:
        if self.prefix and self.operator == "-":
            return f"(OProd {pretty_ratio(-1)} {self.operand.pretty()})"

    def __str__(self):
        if self.prefix:
            return f"({self.operator}{str(self.operand)})"
        else:
            return f"({str(self.operand)}{self.operator})"


@dataclass
class SillyVar:
    name: str

    def pretty(self) -> str:
        return f'(OVar "{self.name}")'

    def __str__(self):
        return self.name


@dataclass
class SillyConst:
    value: str

    def as_int(self):
        if self.value.startswith("0x"):
            return int(self.value, base=16)
        elif self.value.startswith("0"):
            return int(self.value, base=8)
        else:
            return int(self.value)

    def pretty(self):
        return pretty_ratio(self.as_int())

    def __str__(self):
        return self.value


@dataclass
class SillyCounterFor:
    counter: str
    intital: Any
    final: Any
    counter_increase: Any
    body: Any

    def pretty(self) -> str:
        return f'(SCounterFor "{self.counter}" {self.intital.pretty()} {self.final.pretty()} {self.body.pretty()})'


@dataclass
class SillyIf:
    condition: Any
    then_branch: Any
    else_branch: Any | None

    def pretty(self) -> str:
        left = self.then_branch.pretty()
        right = "SSkip" if self.else_branch is None else self.else_branch.pretty()

        return f"(SIf {left} {right})"


@dataclass
class SillyExpr:
    pass

    def pretty(self) -> str:
        return "SAtom"


@dataclass
class SillyBlock:
    children: List[Any]

    def pretty(self) -> str:
        children = [c.pretty() for c in self.children]
        return f"(SBlock [{', '.join(children)}])"
