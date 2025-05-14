from typing import Any, List
from dataclasses import dataclass
from fractions import Fraction
import functools
import operator
import copy


class FunctionDescriprtion:
    def __init__(self, name, header, args, body, corresponding_comment=None):
        self.name = name
        self.header = header
        self.args = {arg.spelling: (arg.type.spelling, i) for i, arg in enumerate(args)}
        self.body = body
        self.parameter_name = None
        self.cfg = None
        self.comment = corresponding_comment

    def parameter_idx(self):
        if self.parameter_name is None or self.parameter_name not in self.args:
            raise ValueError(
                f"Function {self.name} has no parameter or wrong parameter declaration"
            )
        return self.args[self.parameter_name][1]


def pretty_ratio(n: int, d: int = 1) -> str:
    numerator = n if n >= 0 else f"({n})"
    assert d > 0
    return f"(OCoeff ({numerator} % {d}))"


@dataclass
class NamedFunctionCall:
    # function_name: str
    function_name: str
    args: List[Any]
    function_desc: FunctionDescriprtion | None

    def cata(self, algebra):
        return algebra[NamedFunctionCall](self)

    def pretty(self) -> str:
        if self.function_desc is None:
            raise ValueError(f"Call to unknown function {self.function_name}")

        arg = next(arg for arg in self.args if arg != 1)
        return f'(SCall "{self.function_name}" {arg.pretty()})'

    def fold_to_const(self) -> Fraction | None:
        return None


@dataclass
class SBinop:
    operator: str
    lhs: any
    rhs: any

    def fold_to_const(self) -> Fraction | None:
        if (lhs := self.lhs.fold_to_const()) is not None and (
            rhs := self.lhs.fold_to_const()
        ):
            match self.operator:
                case "+":
                    return lhs + rhs
                case "*":
                    return lhs * rhs
                case "-":
                    return lhs - rhs
                case "/":
                    return lhs / rhs
                case "%":
                    return lhs % rhs
                case _:
                    return None

    def pretty(self) -> str:
        match self.operator:
            case "+":
                return f"(OSum {self.lhs.pretty()} {self.rhs.pretty()})"
            case "*":
                return f"(OProd {self.lhs.pretty()} {self.rhs.pretty()})"
            case "-":
                return SBinop("+", self.lhs, SUnop("-", True, self.rhs)).pretty()
            case "/":
                if (folded_rhs := self.rhs.fold_to_const()) is not None:
                    if folded_rhs == 0:
                        return pretty_ratio(0)
                    inverted: Fraction = 1 / folded_rhs
                    return f"(OProd {pretty_ratio(inverted.numerator, inverted.denominator)} {self.lhs.pretty()})"
            case _:
                raise ValueError(f"Unsupported operator {self.operator}")

    def __str__(self):
        return f"({str(self.lhs)}{self.operator}{str(self.rhs)})"


@dataclass
class SUnop:
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

    def fold_to_const(self) -> Fraction | None:
        if (
            self.prefix
            and self.operator == "-"
            and (operand := self.operand.fold_to_const()) is not None
        ):
            return -operand
        else:
            return None


@dataclass
class SVar:
    name: str

    def pretty(self) -> str:
        return f'(OVar "{self.name}")'

    def __str__(self):
        return self.name

    def fold_to_const(self) -> Fraction | None:
        return None


@dataclass
class SConst:
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

    def fold_to_const(self) -> Fraction | None:
        return Fraction(self.value)


@dataclass
class SCounterFor:
    counter: str
    intital: Any
    final: Any
    counter_increase: Any
    body: Any

    def cata(self, algebra):
        copied = copy.copy(self)
        copied.body = self.body.cata(algebra)
        return algebra[SCounterFor](copied)

    def pretty(self) -> str:
        match self.counter_increase:
            case "++":
                return f'(SCounterFor "{self.counter}" {self.intital.pretty()} {self.final.pretty()} OIncrenmentCounter {self.body.pretty()})'
            case "--":
                return f'(SCounterFor "{self.counter}" {self.final.pretty()} {self.intital.pretty()} OIncrenmentCounter {self.body.pretty()})'
            case _:
                raise ValueError(
                    "Found cycle for with unknwon counter mutation function"
                )


@dataclass
class SIf:
    condition: Any
    then_branch: Any
    else_branch: Any | None

    def cata(self, algebra):
        copied = copy.copy(self)
        copied.then_branch = self.then_branch.cata(algebra)
        copied.else_branch = (
            None if self.else_branch is None else self.else_branch.cata(algebra)
        )
        return algebra[SIf](copied)

    def pretty(self) -> str:
        left = self.then_branch.pretty()
        right = "SSkip" if self.else_branch is None else self.else_branch.pretty()

        return f"(SIf {left} {right})"


@dataclass
class SExpr:
    def __init__(self):
        pass

    def cata(self, algebra):
        return algebra[SExpr](self)

    def pretty(self) -> str:
        return "SAtom"


@dataclass
class SSkip:
    def __init__(self):
        pass

    def cata(self, algebra):
        return algebra[SSkip](self)

    def pretty(self) -> str:
        return "SSkip"


@dataclass
class SBlock:
    children: List[Any]

    def cata(self, algebra):
        copied = SBlock([child.cata(algebra) for child in self.children])
        return algebra[SBlock](copied)

    def pretty(self) -> str:
        children = [c.pretty() for c in self.children]
        return f"(SBlock [{', '.join(children)}])"


def cfg_collect_calls(stmt):
    return stmt.cata(
        {
            NamedFunctionCall: lambda fc: {fc.function_name},
            SExpr: lambda sexpr: set(),
            SSkip: lambda sskip: set(),
            SBlock: lambda sblock: functools.reduce(
                operator.or_, sblock.children, set()
            ),
            SCounterFor: lambda scf: scf.body,
            SIf: lambda sif: sif.then_branch
            | (set() if sif.else_branch is None else sif.else_branch),
        }
    )
