from dataclasses import dataclass
from typing import List, Tuple
from patterns import *
from domain import *
from pyparsing import Word, Literal, Forward, Suppress, alphas, alphanums


@dataclass
class Directive:
    cmd: str
    args: List[str]


def parse_directive(src: str) -> Directive:
    src = src.strip()
    if not src.startswith("@"):
        return None

    if "(" not in src:
        return Directive(src[1:], [])

    cmd, _, rest = src.partition("(")
    cmd = cmd.strip()[1:]
    rest = rest[:-1]

    args = [arg.strip() for arg in rest.split(",")]

    return Directive(cmd, args)


param_pattern_p = filter_p(
    lambda directive: directive.cmd == "param" and len(directive.args) == 1,
    from_function_p(parse_directive),
)

loop_variant_p = map_p(
    lambda x: Directive(x.cmd, x.args),
    filter_p(
        lambda directive: directive.cmd == "loop_variant" and len(directive.args) == 4,
        from_function_p(parse_directive),
    ),
)

approx_comment_p = map_p(
    lambda x: Directive(x.cmd, [text_expr_p.match(x.args[0])]),
    filter_p(
        lambda directive: directive.cmd == "approx" and len(directive.args) == 1,
        from_function_p(parse_directive),
    ),
)

already_encountered_label_p = filter_p(
    lambda directive: directive.cmd == "already_encountered"
    and len(directive.args) == 0,
    from_function_p(parse_directive),
)


###### SExpr text parsing ######


def skip_spaces(src: str) -> str:
    return src.lstrip()


def get_token(src: str) -> Tuple[str, str]:
    src = skip_spaces(src)
    if src[0].isalpha() or src[0] == "_":
        i = 1
        while i < len(src) and (src[i].isalpha() or src[i].isdigit() or src[i] == "_"):
            i += 1
        return src[:i], src[i:]
    elif src[0].isdigit():
        i = 1
        while i < len(src) and src[i].isdigit():
            i += 1
        return src[:i], src[i:]
    else:
        return src[:1], src[1:]


def tokenize(src: str) -> List[str]:
    res = []
    while len(src) > 0:
        token, rest = get_token(src)
        res.append(token)
        src = rest
    return res


def parse_atom(tokens: List[str]) -> Tuple[Any, List[str]]:
    if tokens[0][0].isdigit():
        return SConst(tokens[0]), tokens[1:]
    elif tokens[0][0].isalpha() or tokens[0][0] == "_":
        return SVar(tokens[0]), tokens[1:]
    elif tokens[0] == "-":
        atom, rest = parse_atom(tokens[1:])
        return SUnop("-", True, atom), rest
    elif tokens[0] == "(":
        atom, rest = parse_expr(tokens[1:])
        if rest[0] != ")":
            raise ValueError("Unclosed parenthes inside expression")
        return atom, rest[1:]


OPERATORS_TABLE = [["+", "-"], ["*", "/"]]


def parse_expr(tokens: List[str], level=0) -> Tuple[Any, List[str]]:
    if level == len(OPERATORS_TABLE):
        return parse_atom(tokens)
    prev, rest = parse_expr(tokens, level + 1)
    while len(rest) > 0:
        operator = rest[0]
        if operator not in OPERATORS_TABLE[level]:
            raise ValueError(
                f"Unexpected token {rest[0]} while parsing arithmetic expression"
            )
        next, rest = parse_expr(rest[1:], level + 1)
        prev = SBinop(operator, prev, next)
    return prev, rest


class TextExprPattern(Pattern):
    def __init__(self):
        pass

    def match(self, line):
        tokens = tokenize(line)
        try:
            expr, rest = parse_expr(tokens)
            if len(rest) > 0:
                return None
            return expr
        except:
            return None


text_expr_p = TextExprPattern()
