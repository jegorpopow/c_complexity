from dataclasses import dataclass
from typing import List


@dataclass
class Directive:
    cmd: str
    args: List[str]


def parse_directive(src: str) -> Directive:
    src = src.strip()
    if not src.startswith("@"):
        return None

    cmd, _, rest = src.partition("(")
    cmd = cmd.strip()[1:]
    rest = rest[:-1]

    args = [arg.strip() for arg in rest.split(",")]

    return Directive(cmd, args)
