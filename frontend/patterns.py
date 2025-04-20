# first-class pattern, .match(tree) returns None on failure

from typing import List, Any


class Pattern(object):
    def match(self, tree):
        assert False

    def match_first(self, trees):
        for tree in trees:
            if (res := self.match(tree)) is not None:
                return res
        return None

    def __or__(self, other):
        return AlternativePattern(self, other)

    def __mul__(self, other):
        return ComposePattern(self, other)


def zip_match(patterns: List[Pattern], targets: List[Any]) -> List[Any] | None:
    if len(patterns) != len(targets):
        return None
    results = [pattern.match(target) for pattern, target in zip(patterns, targets)]
    if any(result is None for result in results):
        return None
    return results


### Some high-level pattern combinators


class WildcardPattern(Pattern):
    def match(self, tree):
        return tree


class MappedPattern(Pattern):
    def __init__(self, f, base):
        self.f = f
        self.base = base

    def match(self, tree):
        result = self.base.match(tree)
        return None if result is None else self.f(result)


class ComposePattern(Pattern):
    def __init__(self, outer, inner):
        self.outer = outer
        self.inner = inner

    def match(self, tree):
        inner_result = self.inner.match(tree)
        if inner_result is None:
            return None
        return self.outer.match(inner_result)


class AlternativePattern(Pattern):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def match(self, tree):
        left_result = self.left.match(tree)
        if not left_result is None:
            return left_result
        return self.right.match(tree)


class WrapperPattern(Pattern):
    def __init__(self, f):
        self.f = f

    def match(self, tree):
        return self.f(tree)


class ConditionPattern(Pattern):
    def __init__(self, predicate, base):
        self.base = base
        self.predicate = predicate

    def match(self, tree):
        result = self.base.match(tree)
        if result is None or self.predicate(result):
            return result
        return None


class FixpointPattern(Pattern):
    """
    Python forbids constructions like `x = f(x)`, so we can't create recursive pattern explicitly
    FixpointPattern is wrapper, which make it possible to write such patterns in fixpoint programming style
    """

    def __init__(self, step):
        self.step = step

    def match(self, tree):
        return self.step(self).match(tree)


wildcard_p = WildcardPattern()
filter_p = lambda pred, base: ConditionPattern(pred, base)
satisfy_p = lambda pred: filter_p(pred, wildcard_p)
from_function_p = lambda f: WrapperPattern(f)
map_p = lambda f, base: MappedPattern(f, base)
fix_p = lambda pat_gen: FixpointPattern(pat_gen)


if __name__ == "__main__":
    pass
