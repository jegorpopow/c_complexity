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


def collect_matches(pattern: Pattern, traverse_apply_func, target):
    collected = []
    traverse_apply_func(pattern, lambda matched: collected.append(matched), target)
    return collected


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


class AllPattern(Pattern):
    def __init__(self, patterns):
        self.patterns = patterns

    def match(self, tree):
        matches = [p.match(tree) for p in self.patterns]
        if any(m is None for m in matches):
            return None
        else:
            return matches


class BindPattern(Pattern):
    def __init__(self, base, kleisli):
        self.base = base
        self.kleisli = kleisli

    def match(self, tree):
        pat = map_p(self.kleisli, self.base).match(tree)
        if pat is None:
            return None
        return pat.match(tree)


class FixpointPattern(Pattern):
    """
    Python forbids constructions like `x = f(x)`, so we can't create recursive pattern explicitly
    FixpointPattern is wrapper, which make it possible to write such patterns in fixpoint programming style
    """

    def __init__(self, step):
        self.step = step

    def match(self, tree):
        return self.step(self).match(tree)


class SomeMatchesPattern(Pattern):
    def __init__(self, traverse_apply_function, inner):
        self.traverse_apply_function = traverse_apply_function
        self.inner = inner

    def match(self, tree):
        matches = collect_matches(self.inner, self.traverse_apply_function, tree)
        if len(matches) == 0:
            return None
        return matches


wildcard_p = WildcardPattern()
filter_p = lambda pred, base: ConditionPattern(pred, base)
satisfy_p = lambda pred: filter_p(pred, wildcard_p)
from_function_p = lambda f: WrapperPattern(f)
failed_p = satisfy_p(lambda _: False)
map_p = lambda f, base: MappedPattern(f, base)
fix_p = lambda pat_gen: FixpointPattern(pat_gen)
bind_p = lambda b, k: BindPattern(b, k)
all_p = lambda patterns: AllPattern(patterns)
some_p = lambda traverse, base: SomeMatchesPattern(traverse, base)

if __name__ == "__main__":
    pass
