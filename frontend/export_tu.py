from typing import Dict, Set, Any, Callable
from domain import *
from clang_wrap import *


class FrontendException(Exception):
    def __init__(self, desc):
        self.desc = desc

    def __str__(self):
        return self.desc


DEFAULT_CFG_PATTERN = lambda tu: lambda func_desc: fix_p(
    lambda cfg: map_p(lambda c: SBlock(c), block_of_p(cfg))
    | silly_if_p(cfg)
    | expr_to_block_p(
        recursive_call_p(PatternContext(func_desc, tu))
        | interunit_call_p(PatternContext(func_desc, tu))
        | map_p(
            lambda c: SExpr(), any_call_p
        ),  # All extern call are considered as constants
        expr_p | return_p | decl_p,
    )
    | map_p(lambda _: SExpr(), expr_p * no_calls_p)
    | map_p(lambda _: SExpr(), decl_p * no_calls_p)
    | map_p(lambda _: SExpr(), return_p * no_calls_p)
    | CounterForPattern(cfg)
    | hinted_loop_p(PatternContext(func_desc, tu))(cfg)
    | already_encountered_p(PatternContext(func_desc, tu))(cfg)
)


class ExportTU:
    """
    Describes the set of functions to export to backend
    """

    def __init__(self):
        self.functions: Dict[str, FunctionDescriprtion] = dict()

    def add_function(self, function: FunctionDescriprtion):
        if function.cfg is None:
            raise FrontendException(
                f"Function `{function.name}` does not contain suupported CFG"
            )
        if not function.name in self.functions:
            self.functions[function.name] = function

    def get_function(self, function_name: str) -> FunctionDescriprtion:
        if function_name not in self.functions:
            raise FrontendException(
                f"Can't find function named {function_name} in exported translation unit"
            )
        return self.functions[function_name]

    def dump(self, file):
        for function in self.functions:
            print(function, file=file)
            print(self.functions[function].parameter_name)
            print(self.functions[function].cfg.pretty(), file=file)


def working_set_collect(intitial: Set[Any], transform) -> Set[Any]:
    working: Set[Any] = intitial.copy()
    used: Set[Any] = set()
    result: Set[Any] = set()

    while len(working) != 0:
        elem = working.pop()
        used.add(elem)

        processed, induced = transform(elem)
        result.add(processed)

        induced -= used
        working |= induced

    return result


def match_cfg(function: FunctionDescriprtion, cfg_pattern_fabric):
    function.cfg = cfg_pattern_fabric(function).match(function.body)
    if function.cfg is None:
        raise FrontendException(
            f"Can not extract control-flow graph from function `{function.name}` AST. It may contain some unsupported conctructions"
        )
    return function


def get_function_from_tu(
    tu: TranslationUnit, function_name: str
) -> FunctionDescriprtion:
    f = tu.get_function(function_name)
    if f is None:
        raise FrontendException(
            f"Cannot find function `{function_name}` in translation unit `{tu.file_name}`"
        )
    return f


def collect_descriptions(
    tu: TranslationUnit, function_name: str, cfg_pattern_fabric=DEFAULT_CFG_PATTERN
) -> ExportTU:
    def transform(function_name):
        function_desc = get_function_from_tu(tu, function_name)
        function = match_cfg(function_desc, cfg_pattern_fabric(tu))
        return function, cfg_collect_calls(function.cfg)

    function_descriptions = working_set_collect({function_name}, transform)

    export_tu = ExportTU()
    for function_description in function_descriptions:
        export_tu.add_function(function_description)
    return export_tu
