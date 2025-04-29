import clang.cindex
import sys
from typing import Any, List
from dataclasses import dataclass

from patterns import *
from domain import *
from comments_parser import *


DEFAULT_PARAMETER_NAME = "n"


def beginning_line(elem: Any) -> int:
    return elem.extent.start.line


def ending_line(elem: Any) -> int:
    return elem.extent.end.line


# [CursorKind.UNEXPOSED_DECL, CursorKind.STRUCT_DECL, CursorKind.UNION_DECL, CursorKind.CLASS_DECL, CursorKind.ENUM_DECL, CursorKind.FIELD_DECL, CursorKind.ENUM_CONSTANT_DECL, CursorKind.FUNCTION_DECL, CursorKind.VAR_DECL, CursorKind.PARM_DECL, CursorKind.OBJC_INTERFACE_DECL, CursorKind.OBJC_CATEGORY_DECL, CursorKind.OBJC_PROTOCOL_DECL, CursorKind.OBJC_PROPERTY_DECL, CursorKind.OBJC_IVAR_DECL, CursorKind.OBJC_INSTANCE_METHOD_DECL, CursorKind.OBJC_CLASS_METHOD_DECL, CursorKind.OBJC_IMPLEMENTATION_DECL, CursorKind.OBJC_CATEGORY_IMPL_DECL, CursorKind.TYPEDEF_DECL, CursorKind.CXX_METHOD, CursorKind.NAMESPACE, CursorKind.LINKAGE_SPEC, CursorKind.CONSTRUCTOR, CursorKind.DESTRUCTOR, CursorKind.CONVERSION_FUNCTION, CursorKind.TEMPLATE_TYPE_PARAMETER, CursorKind.TEMPLATE_NON_TYPE_PARAMETER, CursorKind.TEMPLATE_TEMPLATE_PARAMETER, CursorKind.FUNCTION_TEMPLATE, CursorKind.CLASS_TEMPLATE, CursorKind.CLASS_TEMPLATE_PARTIAL_SPECIALIZATION, CursorKind.NAMESPACE_ALIAS, CursorKind.USING_DIRECTIVE, CursorKind.USING_DECLARATION, CursorKind.TYPE_ALIAS_DECL, CursorKind.OBJC_SYNTHESIZE_DECL, CursorKind.OBJC_DYNAMIC_DECL, CursorKind.CXX_ACCESS_SPEC_DECL, CursorKind.OBJC_SUPER_CLASS_REF, CursorKind.OBJC_PROTOCOL_REF, CursorKind.OBJC_CLASS_REF, CursorKind.TYPE_REF, CursorKind.CXX_BASE_SPECIFIER, CursorKind.TEMPLATE_REF, CursorKind.NAMESPACE_REF, CursorKind.MEMBER_REF, CursorKind.LABEL_REF, CursorKind.OVERLOADED_DECL_REF, CursorKind.VARIABLE_REF, CursorKind.INVALID_FILE, CursorKind.NO_DECL_FOUND, CursorKind.NOT_IMPLEMENTED, CursorKind.INVALID_CODE, CursorKind.UNEXPOSED_EXPR, CursorKind.DECL_REF_EXPR, CursorKind.MEMBER_REF_EXPR, CursorKind.CALL_EXPR, CursorKind.OBJC_MESSAGE_EXPR, CursorKind.BLOCK_EXPR, CursorKind.INTEGER_LITERAL, CursorKind.FLOATING_LITERAL, CursorKind.IMAGINARY_LITERAL, CursorKind.STRING_LITERAL, CursorKind.CHARACTER_LITERAL, CursorKind.PAREN_EXPR, CursorKind.UNARY_OPERATOR, CursorKind.ARRAY_SUBSCRIPT_EXPR, CursorKind.BINARY_OPERATOR, CursorKind.COMPOUND_ASSIGNMENT_OPERATOR, CursorKind.CONDITIONAL_OPERATOR, CursorKind.CSTYLE_CAST_EXPR, CursorKind.COMPOUND_LITERAL_EXPR, CursorKind.INIT_LIST_EXPR, CursorKind.ADDR_LABEL_EXPR, CursorKind.StmtExpr, CursorKind.GENERIC_SELECTION_EXPR, CursorKind.GNU_NULL_EXPR, CursorKind.CXX_STATIC_CAST_EXPR, CursorKind.CXX_DYNAMIC_CAST_EXPR, CursorKind.CXX_REINTERPRET_CAST_EXPR, CursorKind.CXX_CONST_CAST_EXPR, CursorKind.CXX_FUNCTIONAL_CAST_EXPR, CursorKind.CXX_TYPEID_EXPR, CursorKind.CXX_BOOL_LITERAL_EXPR, CursorKind.CXX_NULL_PTR_LITERAL_EXPR, CursorKind.CXX_THIS_EXPR, CursorKind.CXX_THROW_EXPR, CursorKind.CXX_NEW_EXPR, CursorKind.CXX_DELETE_EXPR, CursorKind.CXX_UNARY_EXPR, CursorKind.OBJC_STRING_LITERAL, CursorKind.OBJC_ENCODE_EXPR, CursorKind.OBJC_SELECTOR_EXPR, CursorKind.OBJC_PROTOCOL_EXPR, CursorKind.OBJC_BRIDGE_CAST_EXPR, CursorKind.PACK_EXPANSION_EXPR, CursorKind.SIZE_OF_PACK_EXPR, CursorKind.LAMBDA_EXPR, CursorKind.OBJ_BOOL_LITERAL_EXPR, CursorKind.OBJ_SELF_EXPR, CursorKind.OMP_ARRAY_SECTION_EXPR, CursorKind.OBJC_AVAILABILITY_CHECK_EXPR, CursorKind.UNEXPOSED_STMT, CursorKind.LABEL_STMT, CursorKind.COMPOUND_STMT, CursorKind.CASE_STMT, CursorKind.DEFAULT_STMT, CursorKind.IF_STMT, CursorKind.SWITCH_STMT, CursorKind.WHILE_STMT, CursorKind.DO_STMT, CursorKind.FOR_STMT, CursorKind.GOTO_STMT, CursorKind.INDIRECT_GOTO_STMT, CursorKind.CONTINUE_STMT, CursorKind.BREAK_STMT, CursorKind.RETURN_STMT, CursorKind.ASM_STMT, CursorKind.OBJC_AT_TRY_STMT, CursorKind.OBJC_AT_CATCH_STMT, CursorKind.OBJC_AT_FINALLY_STMT, CursorKind.OBJC_AT_THROW_STMT, CursorKind.OBJC_AT_SYNCHRONIZED_STMT, CursorKind.OBJC_AUTORELEASE_POOL_STMT, CursorKind.OBJC_FOR_COLLECTION_STMT, CursorKind.CXX_CATCH_STMT, CursorKind.CXX_TRY_STMT, CursorKind.CXX_FOR_RANGE_STMT, CursorKind.SEH_TRY_STMT, CursorKind.SEH_EXCEPT_STMT, CursorKind.SEH_FINALLY_STMT, CursorKind.MS_ASM_STMT, CursorKind.NULL_STMT, CursorKind.DECL_STMT, CursorKind.OMP_PARALLEL_DIRECTIVE, CursorKind.OMP_SIMD_DIRECTIVE, CursorKind.OMP_FOR_DIRECTIVE, CursorKind.OMP_SECTIONS_DIRECTIVE, CursorKind.OMP_SECTION_DIRECTIVE, CursorKind.OMP_SINGLE_DIRECTIVE, CursorKind.OMP_PARALLEL_FOR_DIRECTIVE, CursorKind.OMP_PARALLEL_SECTIONS_DIRECTIVE, CursorKind.OMP_TASK_DIRECTIVE, CursorKind.OMP_MASTER_DIRECTIVE, CursorKind.OMP_CRITICAL_DIRECTIVE, CursorKind.OMP_TASKYIELD_DIRECTIVE, CursorKind.OMP_BARRIER_DIRECTIVE, CursorKind.OMP_TASKWAIT_DIRECTIVE, CursorKind.OMP_FLUSH_DIRECTIVE, CursorKind.SEH_LEAVE_STMT, CursorKind.OMP_ORDERED_DIRECTIVE, CursorKind.OMP_ATOMIC_DIRECTIVE, CursorKind.OMP_FOR_SIMD_DIRECTIVE, CursorKind.OMP_PARALLELFORSIMD_DIRECTIVE, CursorKind.OMP_TARGET_DIRECTIVE, CursorKind.OMP_TEAMS_DIRECTIVE, CursorKind.OMP_TASKGROUP_DIRECTIVE, CursorKind.OMP_CANCELLATION_POINT_DIRECTIVE, CursorKind.OMP_CANCEL_DIRECTIVE, CursorKind.OMP_TARGET_DATA_DIRECTIVE, CursorKind.OMP_TASK_LOOP_DIRECTIVE, CursorKind.OMP_TASK_LOOP_SIMD_DIRECTIVE, CursorKind.OMP_DISTRIBUTE_DIRECTIVE, CursorKind.OMP_TARGET_ENTER_DATA_DIRECTIVE, CursorKind.OMP_TARGET_EXIT_DATA_DIRECTIVE, CursorKind.OMP_TARGET_PARALLEL_DIRECTIVE, CursorKind.OMP_TARGET_PARALLELFOR_DIRECTIVE, CursorKind.OMP_TARGET_UPDATE_DIRECTIVE, CursorKind.OMP_DISTRIBUTE_PARALLELFOR_DIRECTIVE, CursorKind.OMP_DISTRIBUTE_PARALLEL_FOR_SIMD_DIRECTIVE, CursorKind.OMP_DISTRIBUTE_SIMD_DIRECTIVE, CursorKind.OMP_TARGET_PARALLEL_FOR_SIMD_DIRECTIVE, CursorKind.OMP_TARGET_SIMD_DIRECTIVE, CursorKind.OMP_TEAMS_DISTRIBUTE_DIRECTIVE, CursorKind.TRANSLATION_UNIT, CursorKind.UNEXPOSED_ATTR, CursorKind.IB_ACTION_ATTR, CursorKind.IB_OUTLET_ATTR, CursorKind.IB_OUTLET_COLLECTION_ATTR, CursorKind.CXX_FINAL_ATTR, CursorKind.CXX_OVERRIDE_ATTR, CursorKind.ANNOTATE_ATTR, CursorKind.ASM_LABEL_ATTR, CursorKind.PACKED_ATTR, CursorKind.PURE_ATTR, CursorKind.CONST_ATTR, CursorKind.NODUPLICATE_ATTR, CursorKind.CUDACONSTANT_ATTR, CursorKind.CUDADEVICE_ATTR, CursorKind.CUDAGLOBAL_ATTR, CursorKind.CUDAHOST_ATTR, CursorKind.CUDASHARED_ATTR, CursorKind.VISIBILITY_ATTR, CursorKind.DLLEXPORT_ATTR, CursorKind.DLLIMPORT_ATTR, CursorKind.CONVERGENT_ATTR, CursorKind.WARN_UNUSED_ATTR, CursorKind.WARN_UNUSED_RESULT_ATTR, CursorKind.ALIGNED_ATTR, CursorKind.PREPROCESSING_DIRECTIVE, CursorKind.MACRO_DEFINITION, CursorKind.MACRO_INSTANTIATION, CursorKind.INCLUSION_DIRECTIVE, CursorKind.MODULE_IMPORT_DECL, CursorKind.TYPE_ALIAS_TEMPLATE_DECL, CursorKind.STATIC_ASSERT, CursorKind.FRIEND_DECL, CursorKind.OVERLOAD_CANDIDATE]

### libclang-specific pattern combinators

of_kind_p = lambda kind: satisfy_p(lambda tree: tree.kind == kind)
unwrap_singleton_p = lambda wrapper_pat: map_p(
    lambda tree: next(tree.get_children()), wrapper_pat
)

expr_p = satisfy_p(lambda tree: tree.kind.is_expression())
decl_p = satisfy_p(lambda tree: tree.kind == clang.cindex.CursorKind.DECL_STMT)
stmt_p = satisfy_p(lambda tree: tree.kind.is_statement())
return_p = satisfy_p(lambda tree: tree.kind == clang.cindex.CursorKind.RETURN_STMT)

with_unexposed_p = lambda pat: pat | (
    pat * unwrap_singleton_p(of_kind_p(clang.cindex.CursorKind.UNEXPOSED_EXPR))
)


class TranslationUnit:
    def __init__(self, file_name, args=["-std=c11"]):
        self.file_name = file_name
        self.index = clang.cindex.Index.create()
        self.args = args
        self.clang_tu = self.index.parse(self.file_name, args=args)
        self.root = self.clang_tu.cursor
        self.tokens = list(self.root.get_tokens())

    def get_function(self, function_name):
        pretty_function: FunctionDescriprtion | None = find_first(
            named_function_p(function_name), self.root
        )
        if pretty_function is None:
            return pretty_function
        first_line = beginning_line(pretty_function.header)
        comments = self.find_comments(first_line - 1)
        param: Directive = param_pattern_p.match_first(comments)
        if param is not None:
            pretty_function.parameter_name = param.args[0]
        else:
            pretty_function.parameter_name = DEFAULT_PARAMETER_NAME
        if len(comments) > 0:
            pretty_function.comment = comments
        return pretty_function

    def find_one_line_comment(self, line):
        for token in self.tokens:
            if (
                token.kind == clang.cindex.TokenKind.COMMENT
                and token.spelling.startswith("//")
                and token.extent.start.line == line
            ):
                # print(token_to_string(token))
                return token.spelling[2:]

        return None

    def find_multiline_comments(self, line):
        result = []
        for token in self.tokens:
            if (
                token.kind == clang.cindex.TokenKind.COMMENT
                and token.spelling.startswith("/*")
                and token.extent.start.line <= line
                and token.extent.end.line >= line
            ):
                result.append(token.spelling[2:-2])
        return result

    def find_comments(self, line):
        one_line = self.find_one_line_comment(line)
        result = self.find_multiline_comments(line)

        if one_line is not None:
            result.append(one_line)

        return result


class BlockPattern(Pattern):
    def __init__(self, atom):
        self.atom = atom

    def match(self, tree):
        if tree.kind != clang.cindex.CursorKind.COMPOUND_STMT:
            return None
        matches = [self.atom.match(i) for i in tree.get_children()]
        if any(result is None for result in matches):
            return None
        return matches


block_of_p = lambda pat: BlockPattern(pat)


@dataclass
class PatternContext:
    function: FunctionDescriprtion
    tu: TranslationUnit


class FunctionPattern(Pattern):
    def __init__(self, body):
        self.body = body

    def match(self, tree):
        if tree.kind != clang.cindex.CursorKind.FUNCTION_DECL:
            return None
        arguments = tree.get_arguments()
        name = tree.spelling
        body = [i for i in tree.get_children()][-1]
        return FunctionDescriprtion(name, tree, arguments, body)


named_function_p = lambda name: filter_p(
    lambda function_desc: function_desc.name == name,
    FunctionPattern(block_of_p(expr_p | stmt_p)),
)


def get_binop_spelling(node):
    lhs, _ = [i for i in node.get_children()]
    lhs_size = sum(1 for _ in lhs.get_tokens())
    return [i for i in node.get_tokens()][lhs_size].spelling


def get_unop_spelling(node):
    tokens = [i for i in node.get_tokens()]
    assert len(tokens) >= 2
    if tokens[0].spelling in ["++", "--", "-"]:
        return True, tokens[0].spelling
    else:
        return False, tokens[-1].spelling


class BinopPattern(Pattern):
    def __init__(self, left_operand, right_operand):
        self.left_operand = left_operand
        self.right_operand = right_operand

    def match(self, tree):
        if tree.kind != clang.cindex.CursorKind.BINARY_OPERATOR:
            return None
        operands = [i for i in tree.get_children()]
        assert len(operands) == 2
        lhs, rhs = self.left_operand.match(operands[0]), self.right_operand.match(
            operands[1]
        )
        if lhs is None or rhs is None:
            return None
        return SillyBinop(get_binop_spelling(tree), lhs, rhs)


class UnopPattern(Pattern):
    def __init__(self, operand):
        self.operand = operand

    def match(self, tree):
        if tree.kind != clang.cindex.CursorKind.UNARY_OPERATOR:
            return None
        operand = next(tree.get_children())
        prefix, operator = get_unop_spelling(tree)

        val = self.operand.match(operand)
        if val == None:
            return

        return SillyUnop(operator, prefix, val)


integer_literal_p = map_p(
    lambda tree: SillyConst(next(tree.get_tokens()).spelling),
    of_kind_p(clang.cindex.CursorKind.INTEGER_LITERAL),
)

expr_var_p = with_unexposed_p(
    map_p(
        lambda tree: SillyVar(tree.spelling),
        of_kind_p(clang.cindex.CursorKind.DECL_REF_EXPR),
    )
)

binop_of_p = lambda lhs, rhs: BinopPattern(lhs, rhs)
unop_of_p = lambda operand: UnopPattern(operand)

silly_expr_p = fix_p(
    lambda pat: integer_literal_p | expr_var_p | BinopPattern(pat, pat)
)


class FunctionCallPattern(Pattern):
    def __init__(self, args: List[Pattern], name: str | None = None):
        self.args_pattern = args
        self.name = name

    def match(self, tree):
        if tree.kind != clang.cindex.CursorKind.CALL_EXPR:
            return None

        if self.name is not None and tree.spelling != self.name:
            return None

        args = list(tree.get_children())[1:]
        args = zip_match(patterns=self.args_pattern, targets=args)

        if args is None:
            return None

        return NamedFunctionCall(tree.spelling, args)


def function_one_argument_patterns(
    function: FunctionDescriprtion, pattern: Pattern
) -> List[Pattern]:
    return [
        pattern if i == function.parameter_idx() else map_p(lambda _: None, wildcard_p)
        for i in range(len(function.args))
    ]


recursive_call_p = lambda ctx: FunctionCallPattern(
    function_one_argument_patterns(ctx.function, silly_expr_p), name=ctx.function.name
)


class IfPattern(Pattern):
    def __init__(self, condition, then_branch, else_branch=None, else_obligatory=False):
        self.condition = condition
        self.then_branch = then_branch
        self.else_branch = else_branch
        self.else_obligatory = else_obligatory

    def match(self, tree):
        if tree.kind != clang.cindex.CursorKind.IF_STMT:
            return None

        children = [i for i in tree.get_children()]
        if self.else_obligatory and len(children) == 2:
            return None
        cond = self.condition.match(children[0])
        then_branch = self.then_branch.match(children[1])
        if len(children) == 3:
            else_branch = self.else_branch.match(children[2])
        else:
            else_branch = None
        return SillyIf(cond, then_branch, else_branch)


silly_if_p = lambda branch: IfPattern(wildcard_p, branch, branch, else_obligatory=False)


class CounterForPattern(Pattern):
    def __init__(self, body):
        self.body = body

    def match(self, tree):
        if tree.kind != clang.cindex.CursorKind.FOR_STMT:
            return None

        assignment_p = filter_p(
            lambda expr: expr.operator == "=", binop_of_p(expr_var_p, silly_expr_p)
        )
        value_initialization_p = filter_p(
            lambda expr: not expr.rhs is None,
            map_p(
                lambda decl: SillyBinop(
                    "=",
                    SillyVar(decl.spelling),
                    silly_expr_p.match(next(decl.get_children())),
                ),
                of_kind_p(clang.cindex.CursorKind.VAR_DECL),
            ),
        ) * unwrap_singleton_p(of_kind_p(clang.cindex.CursorKind.DECL_STMT))

        mutation_p = filter_p(
            lambda expr: expr.operator in ["++", "--"], unop_of_p(silly_expr_p)
        ) | filter_p(
            lambda expr: expr.operator in ["+=", "-="],
            binop_of_p(silly_expr_p, silly_expr_p),
        )
        pre, condition, iter, body = [i for i in tree.get_children()]

        condition: SillyBinop = silly_expr_p.match(condition)
        if condition is None:
            return None

        init: SillyBinop = (assignment_p | value_initialization_p).match(pre)
        if init is None:
            return None

        mut: SillyUnop = mutation_p.match(iter)
        if mut == None:
            return None

        body = self.body.match(body)
        if body is None:
            return

        counter_name = init.lhs.name

        if condition.operator in ["<", "<="] and mut.operator in ["++", "+="]:
            counter_lower_bound = init.rhs
            counter_upper_bound = condition.rhs
            return SillyCounterFor(
                counter_name, counter_lower_bound, counter_upper_bound, "++", body
            )

        if condition.operator in [">", ">="] and mut.operator in ["--", "-="]:
            counter_lower_bound = condition.rhs
            counter_upper_bound = init.rhs
            return SillyCounterFor(
                counter_name, counter_lower_bound, counter_upper_bound, "--", body
            )

        return None


class HintedLoopPattern(Pattern):
    def __init__(self, ctx: PatternContext, body_pattern: Pattern):
        self.ctx = ctx
        self.body_pattern = body_pattern

    def match(self, tree):
        if tree.kind not in [
            clang.cindex.CursorKind.FOR_STMT,
            clang.cindex.CursorKind.WHILE_STMT,
        ]:
            return None

        line = beginning_line(tree) - 1
        comments = self.ctx.tu.find_comments(line)

        loop_variant_directive: Directive = loop_variant_p.match_first(comments)
        if loop_variant_directive is None:
            return None

        body = None
        match tree.kind:
            case clang.cindex.CursorKind.FOR_STMT:
                _, _, _, body = [i for i in tree.get_children()]
            case clang.cindex.CursorKind.WHILE_STMT:
                _, body = [i for i in tree.get_children()]
            case _:
                return None

        body = self.body_pattern.match(body)

        if body is None:
            return None

        parsed = zip_match(
            patterns=[wildcard_p, text_expr_p, text_expr_p, wildcard_p],
            targets=loop_variant_directive.args,
        )

        if parsed is None:
            return None

        counter, initial, final, step = parsed
        return SillyCounterFor(
            counter, initial, final, "++" if int(step) > 0 else "--", body
        )


class AlreadyEncounteredLoopPattern(Pattern):
    def __init__(self, ctx: PatternContext, body_pattern: Pattern):
        self.ctx = ctx
        self.body_pattern = body_pattern

    def match(self, tree):
        if tree.kind not in [
            clang.cindex.CursorKind.FOR_STMT,
            clang.cindex.CursorKind.WHILE_STMT,
        ]:
            return None

        line = beginning_line(tree) - 1
        comments = self.ctx.tu.find_comments(line)

        loop_variant_directive = already_encountered_label_p.match_first(comments)
        if loop_variant_directive is None:
            return None

        body = None
        match tree.kind:
            case clang.cindex.CursorKind.FOR_STMT:
                _, _, _, body = [i for i in tree.get_children()]
            case clang.cindex.CursorKind.WHILE_STMT:
                _, body = [i for i in tree.get_children()]
            case _:
                return None

        body = self.body_pattern.match(body)
        if body is None:
            return None
        return SillyExpr()


hinted_loop_p = lambda ctx: lambda body: HintedLoopPattern(ctx, body)
already_encountered_p = lambda ctx: lambda body: AlreadyEncounteredLoopPattern(
    ctx, body
)

expr_to_block_p = lambda p: map_p(
    lambda elements: SillyBlock(elements), some_p(traverse_apply, p)
) * (expr_p | decl_p | return_p)


def traverse_apply(pattern: Pattern, action, root):
    """Visits all AST Nodes, performs action on all nodes, that matches pattern"""
    result = pattern.match(root)
    if not result is None:
        action(result)
    children = [i for i in root.get_children()]
    for child in children:
        traverse_apply(pattern, action, child)


def find_first(pattern: Pattern, root):
    """finds the most left-outer node, which matches the pattern"""
    result = pattern.match(root)
    if not result is None:
        return result
    for child in [i for i in root.get_children()]:
        child_result = find_first(pattern, child)
        if not child_result is None:
            return child_result
    return None


def extent_to_string(extent) -> str:
    return f"{extent.start.line}:{extent.start.column} - {extent.end.line}:{extent.end.column}"


def print_kinds(root, file=sys.stdout, decay=0):
    print(" " * decay, root.kind, root.spelling, file=file)
    for child in root.get_children():
        print_kinds(child, file, decay + 1)


def dump_nodes(root, file=sys.stdout, decay=0):
    print(
        f"{'    ' * decay}{root.kind} `{root.spelling}` ({extent_to_string(root.extent)})",
        file=file,
    )
    for child in root.get_children():
        dump_nodes(child, file, decay + 1)


def token_to_string(token) -> str:
    return f"{token.kind} `{token.spelling}` ({extent_to_string(token.extent)})"


def dump_tokens(tokens):
    for token in tokens:
        print(token_to_string(token))


silly_ast_pattern = lambda ctx: fix_p(
    lambda silly_ast: map_p(lambda c: SillyBlock(c), block_of_p(silly_ast))
    | silly_if_p(silly_ast)
    | expr_to_block_p(recursive_call_p(ctx))
    | map_p(lambda _: SillyExpr(), expr_p)
    | map_p(lambda _: SillyExpr(), decl_p)
    | map_p(lambda _: SillyExpr(), return_p)
    | CounterForPattern(silly_ast)
    | hinted_loop_p(ctx)(silly_ast)
    | already_encountered_p(ctx)(silly_ast)
)

if __name__ == "__main__":
    assert len(sys.argv) >= 3

    file_name = sys.argv[1]
    function_name = sys.argv[2]

    tu = TranslationUnit(file_name=file_name)
    pretty_function = tu.get_function(function_name=function_name)

    index = clang.cindex.Index.create()
    translation_unit = index.parse(file_name, args=["-std=c11"])
    root = translation_unit.cursor

    pretty_function = find_first(named_function_p(function_name), root)
    function = pretty_function

    # function = find_first(satisfy_p(lambda node: node.spelling == function_name), root)
    # print("-------------------")
    # print_kinds(function.body)
    # print("-------------------")
    # print_kinds(root)
    # print("-------------------")
    # print(*[i.type.spelling for i in function.get_arguments()])
    # print("-------------------")
    # traverse_function(function.body)
    # print("-------------------")
    # traverse_apply(
    #     block_of_p(expr_p | stmt_p),
    #     lambda exprs: print(*[expr.kind for expr in exprs]),
    #     function.body,
    # )
    # print("-------------------")
    # print(pretty_function.name)
    # print("-------------------")
    # traverse_apply(silly_expr_p, lambda expr: print(str(expr)), function.body)
    # print("-------------------")
    # traverse_apply(
    #     CounterForPattern(block_of_p(expr_p | stmt_p)),
    #     lambda expr: print(str(expr)),
    #     function.body,
    # )
    # print("-------------------")
    print(silly_ast_pattern.match(function.body).pretty())
