import clang.cindex
import sys
from typing import Any, List
from dataclasses import dataclass

# [CursorKind.UNEXPOSED_DECL, CursorKind.STRUCT_DECL, CursorKind.UNION_DECL, CursorKind.CLASS_DECL, CursorKind.ENUM_DECL, CursorKind.FIELD_DECL, CursorKind.ENUM_CONSTANT_DECL, CursorKind.FUNCTION_DECL, CursorKind.VAR_DECL, CursorKind.PARM_DECL, CursorKind.OBJC_INTERFACE_DECL, CursorKind.OBJC_CATEGORY_DECL, CursorKind.OBJC_PROTOCOL_DECL, CursorKind.OBJC_PROPERTY_DECL, CursorKind.OBJC_IVAR_DECL, CursorKind.OBJC_INSTANCE_METHOD_DECL, CursorKind.OBJC_CLASS_METHOD_DECL, CursorKind.OBJC_IMPLEMENTATION_DECL, CursorKind.OBJC_CATEGORY_IMPL_DECL, CursorKind.TYPEDEF_DECL, CursorKind.CXX_METHOD, CursorKind.NAMESPACE, CursorKind.LINKAGE_SPEC, CursorKind.CONSTRUCTOR, CursorKind.DESTRUCTOR, CursorKind.CONVERSION_FUNCTION, CursorKind.TEMPLATE_TYPE_PARAMETER, CursorKind.TEMPLATE_NON_TYPE_PARAMETER, CursorKind.TEMPLATE_TEMPLATE_PARAMETER, CursorKind.FUNCTION_TEMPLATE, CursorKind.CLASS_TEMPLATE, CursorKind.CLASS_TEMPLATE_PARTIAL_SPECIALIZATION, CursorKind.NAMESPACE_ALIAS, CursorKind.USING_DIRECTIVE, CursorKind.USING_DECLARATION, CursorKind.TYPE_ALIAS_DECL, CursorKind.OBJC_SYNTHESIZE_DECL, CursorKind.OBJC_DYNAMIC_DECL, CursorKind.CXX_ACCESS_SPEC_DECL, CursorKind.OBJC_SUPER_CLASS_REF, CursorKind.OBJC_PROTOCOL_REF, CursorKind.OBJC_CLASS_REF, CursorKind.TYPE_REF, CursorKind.CXX_BASE_SPECIFIER, CursorKind.TEMPLATE_REF, CursorKind.NAMESPACE_REF, CursorKind.MEMBER_REF, CursorKind.LABEL_REF, CursorKind.OVERLOADED_DECL_REF, CursorKind.VARIABLE_REF, CursorKind.INVALID_FILE, CursorKind.NO_DECL_FOUND, CursorKind.NOT_IMPLEMENTED, CursorKind.INVALID_CODE, CursorKind.UNEXPOSED_EXPR, CursorKind.DECL_REF_EXPR, CursorKind.MEMBER_REF_EXPR, CursorKind.CALL_EXPR, CursorKind.OBJC_MESSAGE_EXPR, CursorKind.BLOCK_EXPR, CursorKind.INTEGER_LITERAL, CursorKind.FLOATING_LITERAL, CursorKind.IMAGINARY_LITERAL, CursorKind.STRING_LITERAL, CursorKind.CHARACTER_LITERAL, CursorKind.PAREN_EXPR, CursorKind.UNARY_OPERATOR, CursorKind.ARRAY_SUBSCRIPT_EXPR, CursorKind.BINARY_OPERATOR, CursorKind.COMPOUND_ASSIGNMENT_OPERATOR, CursorKind.CONDITIONAL_OPERATOR, CursorKind.CSTYLE_CAST_EXPR, CursorKind.COMPOUND_LITERAL_EXPR, CursorKind.INIT_LIST_EXPR, CursorKind.ADDR_LABEL_EXPR, CursorKind.StmtExpr, CursorKind.GENERIC_SELECTION_EXPR, CursorKind.GNU_NULL_EXPR, CursorKind.CXX_STATIC_CAST_EXPR, CursorKind.CXX_DYNAMIC_CAST_EXPR, CursorKind.CXX_REINTERPRET_CAST_EXPR, CursorKind.CXX_CONST_CAST_EXPR, CursorKind.CXX_FUNCTIONAL_CAST_EXPR, CursorKind.CXX_TYPEID_EXPR, CursorKind.CXX_BOOL_LITERAL_EXPR, CursorKind.CXX_NULL_PTR_LITERAL_EXPR, CursorKind.CXX_THIS_EXPR, CursorKind.CXX_THROW_EXPR, CursorKind.CXX_NEW_EXPR, CursorKind.CXX_DELETE_EXPR, CursorKind.CXX_UNARY_EXPR, CursorKind.OBJC_STRING_LITERAL, CursorKind.OBJC_ENCODE_EXPR, CursorKind.OBJC_SELECTOR_EXPR, CursorKind.OBJC_PROTOCOL_EXPR, CursorKind.OBJC_BRIDGE_CAST_EXPR, CursorKind.PACK_EXPANSION_EXPR, CursorKind.SIZE_OF_PACK_EXPR, CursorKind.LAMBDA_EXPR, CursorKind.OBJ_BOOL_LITERAL_EXPR, CursorKind.OBJ_SELF_EXPR, CursorKind.OMP_ARRAY_SECTION_EXPR, CursorKind.OBJC_AVAILABILITY_CHECK_EXPR, CursorKind.UNEXPOSED_STMT, CursorKind.LABEL_STMT, CursorKind.COMPOUND_STMT, CursorKind.CASE_STMT, CursorKind.DEFAULT_STMT, CursorKind.IF_STMT, CursorKind.SWITCH_STMT, CursorKind.WHILE_STMT, CursorKind.DO_STMT, CursorKind.FOR_STMT, CursorKind.GOTO_STMT, CursorKind.INDIRECT_GOTO_STMT, CursorKind.CONTINUE_STMT, CursorKind.BREAK_STMT, CursorKind.RETURN_STMT, CursorKind.ASM_STMT, CursorKind.OBJC_AT_TRY_STMT, CursorKind.OBJC_AT_CATCH_STMT, CursorKind.OBJC_AT_FINALLY_STMT, CursorKind.OBJC_AT_THROW_STMT, CursorKind.OBJC_AT_SYNCHRONIZED_STMT, CursorKind.OBJC_AUTORELEASE_POOL_STMT, CursorKind.OBJC_FOR_COLLECTION_STMT, CursorKind.CXX_CATCH_STMT, CursorKind.CXX_TRY_STMT, CursorKind.CXX_FOR_RANGE_STMT, CursorKind.SEH_TRY_STMT, CursorKind.SEH_EXCEPT_STMT, CursorKind.SEH_FINALLY_STMT, CursorKind.MS_ASM_STMT, CursorKind.NULL_STMT, CursorKind.DECL_STMT, CursorKind.OMP_PARALLEL_DIRECTIVE, CursorKind.OMP_SIMD_DIRECTIVE, CursorKind.OMP_FOR_DIRECTIVE, CursorKind.OMP_SECTIONS_DIRECTIVE, CursorKind.OMP_SECTION_DIRECTIVE, CursorKind.OMP_SINGLE_DIRECTIVE, CursorKind.OMP_PARALLEL_FOR_DIRECTIVE, CursorKind.OMP_PARALLEL_SECTIONS_DIRECTIVE, CursorKind.OMP_TASK_DIRECTIVE, CursorKind.OMP_MASTER_DIRECTIVE, CursorKind.OMP_CRITICAL_DIRECTIVE, CursorKind.OMP_TASKYIELD_DIRECTIVE, CursorKind.OMP_BARRIER_DIRECTIVE, CursorKind.OMP_TASKWAIT_DIRECTIVE, CursorKind.OMP_FLUSH_DIRECTIVE, CursorKind.SEH_LEAVE_STMT, CursorKind.OMP_ORDERED_DIRECTIVE, CursorKind.OMP_ATOMIC_DIRECTIVE, CursorKind.OMP_FOR_SIMD_DIRECTIVE, CursorKind.OMP_PARALLELFORSIMD_DIRECTIVE, CursorKind.OMP_TARGET_DIRECTIVE, CursorKind.OMP_TEAMS_DIRECTIVE, CursorKind.OMP_TASKGROUP_DIRECTIVE, CursorKind.OMP_CANCELLATION_POINT_DIRECTIVE, CursorKind.OMP_CANCEL_DIRECTIVE, CursorKind.OMP_TARGET_DATA_DIRECTIVE, CursorKind.OMP_TASK_LOOP_DIRECTIVE, CursorKind.OMP_TASK_LOOP_SIMD_DIRECTIVE, CursorKind.OMP_DISTRIBUTE_DIRECTIVE, CursorKind.OMP_TARGET_ENTER_DATA_DIRECTIVE, CursorKind.OMP_TARGET_EXIT_DATA_DIRECTIVE, CursorKind.OMP_TARGET_PARALLEL_DIRECTIVE, CursorKind.OMP_TARGET_PARALLELFOR_DIRECTIVE, CursorKind.OMP_TARGET_UPDATE_DIRECTIVE, CursorKind.OMP_DISTRIBUTE_PARALLELFOR_DIRECTIVE, CursorKind.OMP_DISTRIBUTE_PARALLEL_FOR_SIMD_DIRECTIVE, CursorKind.OMP_DISTRIBUTE_SIMD_DIRECTIVE, CursorKind.OMP_TARGET_PARALLEL_FOR_SIMD_DIRECTIVE, CursorKind.OMP_TARGET_SIMD_DIRECTIVE, CursorKind.OMP_TEAMS_DISTRIBUTE_DIRECTIVE, CursorKind.TRANSLATION_UNIT, CursorKind.UNEXPOSED_ATTR, CursorKind.IB_ACTION_ATTR, CursorKind.IB_OUTLET_ATTR, CursorKind.IB_OUTLET_COLLECTION_ATTR, CursorKind.CXX_FINAL_ATTR, CursorKind.CXX_OVERRIDE_ATTR, CursorKind.ANNOTATE_ATTR, CursorKind.ASM_LABEL_ATTR, CursorKind.PACKED_ATTR, CursorKind.PURE_ATTR, CursorKind.CONST_ATTR, CursorKind.NODUPLICATE_ATTR, CursorKind.CUDACONSTANT_ATTR, CursorKind.CUDADEVICE_ATTR, CursorKind.CUDAGLOBAL_ATTR, CursorKind.CUDAHOST_ATTR, CursorKind.CUDASHARED_ATTR, CursorKind.VISIBILITY_ATTR, CursorKind.DLLEXPORT_ATTR, CursorKind.DLLIMPORT_ATTR, CursorKind.CONVERGENT_ATTR, CursorKind.WARN_UNUSED_ATTR, CursorKind.WARN_UNUSED_RESULT_ATTR, CursorKind.ALIGNED_ATTR, CursorKind.PREPROCESSING_DIRECTIVE, CursorKind.MACRO_DEFINITION, CursorKind.MACRO_INSTANTIATION, CursorKind.INCLUSION_DIRECTIVE, CursorKind.MODULE_IMPORT_DECL, CursorKind.TYPE_ALIAS_TEMPLATE_DECL, CursorKind.STATIC_ASSERT, CursorKind.FRIEND_DECL, CursorKind.OVERLOAD_CANDIDATE]

# My simple AST structures


class FunctionDescriprtion:
    def __init__(self, name, args, body):
        self.name = name
        self.args = {arg.spelling: arg.type.spelling for arg in args}
        self.body = body


@dataclass
class SillyBinop:
    operator: str
    lhs: any
    rhs: any

    def __str__(self):
        return f"({str(self.lhs)}{self.operator}{str(self.rhs)})"


@dataclass
class SillyUnop:
    operator: str
    prefix: bool  # Avoid boolean blindness
    operand: Any

    def __str__(self):
        if self.prefix:
            return f"({self.operator}{str(self.operand)})"
        else:
            return f"({str(self.operand)}{self.operator})"


@dataclass
class SillyVar:
    name: str

    def __str__(self):
        return self.name


@dataclass
class SillyConst:
    value: str

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
        return (
            f'(SCounterFor "{self.counter}" "{self.final.name}" {self.body.pretty()})'
        )


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
        return "SExpr"


@dataclass
class SillyBlock:
    children: List[Any]

    def pretty(self) -> str:
        children = [c.pretty() for c in self.children]
        return f"(SBlock [{', '.join(children)}])"


# first-class pattern, .match(tree) returns None on failure


class Pattern(object):
    def match(self, tree):
        assert False

    def __or__(self, other):
        return AlternativePattern(self, other)

    def __mul__(self, other):
        return ComposePattern(self, other)


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
map_p = lambda f, base: MappedPattern(f, base)
fix_p = lambda pat_gen: FixpointPattern(pat_gen)

### libclang-specific pattern combinators

of_kind_p = lambda kind: satisfy_p(lambda tree: tree.kind == kind)
unwrap_singleton_p = lambda wrapper_pat: map_p(
    lambda tree: next(tree.get_children()), wrapper_pat
)

expr_p = satisfy_p(lambda tree: tree.kind.is_expression())
stmt_p = satisfy_p(lambda tree: tree.kind.is_statement())

with_unexposed_p = lambda pat: pat | (
    pat * unwrap_singleton_p(of_kind_p(clang.cindex.CursorKind.UNEXPOSED_EXPR))
)


class BlockPattern(Pattern):
    def __init__(self, atom):
        self.atom = atom

    def match(self, tree):
        if tree.kind != clang.cindex.CursorKind.COMPOUND_STMT:
            return None
        print("matching block....")
        matches = [self.atom.match(i) for i in tree.get_children()]
        print(f"chidren matches = {matches}")
        if any(result is None for result in matches):
            return None
        return matches


block_of_p = lambda pat: BlockPattern(pat)


class FunctionPattern(Pattern):
    def __init__(self, body):
        self.body = body

    def match(self, tree):
        if tree.kind != clang.cindex.CursorKind.FUNCTION_DECL:
            return None
        arguments = tree.get_arguments()
        name = tree.spelling
        body = [i for i in tree.get_children()][-1]
        return FunctionDescriprtion(name, arguments, body)


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

        # print("found operator:", f"{operator}(...)" if prefix else f"(...){operator}")

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

        print("for statement found!")

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
            lambda expr: expr.operator in ["++", "--"], unop_of_p(expr_var_p)
        )  # TODO: recognize `i += 1` or `i *= 2` expressions

        pre, condition, iter, body = [i for i in tree.get_children()]

        condition: SillyBinop = silly_expr_p.match(condition)
        print(f"condition = {str(condition)}")
        if condition is None:
            return None

        init: SillyBinop = (assignment_p | value_initialization_p).match(pre)
        print(f"init = {str(init)}")
        if init is None:
            return None

        mut: SillyUnop = mutation_p.match(iter)
        print(f"mut  = {str(mut)}")
        if mut == None:
            return None

        body = self.body.match(body)
        if body is None:
            return

        counter_name = init.lhs.name
        counter_lower_bound = init.rhs

        print(condition.operator, mut.operator)
        # TODO variate cycle types
        if condition.operator not in ["<", "<="] or mut.operator != "++":
            return None

        print("here")

        counter_upper_bound = condition.rhs

        return SillyCounterFor(
            counter_name, counter_lower_bound, counter_upper_bound, "++", body
        )


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


# def traverse_function(function):
#     children = [i for i in function.get_children()]
#     assert len(children) == 2
#     body = children[1]
#     print(body.kind)
#     statements = [i for i in body.get_children()]
#     for stmt in statements:
#         print(
#             stmt.kind,
#             stmt.kind.is_expression(),
#             stmt.kind.is_declaration(),
#             stmt.kind.is_statement(),
#         )
#     print(sum(1 for _ in body.get_children()))


def print_kinds(root, decay=0):
    print(" " * decay, root.kind, root.spelling)
    for child in root.get_children():
        print_kinds(child, decay + 1)


silly_ast_pattern = fix_p(
    lambda silly_ast: map_p(lambda c: SillyBlock(c), block_of_p(silly_ast))
    | silly_if_p(silly_ast)
    | map_p(lambda _: SillyExpr(), expr_p)
    | map_p(
        lambda _: SillyExpr(),
        satisfy_p(lambda tree: tree.kind == clang.cindex.CursorKind.DECL_STMT),
    )
    | map_p(
        lambda _: SillyExpr(),
        satisfy_p(lambda tree: tree.kind == clang.cindex.CursorKind.RETURN_STMT),
    )
    | CounterForPattern(silly_ast)
)

if __name__ == "__main__":
    assert len(sys.argv) >= 3

    file_name = sys.argv[1]
    function_name = sys.argv[2]
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
