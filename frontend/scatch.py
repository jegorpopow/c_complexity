from domain import *
from clang_wrap import *
from patterns import *

if __name__ == "__main__":
    assert len(sys.argv) >= 3

    file_name = sys.argv[1]
    function_name = sys.argv[2]

    tu = TranslationUnit(file_name=file_name)

    pretty_function = tu.get_function(function_name=function_name)

    # print(tu.find_comments(66))

    # dump_tokens(tu.tokens)
    # dump_tokens(tu.tokens)
    # dump_nodes(tu.root)
    # dump_nodes(pretty_function.body)

    # print(pretty_function.comment)
    # print(find_first(function_call_p(wildcard_p), pretty_function.body))
