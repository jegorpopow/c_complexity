from clang_wrap import *
from domain import *
from patterns import *
from comments_parser import *

import click


@click.command()
@click.argument("file_name")
@click.argument("function_name")
@click.option(
    "-d", "--dump-cfg", type=bool, default=False, help="Show resulting cfg to stderr"
)
@click.option(
    "-D", "--dump-ast", type=bool, default=False, help="Show clang ast to stderr"
)
def main(file_name, function_name, dump_cfg, dump_ast):
    """Main frontend entrypoint:
    * parses TU using clang frontend
    * finds analyzable patterns and dumps a CFG to stdout
    """

    try:
        tu = TranslationUnit(file_name)
        function = tu.get_function(function_name)
    except:
        print("Can not parse file", file=sys.stderr)
        exit(1)

    if function is None:
        print("No such function", file=sys.stderr)
        exit(1)

    if dump_ast:
        dump_nodes(function.body, file=sys.stderr)

    cfg = silly_ast_pattern(PatternContext(tu)).match(function.body)

    if cfg is None:
        print("Non supported program found", file=sys.stderr)
        exit(1)

    if dump_cfg:
        print(cfg.pretty(), file=sys.stderr)

    if (
        function.comment is not None
        and (direcrive := param_pattern_p.match(function.comment)) is not None
    ):
        print(direcrive.args[0])
    else:
        print("n")
    print(cfg.pretty())


if __name__ == "__main__":
    main()
