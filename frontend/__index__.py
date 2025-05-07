from clang_wrap import *
from domain import *
from patterns import *
from comments_parser import *
from export_tu import *

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
    except Exception as e:
        print(f"Can not parse file due to reason {e.format_exc()}", file=sys.stderr)
        exit(1)

    if dump_ast:
        dump_nodes(tu.root, file=sys.stderr)

    try:
        export_tu = collect_descriptions(tu, function_name)
        function = export_tu.get_function(function_name)
        if function.parameter_name is None:
            raise FrontendException(
                f"Can not deduce parameter name for function {function_name}"
            )
        print(function.parameter_name)
        print(function.name)
        export_tu.dump(file=sys.stdout)
        if dump_cfg:
            print(function.cfg.pretty(), file=sys.stderr)
    except FrontendException as e:
        print(f"Frontend error:\n{e.desc}", file=sys.stderr)
    except Exception as e:
        print(f"Unexpected error occured in frontend:\n{e.format_exc()}")
    except:
        print("Unknown error in frontend")


if __name__ == "__main__":
    main()
