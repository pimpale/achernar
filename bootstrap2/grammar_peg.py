import os
from arpeggio.peg import ParserPEG
from arpeggio import visit_parse_tree
from visitor_pattern import GrammarVisitor


def main(debug=False):

    # Grammar is defined using textual specification based on PEG language.
    # Load grammar form file.
    grammar = open('../minigrammar.peg', 'r').read()

    # First we will make a parser - an instance of the grammar model.
    # Parser model is given in the form of PEG notation therefore we
    # are using ParserPEG class. Root rule name (parsing expression) is "Root".
    parser = ParserPEG(grammar, "Root", debug=debug)

    # An expression we want to evaluate
    input_file = open('testprogram.acn', 'r').read()

    # Then parse tree is created out of the input_expr expression.
    parse_tree = parser.parse(input_file)

    # The result is obtained by semantic evaluation using visitor class.
    # visit_parse_tree will start semantic analysis.
    # In this case semantic analysis will evaluate expression and
    # returned value will be evaluated result of the input_expr expression.
    # result = visit_parse_tree(parse_tree, GrammarVisitor(debug=debug))

    # print("{} = {}".format(input_expr, result))


if __name__ == "__main__":
    # In debug mode dot (graphviz) files for parser model
    # and parse tree will be created for visualization.
    # Checkout current folder for .dot files.
    main(debug=True)
