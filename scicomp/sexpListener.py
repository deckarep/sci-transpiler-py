# Generated from sexp.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .sexpParser import sexpParser
else:
    from sexpParser import sexpParser

# This class defines a complete listener for a parse tree produced by sexpParser.
class sexpListener(ParseTreeListener):

    # Enter a parse tree produced by sexpParser#sexpr.
    def enterSexpr(self, ctx:sexpParser.SexprContext):
        pass

    # Exit a parse tree produced by sexpParser#sexpr.
    def exitSexpr(self, ctx:sexpParser.SexprContext):
        pass


    # Enter a parse tree produced by sexpParser#item.
    def enterItem(self, ctx:sexpParser.ItemContext):
        pass

    # Exit a parse tree produced by sexpParser#item.
    def exitItem(self, ctx:sexpParser.ItemContext):
        pass


    # Enter a parse tree produced by sexpParser#list_.
    def enterList_(self, ctx:sexpParser.List_Context):
        pass

    # Exit a parse tree produced by sexpParser#list_.
    def exitList_(self, ctx:sexpParser.List_Context):
        pass


    # Enter a parse tree produced by sexpParser#array_.
    def enterArray_(self, ctx:sexpParser.Array_Context):
        pass

    # Exit a parse tree produced by sexpParser#array_.
    def exitArray_(self, ctx:sexpParser.Array_Context):
        pass


    # Enter a parse tree produced by sexpParser#atom.
    def enterAtom(self, ctx:sexpParser.AtomContext):
        pass

    # Exit a parse tree produced by sexpParser#atom.
    def exitAtom(self, ctx:sexpParser.AtomContext):
        pass



del sexpParser