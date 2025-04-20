# Generated from sexp.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .sexpParser import sexpParser
else:
    from sexpParser import sexpParser

# This class defines a complete generic visitor for a parse tree produced by sexpParser.

class sexpVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by sexpParser#sexpr.
    def visitSexpr(self, ctx:sexpParser.SexprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by sexpParser#item.
    def visitItem(self, ctx:sexpParser.ItemContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by sexpParser#list_.
    def visitList_(self, ctx:sexpParser.List_Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by sexpParser#array_.
    def visitArray_(self, ctx:sexpParser.Array_Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by sexpParser#atom.
    def visitAtom(self, ctx:sexpParser.AtomContext):
        return self.visitChildren(ctx)



del sexpParser