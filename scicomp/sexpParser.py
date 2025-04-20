# Generated from sexp.g4 by ANTLR 4.13.2
# encoding: utf-8
from antlr4 import *
from io import StringIO
import sys
if sys.version_info[1] > 5:
	from typing import TextIO
else:
	from typing.io import TextIO

def serializedATN():
    return [
        4,1,9,44,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,1,0,5,0,12,8,0,
        10,0,12,0,15,9,0,1,0,1,0,1,1,1,1,1,1,3,1,22,8,1,1,2,1,2,5,2,26,8,
        2,10,2,12,2,29,9,2,1,2,1,2,1,3,1,3,5,3,35,8,3,10,3,12,3,38,9,3,1,
        3,1,3,1,4,1,4,1,4,0,0,5,0,2,4,6,8,0,1,2,0,1,1,3,4,43,0,13,1,0,0,
        0,2,21,1,0,0,0,4,23,1,0,0,0,6,32,1,0,0,0,8,41,1,0,0,0,10,12,3,2,
        1,0,11,10,1,0,0,0,12,15,1,0,0,0,13,11,1,0,0,0,13,14,1,0,0,0,14,16,
        1,0,0,0,15,13,1,0,0,0,16,17,5,0,0,1,17,1,1,0,0,0,18,22,3,8,4,0,19,
        22,3,4,2,0,20,22,3,6,3,0,21,18,1,0,0,0,21,19,1,0,0,0,21,20,1,0,0,
        0,22,3,1,0,0,0,23,27,5,7,0,0,24,26,3,2,1,0,25,24,1,0,0,0,26,29,1,
        0,0,0,27,25,1,0,0,0,27,28,1,0,0,0,28,30,1,0,0,0,29,27,1,0,0,0,30,
        31,5,8,0,0,31,5,1,0,0,0,32,36,5,5,0,0,33,35,3,2,1,0,34,33,1,0,0,
        0,35,38,1,0,0,0,36,34,1,0,0,0,36,37,1,0,0,0,37,39,1,0,0,0,38,36,
        1,0,0,0,39,40,5,6,0,0,40,7,1,0,0,0,41,42,7,0,0,0,42,9,1,0,0,0,4,
        13,21,27,36
    ]

class sexpParser ( Parser ):

    grammarFileName = "sexp.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                     "<INVALID>", "'['", "']'", "'('", "')'" ]

    symbolicNames = [ "<INVALID>", "STRING", "WHITESPACE", "NUMBER", "SYMBOL", 
                      "LBRACKET", "RBRACKET", "LPAREN", "RPAREN", "LINE_COMMENT" ]

    RULE_sexpr = 0
    RULE_item = 1
    RULE_list_ = 2
    RULE_array_ = 3
    RULE_atom = 4

    ruleNames =  [ "sexpr", "item", "list_", "array_", "atom" ]

    EOF = Token.EOF
    STRING=1
    WHITESPACE=2
    NUMBER=3
    SYMBOL=4
    LBRACKET=5
    RBRACKET=6
    LPAREN=7
    RPAREN=8
    LINE_COMMENT=9

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.13.2")
        self._interp = ParserATNSimulator(self, self.atn, self.decisionsToDFA, self.sharedContextCache)
        self._predicates = None




    class SexprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def EOF(self):
            return self.getToken(sexpParser.EOF, 0)

        def item(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(sexpParser.ItemContext)
            else:
                return self.getTypedRuleContext(sexpParser.ItemContext,i)


        def getRuleIndex(self):
            return sexpParser.RULE_sexpr

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterSexpr" ):
                listener.enterSexpr(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitSexpr" ):
                listener.exitSexpr(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitSexpr" ):
                return visitor.visitSexpr(self)
            else:
                return visitor.visitChildren(self)




    def sexpr(self):

        localctx = sexpParser.SexprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_sexpr)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 13
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while (((_la) & ~0x3f) == 0 and ((1 << _la) & 186) != 0):
                self.state = 10
                self.item()
                self.state = 15
                self._errHandler.sync(self)
                _la = self._input.LA(1)

            self.state = 16
            self.match(sexpParser.EOF)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ItemContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def atom(self):
            return self.getTypedRuleContext(sexpParser.AtomContext,0)


        def list_(self):
            return self.getTypedRuleContext(sexpParser.List_Context,0)


        def array_(self):
            return self.getTypedRuleContext(sexpParser.Array_Context,0)


        def getRuleIndex(self):
            return sexpParser.RULE_item

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterItem" ):
                listener.enterItem(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitItem" ):
                listener.exitItem(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitItem" ):
                return visitor.visitItem(self)
            else:
                return visitor.visitChildren(self)




    def item(self):

        localctx = sexpParser.ItemContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_item)
        try:
            self.state = 21
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [1, 3, 4]:
                self.enterOuterAlt(localctx, 1)
                self.state = 18
                self.atom()
                pass
            elif token in [7]:
                self.enterOuterAlt(localctx, 2)
                self.state = 19
                self.list_()
                pass
            elif token in [5]:
                self.enterOuterAlt(localctx, 3)
                self.state = 20
                self.array_()
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class List_Context(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def LPAREN(self):
            return self.getToken(sexpParser.LPAREN, 0)

        def RPAREN(self):
            return self.getToken(sexpParser.RPAREN, 0)

        def item(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(sexpParser.ItemContext)
            else:
                return self.getTypedRuleContext(sexpParser.ItemContext,i)


        def getRuleIndex(self):
            return sexpParser.RULE_list_

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterList_" ):
                listener.enterList_(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitList_" ):
                listener.exitList_(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitList_" ):
                return visitor.visitList_(self)
            else:
                return visitor.visitChildren(self)




    def list_(self):

        localctx = sexpParser.List_Context(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_list_)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 23
            self.match(sexpParser.LPAREN)
            self.state = 27
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while (((_la) & ~0x3f) == 0 and ((1 << _la) & 186) != 0):
                self.state = 24
                self.item()
                self.state = 29
                self._errHandler.sync(self)
                _la = self._input.LA(1)

            self.state = 30
            self.match(sexpParser.RPAREN)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Array_Context(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def LBRACKET(self):
            return self.getToken(sexpParser.LBRACKET, 0)

        def RBRACKET(self):
            return self.getToken(sexpParser.RBRACKET, 0)

        def item(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(sexpParser.ItemContext)
            else:
                return self.getTypedRuleContext(sexpParser.ItemContext,i)


        def getRuleIndex(self):
            return sexpParser.RULE_array_

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterArray_" ):
                listener.enterArray_(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitArray_" ):
                listener.exitArray_(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitArray_" ):
                return visitor.visitArray_(self)
            else:
                return visitor.visitChildren(self)




    def array_(self):

        localctx = sexpParser.Array_Context(self, self._ctx, self.state)
        self.enterRule(localctx, 6, self.RULE_array_)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 32
            self.match(sexpParser.LBRACKET)
            self.state = 36
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while (((_la) & ~0x3f) == 0 and ((1 << _la) & 186) != 0):
                self.state = 33
                self.item()
                self.state = 38
                self._errHandler.sync(self)
                _la = self._input.LA(1)

            self.state = 39
            self.match(sexpParser.RBRACKET)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class AtomContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def STRING(self):
            return self.getToken(sexpParser.STRING, 0)

        def SYMBOL(self):
            return self.getToken(sexpParser.SYMBOL, 0)

        def NUMBER(self):
            return self.getToken(sexpParser.NUMBER, 0)

        def getRuleIndex(self):
            return sexpParser.RULE_atom

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAtom" ):
                listener.enterAtom(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAtom" ):
                listener.exitAtom(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitAtom" ):
                return visitor.visitAtom(self)
            else:
                return visitor.visitChildren(self)




    def atom(self):

        localctx = sexpParser.AtomContext(self, self._ctx, self.state)
        self.enterRule(localctx, 8, self.RULE_atom)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 41
            _la = self._input.LA(1)
            if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 26) != 0)):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx





