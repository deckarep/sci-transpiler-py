import glob
import sys
import pdb
import ast
from enum import Enum

from antlr4 import *
from antlr4 import CommonTokenStream
from antlr4 import FileStream
from antlr4 import TerminalNode
from antlr4 import ParserRuleContext
from antlr4.InputStream import InputStream

from scicomp.sexpLexer import sexpLexer
from scicomp.sexpParser import sexpParser
from scicomp.sexpVisitor import sexpVisitor

# Notes: given any ctx, you can get the parent context using ctx.parentCtx
# Notes: if you're at the root, then parentCtx is None.
# Example: go up two levels and spit out the text.
# Example: ctx.parentCtx.parentCtx.getText()

def dump(ctx):
    if ctx is None:
        print("Nothing to dump")
    else:
        print("grand parent: ", ctx.parentCtx.parentCtx.getText(), type(ctx.parentCtx.parentCtx))
        print("immediate parent: ", ctx.parentCtx.getText(), type(ctx.parentCtx))
        children = [c for c in ctx.getChildren()]
        print("len(children): " + str(len(children)))
        for idx, c in enumerate(children):
            print("idx: " + str(idx) + " => " + c.getText() + "\t" + str(type(c)))


class ContextKind(Enum):
    STMT = 0
    EXPR = 1
    TMP_EXPR = 3 # when _tmp variable needs to be used.
    RET_EXPR = 2 # when _ret variable needs to be used.

class ArgCVisitor(sexpVisitor):
    '''This is a non-mutating visitor which checks for the presence of the `argc`
    keyword that may be referenced by SCI in any procedure/method or function.'''

    ARGC_COMMENT = "# sci:argc injected due to usage"
    ARGC_STATEMENT = "argc = sum(v is not None for v in locals().values()) + len(rest)"

    def __init__(self, emitter):
        self.uses_argc = False
        self.emitter = emitter

    def emit(self):
        if self.uses_argc:
            self.emitter()
            self.emitter(self.ARGC_COMMENT)
            self.emitter(self.ARGC_STATEMENT)
            self.emitter()

    def visitAtom(self, ctx:sexpParser.AtomContext):
        if ctx.getText() == "argc":
            self.uses_argc = True

class IndentCtx:
    def __init__(self, visitor):
        self.visitor = visitor

    def __enter__(self):
        self.visitor.indent_lvl += 1

    def __exit__(self, exc_type, exc_value, traceback):
        self.visitor.indent_lvl -= 1


class SexpCustomVisitor(sexpVisitor):
    def __init__(self):
        # Bottom context stack is STMT
        self.context_stack = [ContextKind.STMT]
        self.side_effect_stmts = []
        self.output = []

        self.indent_char = " "
        self.indent_lvl = 0
        self.indent_size = 4

    def pushCtx(self, ctx:ContextKind):
        self.context_stack.append(ctx)

    def popCtx(self):
        self.context_stack.pop()

    def peekCtx(self):
        return self.context_stack[-1]

    # def get_comments_before(self, ctx, token_stream):
    #     # Get the start token of this rule
    #     start = ctx.start.tokenIndex

    #     comments = []
    #     i = start - 1
    #     while i >= 0:
    #         tok = token_stream.tokens[i]
    #         if tok.channel != Token.HIDDEN_CHANNEL:
    #             break  # Stop at the first real token before the node
    #         if tok.type == MyLangLexer.COMMENT:
    #             comments.insert(0, tok.text.strip())
    #         i -= 1

    #     return comments

    def emit(self, val=None):
        self.emitRaw(val, "\n")

    def emitRaw(self, val, nl=""):
        if val is None:
            # Handle a new-line, when val is None/empty.
            self.output.append(nl)
        else:
            if nl == "\n":
                self.output.append(self.ofIndent() + val + nl)
            else:
                self.output.append(val + nl)

    def ofIndent(self):
        return self.indent_char * (self.indent_lvl * self.indent_size)

    def visitSexpr(self, ctx:sexpParser.SexprContext):
        self.emit("# transpiled from SCI script => python3 by @deckarep (2025)")
        self.visitChildren(ctx)
        return "".join(self.output)

    def visitAtom(self, ctx:sexpParser.AtomContext):
        if ctx.getChildCount() == 1:
            match atm := ctx.getChild(0).getText():
                case "&rest":
                    # Rewrite sci &rest to python *rest
                    return "*rest"
                case "-info-:": # TODO: propDict, etc.
                    # Rewrite sci -info-: to python _info string.
                    return "\"_info:\""
                case "-propDict-:":
                    return "\"_propDict:\""
                case _:
                    return atm
        elif ctx.getChildCount() >= 2:
            raise Exception("An atom should not have 2 children but somehow does!")

    def visitArray_(self, ctx:sexpParser.Array_Context):
        return "{}[{}]".format(ctx.getChild(1).getText(), self.visit(ctx.getChild(2)))

    def visitExpr(self, ctx):
        self.pushCtx(ContextKind.EXPR)
        result = self.visit(ctx)
        self.popCtx()
        return result

    def visitList_(self, ctx:sexpParser.List_Context):
        token = ctx.getChild(1).getText()
        if token == "script#":
            self.emit("# script# " + ctx.getChild(2).getText())
        elif token == "use":
            self.emit("import " + ctx.getChild(2).getText())
        elif token == "include":
            self.emit("import " + ctx.getChild(2).getText())
        elif token == "public":
            self.emit("\npublic_exports = {")
            # Children includes first paranthesis and last, so we trim.
            self.processPublicList_([c for c in ctx.getChildren()][2:-1])
            self.emit("}")
        elif token == "procedure":
            proc_name = ctx.getChild(2).getChild(0).getChild(1).getText()
            if proc_name not in set(["mega"]):#"#umod", "Eval", "WordAt", "OneOf", "sign", "Min", "Max", "InRect"]):
                return

            body_statements = [c for c in ctx.getChildren()][3:-1]
            self.processProcedure(proc_name, 
                [c.getText() for c in ctx.getChild(2).getChild(0).getChildren()][2:-1], 
                body_statements)
        elif token == "class":
            self.emit("\n<class declaration>")
        elif token == "return":
            if not isinstance(ctx.getChild(2), TerminalNode):
                self.processReturn(ctx.getChild(2))
            else:
                self.processReturn(None)
        elif token == "if":
            #TODO: this is buggy, it's asumming that there isn't multiline if statements currently.
            # 1. Find if we have an else clause
            elseIdx = -1
            for i in range(0, ctx.getChildCount()):
                c = ctx.getChild(i)
                if isinstance(c, sexpParser.ItemContext) and c.getChildCount() == 1:
                    # This is a single item, so check if it's an else clause.
                    if c.getChild(0).getText() == "else":
                        elseIdx = i
                        break
            
            children = [c for c in ctx.getChildren()]
            
            # Grab condition right after if token
            cond_expr = ctx.getChild(2)
            # Grab all true statements up until the else clause
            true_stmts = children[3:elseIdx] if elseIdx != -1 else children[3:-1]
            # Grab all false statements after the else clause (if present)
            optional_false_stmts = children[elseIdx + 1:-1] if elseIdx != -1 else []
            
            return self.processIf(ctx, cond_expr, true_stmts, optional_false_stmts)
        elif token == "break":
            self.emit("break #sci:break")
        elif token == "breakif":
            self.processBreakIf(ctx.getChild(2))
        elif token == "continue":
            self.emit("continue #sci:continue")
        elif token == "contif":
            self.processContinueIf(ctx.getChild(2))
        elif token == "repeat":
            children = [c for c in ctx.getChildren()]
            
            repeat_stmts = children[2:-1]
            self.processRepeatLoop(
                repeat_stmts
            )
        elif token == "while":
            children = [c for c in ctx.getChildren()]
            
            cond_expr = children[2]
            while_stmts = children[3:-1]
            
            self.processWhileLoop(
                cond_expr,
                while_stmts
            )
        elif token == "switch":
            children = [c for c in ctx.getChildren()]

            # Find an optional else clause
            elseIdx = -1
            childIdx = 0
            for child in children:
                # Defensive: make sure this child has the shape you expect
                if child.getChildCount() >= 1:
                    subchild = child.getChild(0)
                    if subchild.getChildCount() >= 2:
                        maybe_else = subchild.getChild(1)
                        if isinstance(maybe_else, sexpParser.ItemContext) and maybe_else.getText() == "else":
                            elseIdx = childIdx
                            break
                childIdx +=1

            # Set all needed components            
            test_expr = children[2]
            case_arms = children[3:elseIdx] if elseIdx != -1 else children[3:]
            optional_else_arm_stmts = None
            if elseIdx != -1:
                optional_else_arm_stmts = [c for c in children[elseIdx].getChild(0).getChildren()][2:-1]
            
            return self.processSwitch(
                test_expr,
                case_arms,
                optional_else_arm_stmts    
            )
        elif token == "cond":
            return "cond <cond1> <cond2> ... { <case1> <case2> ... }"
        elif token == "for":
            children = [c for c in ctx.getChildren()]
            self.processForLoop([c for c in ctx.getChild(2).getChild(0).getChildren()][1:-1], 
                ctx.getChild(3),
                children[5:-1], 
                [c for c in ctx.getChild(4).getChild(0).getChildren()][1:-1])
        elif token in set(['+', '-', '*', '/', '%', '==', '!=', '<', '<=', '>', '>=', 'and', 'or']):
            # rewrite n-ary expression from prefix form to infix form
            visitedExprs = [self.visit(c) for c in ctx.getChildren()][2:-1]
            tokenWithSpacing = " " + token + " "

            # TODO: I can do constant folding if the left and right are both number types.

            result = "(" + tokenWithSpacing.join(visitedExprs) + ")"
            return result
        elif token in set(['=', '-=', '+=', '*=', '/=', '%=', '&=', '|=', '<<=', '>>=', '++', '--']):
            visitedExprs = [self.visit(c) for c in ctx.getChildren()][2:-1]

            INC_STMT = "{} += 1 #sci:++ inc stmt"
            DEC_STMT = "{} -= 1 #sci:-- dec stmt"

            match c := self.peekCtx():
                case ContextKind.STMT:  
                    # Statement form
                    match t := token:
                        case "++":
                            return INC_STMT.format(visitedExprs[0])
                        case "--":
                            return DEC_STMT.format(visitedExprs[0])
                        case _:
                            return "{} {} {}".format(visitedExprs[0], t, visitedExprs[1])
                case ContextKind.EXPR:
                    # Expression form, must produce side-effect statements for these operators.
                    match t := token:
                        case "++":
                            self.side_effect_stmts.append(INC_STMT.format(visitedExprs[0]))
                        case "--":
                            self.side_effect_stmts.append(DEC_STMT.format(visitedExprs[0]))
                        case _:
                            self.side_effect_stmts.append("{} {} {}".format(visitedExprs[0], token, visitedExprs[1]))
                    # Finally, just return the expression without the unary component.
                    return "{}".format(visitedExprs[0]) 
        else:
            # For now, else will be considered function calls!
            # Rewrite rules
            #   1. Cascade send call: (gEgo normal: 0 posn: 75 152) => gEgo._send("normal:", 0, "posn:" 75, 152)
            #   2. Function call: (Display "1" dsCOLOR clBLUE) => Display("1", dsCOLOR, clBLUE)

            # 1. Cascade send call
            children = [c for c in ctx.getChildren()][1:-1]
            
            # If we sniff out method or property selectors which end with a colon
            # then we know this is a cascade send call.
            selectors = set([c.getText() for c in children if c.getText().endswith(":")])
            
            if selectors:
                sender = self.visit(children[0])
                remaining_components = []
                for c in children[1:]:
                    if c.getText() in selectors:
                        # This is a selector
                        remaining_components.append("'{}'".format(c.getText()))
                    else:
                        # This is a parameter and therefore any expression which
                        # may have side-effects.
                        hoisted_stmts, expr = self.maybeHoistSideEffectsFromExpr(c)
                        self.maybe_emit_hoisted_stmts(hoisted_stmts)
                        remaining_components.append(expr)
                return "{}._send(".format(sender) + ", ".join(remaining_components) + ")" 
            
            # 2. Regular Function call
            # Ensure hoisting occurs for side-effect expressions and process from left to right me thinks.
            remaining_children = [c for c in ctx.getChildren()][2:-1]
            evaled_params = []
            for c in remaining_children:
                hoisted_stmts, expr = self.maybeHoistSideEffectsFromExpr(c)
                evaled_params.append(expr)
                self.maybe_emit_hoisted_stmts(hoisted_stmts)
            return "{}({})".format(token, ", ".join(evaled_params))
    
    def processStmt(self, stmt):
        # * Valid Statements
        #   * Assignment (=)
        #   * Augmented assignment (++, --, +=, -=, &=, *=, etc)
        #   * Function call/Method call/Procedure call/Cascade Send
        #   * Return statement
        #   * If statement (not if expressions!)
        #   * while statement
        #   * repeat statement
        #   * for statement
        #   * switch statement (not switch expressions)
        #   * cond statement (not cond expressions)
        #   * break/breakif statement
        #   * continue/continueif statement
        #   * ??? what else
        
        # TODO: When I'm processing statements, I could use them as-is.
        # += 1 => ok
        # ++ => += 1
        # /= => ok
        # etc.

        # No paranthesis are needed when statement form.
        # lhs <assign-op | augmented-assign-op> rhs
        pass

    def maybeHoistSideEffectsFromExpr(self, expr):
        '''For expressions that have side-effects, we must hoist the work out using temporaries.
        Alternatively, perhaps we can cheat all the side-effect magic using the Int16 wrapper class
        that we're going to need anyway.'''
        self.pushCtx(ContextKind.EXPR)
        expr = self.visit(expr)
        
        if self.side_effect_stmts:
            stmts = self.side_effect_stmts[:]
            self.side_effect_stmts.clear()
            self.popCtx()
            return stmts, expr
        
        self.popCtx()
        return None, expr

    def maybe_emit_hoisted_stmts(self, hoisted_stmts):
        if hoisted_stmts:
            for stmt in hoisted_stmts:
                self.emit(stmt + " # side-effect op hoisted up as statement")

    def processContinueIf(self, cond_expr):
        # 0. First, ensure that the condition expression is not a side-effect expression.
        hoisted_stmts, modified_expr = self.maybeHoistSideEffectsFromExpr(cond_expr)
        self.maybe_emit_hoisted_stmts(hoisted_stmts)

        # 1. Next, emit continue if condition
        self.emit("if {}: #sci:continueif".format(modified_expr))
        with IndentCtx(self):
            self.emit("continue")

    def processBreakIf(self, cond_expr):
        # 0. First, ensure that the condition expression is not a side-effect expression.
        hoisted_stmts, modified_expr = self.maybeHoistSideEffectsFromExpr(cond_expr)
        self.maybe_emit_hoisted_stmts(hoisted_stmts)

        # 1. Next, emit break if condition
        self.emit("if {}: #sci:breakif".format(modified_expr))
        with IndentCtx(self):
            self.emit("break")

    def processRepeatLoop(self, body_statements):
        # 1. SCI repeat loop => Python3 while True loop
        self.emit("while True: #sci:repeat")
                
        # 2. Next, emit body statement(s)
        with IndentCtx(self):
            for stmt in body_statements:
                self.emit(self.visit(stmt))

    def processWhileLoop(self, cond_expr, body_statements):
        self.emit()

        # 0. First, ensure that the condition expression is not a side-effect expression.
        hoisted_stmts, modified_expr = self.maybeHoistSideEffectsFromExpr(cond_expr)
        self.maybe_emit_hoisted_stmts(hoisted_stmts)

        # 1. Next, for loop as while and condition
        self.emit("while {}: #sci:whilecond".format(modified_expr))
        
        # 2. Next, emit body statement(s)
        with IndentCtx(self):
            for stmt in body_statements:
                self.emit(self.visit(stmt))

    def processForLoop(self, preinit_statements, cond_expr, body_statements, postinit_statements):
        self.emit()

        # 0. First, hoist out any side-effect type operators.
        hoisted_stmts, modified_expr = self.maybeHoistSideEffectsFromExpr(cond_expr)
        self.maybe_emit_hoisted_stmts(hoisted_stmts)

        # 1. Next, emit preinit statemet(s)
        # NOTE: Multiple preinit statements are allowed in SCI.
        for stmt in preinit_statements:
            s = self.visit(stmt) + " #sci:forpreinit"
            self.emit(s)
        
        # 2. Next, for loop as while and condition
        self.emit("while {}: #sci:forcond".format(modified_expr))

        with IndentCtx(self):
            # 3. Next, emit body statement(s)
            for stmt in body_statements:
                self.emit(self.visit(stmt))

            # 4. Next, emit postinit statement(s)
            # NOTE: Multiple postinit statements are allowed in SCI.
            for stmt in postinit_statements:
                s = self.visit(stmt)
                self.emit(s + " #sci:forpostinit")

    def processSwitch(self, test_expr, case_arms, optional_else_arm_stmts):
        '''As far as I can tell, SCI case tests should always be a single expression.
        Maybe, even only ever a single atom.
        '''
        if self.peekCtx() == ContextKind.EXPR:
            # SCI switch-expression => Python3 HOISTED match-case statements
            # with last statement captured to tmp variables and finally
            # modified test_expr replaced with tmp variable.
            
            # First, hoist out side-effect type operators out of test_expr.
            return "(switch-expression TODO)"
        else:
            # SCI switch-case statement => Python3 match-case statements

            # First, hoist out any side-effect type operators.
            hoisted_stmts, modified_expr = self.maybeHoistSideEffectsFromExpr(test_expr)
            self.maybe_emit_hoisted_stmts(hoisted_stmts)
            
            self.emit("match {}: #sci:switch (statement form)".format(modified_expr))
            with IndentCtx(self):
                # BUG: Hacking because somehow TerminalNodes are getting passed in.
                case_arms = [a for a in case_arms if not isinstance(a, TerminalNode)]

                if not case_arms:
                    # No case arms, so emit a pass statement
                    self.emit("pass" + " # no sci:case arms present")
                    return

                # Next, iterate and emit each case arm
                for case_arm in case_arms:
                    # Pull out key and statement(s) for each case arm
                    case_key = case_arm.getChild(0).getChild(1)
                    case_stmts = [c for c in case_arm.getChild(0).getChildren()][2:-1]

                    # 1. First, emit the case arm
                    self.emit("case {}:".format(self.visit(case_key)))

                    # 2. Next, emit the body statement(s)
                    with IndentCtx(self):
                        for stmt in case_stmts:
                            self.emit(self.visit(stmt))
                
                # Next, emit the optional else arm
                if optional_else_arm_stmts is not None:
                    self.emit("case _: #sci:switchelse")
                    with IndentCtx(self):
                        for stmt in optional_else_arm_stmts:
                            self.emit(self.visit(stmt))

    def processIf(self, ctx, cond_expr, true_stmts, optional_false_stmts):
        if self.peekCtx() == ContextKind.EXPR:
            # TODO: Expression needs to be handled in various styles
            # 1. Use Python3 ternary when the trueBody or elseBody is a single result expression (one line).
            # 2. Use Python3 if-else statements when the trueBody and/or elseBody are multiple statements.
            #    This is because ternary won't work with multiple statements in trueBody or elseBody.
            
            # 1. SCI if-expression => Python3 ternary expression
            # TODO: Python3 expression MUST have an else clause.
            #   I need to figure out what to do when an else is not used.
            # TODO: Prove that all SCI if expressions have a single statement per true/else bodies.
            #   Since Python3 if ternary only supports single expression in the true/else bodies.
            assert(len(true_stmts) == 1, "Python3 if expression form requires a single statement")
            assert(len(optional_false_stmts) == 1, "Python3 if expression form requires an else clause")
            # open file

            # First, hoist out side-effect type operators out of cond_expr.
            hoisted_stmts, modified_expr = self.maybeHoistSideEffectsFromExpr(cond_expr)
            self.maybe_emit_hoisted_stmts(hoisted_stmts)

            a = true_stmts[0]
            b = optional_false_stmts[0]
            return "({} if {} else {})".format(self.visit(a), modified_expr, self.visit(b))

            # 2. SCI if-expression => Python3 if-else statement due to multiple statements!
            # TODO
        else:
            # Check for comments
            # This is not working and will need to investigate.
            # comments = self.get_comments_before(ctx, self.token_stream)
            # for comment in comments:
            #     self.emit(f"# {comment}")

            # Do statement form
            hoisted_stmts, new_cond_expr = self.maybeHoistSideEffectsFromExpr(cond_expr)
            self.maybe_emit_hoisted_stmts(hoisted_stmts)
            self.emit("if {}:".format(new_cond_expr))

            if not true_stmts:
                # No true statements, so emit a pass statement
                with IndentCtx(self):
                    self.emit("pass")
            else:
                # Emit the true statements
                with IndentCtx(self):
                    for stmt in true_stmts:
                        self.emit(self.visit(stmt))

                # If we have an else clause, then emit it.
                if optional_false_stmts:
                    self.emit("else:")
                    with IndentCtx(self):
                        for stmt in optional_false_stmts:
                            self.emit(self.visit(stmt))

    def processReturn(self, optionalExp):
        if optionalExp is not None:
            if isinstance(optionalExp, sexpParser.AtomContext):
                # Return value is a scalar (atom)
                self.pushCtx(ContextKind.EXPR)
                self.emit("return ({})".format(self.visit(optionalExp)))
                self.popCtx()
            else:
                # Return value is a more complex expression so needs paranthesis
                hoisted_stmts, ret_expr = self.maybeHoistSideEffectsFromExpr(optionalExp)
                self.maybe_emit_hoisted_stmts(hoisted_stmts)
                self.emit("return {}".format(ret_expr))
        else:
            # No return value
            self.emit("return")

    def extractNormalParams(self, parameters):
        '''Extracts all normal function parameters up until any potential
        &tmp delimiter that may be present'''
        try:
            tmp_idx = parameters.index("&tmp")
        except ValueError:
            return parameters
        return parameters[0:tmp_idx]

    def extractTmpParams(self, parameters):
        '''Extracts all &tmp parameters that SCI requires but are fully uncessary
        in Python-land. However, these become emitted as comments to aid in debugging'''
        try:
            tmp_idx = parameters.index("&tmp")
        except ValueError:
            return []
        return parameters[tmp_idx + 1:]

    def processPublicList_(self, children):
        for x in range(0, len(children), 2):
            self.emit("  \"{}\" : {},".format(children[x].getText(), children[x+1].getText()))

    def processProcedure(self, name, parameters, body_statements):
        # 0. Extract out the noraml vs tmp params, as Python3 will only use tmp for comments
        norm_params = self.extractNormalParams(parameters)
        tmp_params = self.extractTmpParams(parameters)

        self.emit("\n@SCI.procedure")
        self.emit("def {}({}):".format(name, ", ".join(norm_params)))

        with IndentCtx(self):
            if tmp_params:
                self.emit("# sci:&tmp => {}".format(", ".join(tmp_params)))

            # Sniff check, for possible argc usage in the body.
            argc_visitor = ArgCVisitor(self.emit)
            for stmt in body_statements:
                argc_visitor.visit(stmt)

            argc_visitor.emit()

            for stmt in body_statements:
                self.emit(self.visit(stmt))

def main():
    input_stream = FileStream("sci_files/buildup.sc")
    lexer = sexpLexer(input_stream)
    stream = CommonTokenStream(lexer)
    parser = sexpParser(stream)
    tree = parser.sexpr()

    #print(tree.toStringTree(recog=parser))
    visitor = SexpCustomVisitor()
    result = visitor.visit(tree)
    print(result)
    

if __name__ == '__main__':
    main()