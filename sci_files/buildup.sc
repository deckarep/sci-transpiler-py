;;; Sierra Script 1.0 - (do not remove this comment)
;;; Decompiled by sluicebox
(script# 999)
(include sci.sh)
(use Main)
(use Print)

(public
	sign 0
	umod 1
	Min 2
	Max 3
	InRect 4
	OneOf 5
	WordAt 6
	Eval 7
)

(procedure (mega x y z &tmp i j)
    ; cascade send call
    (= i 2)
    (gEgo normal: (= i 0) posn: (+ 75 3) (*= i 152))

    ; if empty
    (if (< 3 6)
        ; wow, such empty
    )

    ; if expression open questions
    ; is it possible for if expressions to be multi-statement for the bodies?

    ; if expression
    (gMessager say: 1 0 (if (IsFlag 17) 9 else 8) 0 self)

    ; if expression with side-effects
    (gMessager say: 1 0 (if (= x (hide 2 3)) 9 else 8) 0 self)

    ; if statement only true body
    (if (not elements)
        (hello "King Graham")
    )

    ; if statement empty true
    (if (not elements)
        ; nothing
    )

    ; if statement empty true and else
    (if (not elements)
        ; nothing
    else
        ;nothing else
    )

     ; if statement w/true empty else
    (if (not elements)
        (sayHi "hi")
    else
        ; nothing
    )

    ; if statement empty true else w/ stmts
    (if (not elements)
        ; nothing
    else
        (sayHi "hi")
    )

    ; if statement multiline
    (if (not elements)
        (++ y)
        (++ y)
        (++ y)
        (++ y)
    else
        (++ x)
        (++ x)
    )

    ; return statement by itself
    (return)

    ; return with string atom
    (return "foo")

    ; return with number atom
    (return 45)

    ; return with expression
    (return (+ 3 2))

    ; return with side effects
    (return (++ x))

    ; return with complex expression
    (return (if (> 3 1) 20 else 40))

    ; return can have any amount of abitrary statements
    ; _ret = ... must be used for the final statement of evry branch
    ;(return
    ;   (if condA
    ;        (if condB
    ;            (foo)
    ;        else
    ;            (bar)
    ;        )
    ;    else
    ;        (baz)
    ;    )
    ;)

    ;if condA:
    ;    if condB:
    ;        _ret = foo()
    ;    else:
    ;        _ret = bar()
    ;else:
    ;    _ret = baz()
    ;
    ;return _ret

    ; TODO: for loop issue
    ; Currently c-style for loops compile to Python while True
    ; If the c-style has a continue, the Python while postinit
    ; will be skipped but it should be run always.

    ; for loop with side-effects in cond_expr
    (for ((= i 0)) (< (= x 2) 10) ((++ i))
        (doSomething 3 2 1)
    )
	
    ; Nested for loop
    (for ((= i 0)) (< i (- argc 1)) ((++ i))
		(for ((= j 0)) (< j 10) ((++ j))
            (if (== what [things (+ i j)])
                (return (or what 1))
            )
        )
	)

    ; traditional while loop - empty body
    (while (and (foo 23) (bar 7))
        ; wow, such empty
    )

    ; traditional while loop
    (while (< 3 (*= n 2))
        (++ y)
        (self doit:)
        (if (not elements)
	        (= elements (NewList))
	    )
    )

    ; repeat loop empty
    (repeat
        ; nothing to see here
    )

    ; repeat loop
    (repeat
        (++ y)
        (self doit:)
        (repeat
            (++ x)
            (break)

            (breakif (> x 5))

            (continue)

            (contif (== global600 53))
        )
        (if (not elements)
	        (= elements (NewList))
            (if (not elements)
	            (= elements (NewList))
            else
                (= x (+ 2 x))
            )
        else
            (= x (+ 2 x))
	    )
    )

    ; switch that is empty
    (switch i
        ; so much empty
    )

    ; switch statement/expressions
    ; often appear as expressions
    ; else is optional
    ; each arm may have multiple statements
    ; when used as an expression the last
    ; statement must be captured in a tmp var per arm
    ; and the entire logic must be hoisted above expression
    (switch i
        (0
            (Display "0" dsCOLOR clLIME)
            (++ y)
            (switch j
                (1
                    (repeat
                        (Display "1" dsCOLOR clBLUE)
                    )
                    (Display "1" dsCOLOR clBLUE)
                )
                (7
                    (Display "7" dsCOLOR clYELLOW)
                )
            )
        )
        (1
            (Display "1" dsCOLOR clBLUE (or 1 i) (+= i 3))
            (Display "1" dsCOLOR clBLUE)
        )
        (7
            (Display "7" dsCOLOR clYELLOW)
        )
        (12
            (Display "C" dsCOLOR clCYAN)
        )
        (else
            (Display "C" dsCOLOR clPURPLE)
        )
    )

    ; cond statments/expressions
    ; may appear as expressions
    ; else is optional
    ; each arm may have multiple statements
    ; when used as expressions that last
    ; statement must be captured in a tmp var per arm
    ; and the entire logic must be hoisted above expression
    ; TODO

    ;basic cond
    (cond
        ((< (gEgo x:) 100)
            (Prints "You're on the left side of the screen")
            (say "Hi Larry!")
        )
        ((> (gEgo x:) 220)
            (Prints "You're on the right side of the screen")
            (say "Hi Patty!")
        )
        (else
            (++ y)
            (++ x)
        )
    )

    ; basic cond no else
    (cond
        ((< (gEgo x:) 100)
            (say "Hi Al")
        )
    )

	(return -info-:)
)

(procedure (sign x)
	(return
		(if (< x 0)
			-1
		else
			(> x 0)
		)
	)
)

(procedure (umod n modN)
	(if (< (-= n (* modN (/ n modN))) 0)
		(+= n modN)
	)
	(return (or 1 (+= n 3)))
)

(procedure (Min nums &tmp otherMin)
	(return
		(if (or (== argc 1) (< nums (= otherMin (Min &rest))))
			nums
		else
			otherMin
		)
	)
)

(procedure (Max nums &tmp otherMax)
	(return
		(if (or (== argc 1) (> nums (= otherMax (Max &rest))))
			nums
		else
			otherMax
		)
	)
)

(procedure (InRect lEdge tEdge rEdge bEdge ptxOrObj pty)
	(return
		(and
			(<=
				lEdge
				(if (< argc 6)
					(ptxOrObj x:)
				else
					ptxOrObj
				)
				rEdge
			)
			(<=
				tEdge
				(if (< argc 6)
					(ptxOrObj y:)
				else
					pty
				)
				bEdge
			)
		)
	)
)

(procedure (OneOf what things &tmp i)
	(for ((= i 0)) (< i (- argc 1)) ((++ i))
		(if (== what [things i])
			(return (or what 1))
		)
	)
	(return 0)
)

(procedure (WordAt ptr n)
    (Buttface (*= z (/ 4 (Foo (*= x 2)))) (+= n (*= n 2)))
	(Memory memPEEK (+ ptr (* 2 n)))
)

(procedure (Eval obj sel)
	(obj sel: &rest)
)

(class Obj
	(properties)

	(method (new)
		(Clone self)
	)

	(method (init))

	(method (doit)
		(return self)
	)

	(method (dispose)
		(DisposeClone self)
	)

	(method (showStr where)
		(StrCpy where name)
	)

	(method (showSelf &tmp [str 200])
		(Prints (self showStr: @str))
	)

	(method (perform theCode)
		(theCode doit: self &rest)
	)

	(method (respondsTo selector)
		(RespondsTo self selector)
	)

	(method (isMemberOf what)
		(return
			(or
				(== what self)
				(and
					(& (what -info-:) $8000)
					(not (& -info- $8000))
					(== -propDict- (what -propDict-:))
				)
			)
		)
	)

	(method (isKindOf what &tmp theSuper)
		(return
			(or
				(and
					(== -propDict- (what -propDict-:))
					(== -classScript- (what -classScript-:))
				)
				(and
					(= theSuper (self -super-:))
					(IsObject theSuper)
					(theSuper isKindOf: what)
				)
			)
		)
	)

	(method (yourself)
		(return self)
	)
)

(class Code of Obj
	(properties)

	(method (doit))
)

(class Collect of Obj
	(properties
		elements 0
		size 0
	)

	(method (doit)
		(self eachElementDo: #doit &rest)
	)

	(method (showStr where)
		(Format where 999 0 name size) ; "%s [Collection of size %d]"
	)

	(method (showSelf &tmp [str 40])
		(Prints (self showStr: @str))
		(self eachElementDo: #showSelf)
	)

	(method (add item &tmp obj n node)
		(if (not elements)
			(= elements (NewList))
		)
		(for ((= n 0)) (< n argc) ((++ n))
			(if (not (self isDuplicate: [item n]))
				(AddToEnd elements (NewNode [item n] [item n]))
				(++ size)
			)
		)
		(return self)
	)

	(method (delete item &tmp n)
		(for ((= n 0)) (< n argc) ((++ n))
			(if (DeleteKey elements [item n])
				(-- size)
			)
		)
		(return self)
	)

	(method (dispose)
		(if elements
			(self eachElementDo: #dispose)
			(DisposeList elements)
		)
		(= size (= elements 0))
		(super dispose:)
	)

	(method (first)
		(FirstNode elements)
	)

	(method (next node)
		(NextNode node)
	)

	(method (isEmpty)
		(return (or (== elements 0) (EmptyList elements)))
	)

	(method (contains anObject)
		(FindKey elements anObject)
	)

	(method (eachElementDo action &tmp node nextNode obj)
		(for ((= node (FirstNode elements))) node ((= node nextNode))
			(= nextNode (NextNode node))
			(if (not (IsObject (= obj (NodeValue node))))
				(return)
			)
			(obj action: &rest)
		)
	)

	(method (firstTrue action &tmp node nextNode obj)
		(for ((= node (FirstNode elements))) node ((= node nextNode))
			(= nextNode (NextNode node))
			(= obj (NodeValue node))
			(if (obj action: &rest)
				(return obj)
			)
		)
		(return 0)
	)

	(method (allTrue action &tmp node nextNode obj)
		(for ((= node (FirstNode elements))) node ((= node nextNode))
			(= nextNode (NextNode node))
			(= obj (NodeValue node))
			(if (not (obj action: &rest))
				(return 0)
			)
		)
		(return 1)
	)

	(method (release &tmp node nextNode)
		(for ((= node (FirstNode elements))) node ((= node nextNode))
			(= nextNode (NextNode node))
			(self delete: (NodeValue node))
		)
	)

	(method (isDuplicate)
		(return 0)
	)
)

(class List of Collect
	(properties)

	(method (showStr where)
		(Format where 999 1 name size) ; "%s [List of size %d]"
	)

	(method (at n &tmp node)
		(for
			((= node (FirstNode elements)))
			(and n node)
			((= node (NextNode node)))
			
			(-- n)
		)
		(return
			(if node
				(NodeValue node)
			else
				0
			)
		)
	)

	(method (last)
		(LastNode elements)
	)

	(method (prev node)
		(PrevNode node)
	)

	(method (addToFront obj &tmp n)
		(if (not elements)
			(= elements (NewList))
		)
		(for ((= n (- argc 1))) (<= 0 n) ((-- n))
			(if (not (self isDuplicate: [obj n]))
				(AddToFront elements (NewNode [obj n] [obj n]))
				(++ size)
			)
		)
		(return self)
	)

	(method (addToEnd obj &tmp n)
		(if (not elements)
			(= elements (NewList))
		)
		(for ((= n 0)) (< n argc) ((++ n))
			(if (not (self isDuplicate: [obj n]))
				(AddToEnd elements (NewNode [obj n] [obj n]))
				(++ size)
			)
		)
		(return self)
	)

	(method (addAfter el obj &tmp n num insertNode)
		(if (= insertNode (FindKey elements el))
			(-- argc)
			(for ((= n 0)) (< n argc) ((++ n))
				(if (not (self isDuplicate: [obj n]))
					(= insertNode
						(AddAfter
							elements
							insertNode
							(NewNode [obj n] [obj n])
						)
					)
					(++ size)
				)
			)
		)
		(return self)
	)

	(method (indexOf obj &tmp n node)
		(= n 0)
		(for ((= node (FirstNode elements))) node ((= node (NextNode node)))
			(if (== obj (NodeValue node))
				(return n)
			)
			(++ n)
		)
		(return -1)
	)
)

(class Set of List
	(properties)

	(method (showStr where)
		(Format where 999 2 name size) ; "%s [Set of size %d]"
	)

	(method (isDuplicate obj)
		(self contains: obj)
	)
)

(class EventHandler of Set
	(properties)

	(method (handleEvent event &tmp node nextNode obj evtClone ret)
		(= evtClone (Clone event))
		(for
			((= node (FirstNode elements)))
			(and node (not (evtClone claimed:)))
			((= node nextNode))
			
			(= nextNode (NextNode node))
			(breakif (not (IsObject (= obj (NodeValue node)))))
			(obj handleEvent: evtClone)
		)
		(= ret (evtClone claimed:))
		(evtClone dispose:)
		(return ret)
	)
)

(class Script of Obj
	(properties
		client 0
		state -1
		start 0
		timer 0
		cycles 0
		seconds 0
		lastSeconds 0
		ticks 0
		lastTicks 0
		register 0
		script 0
		caller 0
		next 0
	)

	(method (doit &tmp thisSeconds)
		(if script
			(script doit:)
		)
		(cond
			(cycles
				(if (not (-- cycles))
					(self cue:)
				)
			)
			(seconds
				(= thisSeconds (GetTime 1)) ; SysTime12
				(if (!= lastSeconds thisSeconds)
					(= lastSeconds thisSeconds)
					(if (not (-- seconds))
						(self cue:)
					)
				)
			)
			((and ticks (<= (-= ticks (Abs (- gGameTime lastTicks))) 0))
				(= ticks 0)
				(self cue:)
			)
		)
		(= lastTicks gGameTime)
	)

	(method (init who whoCares reg)
		(= lastTicks gGameTime)
		(if (>= argc 1)
			((= client who) script: self)
			(if (>= argc 2)
				(= caller whoCares)
				(if (>= argc 3)
					(= register reg)
				)
			)
		)
		(= state (- start 1))
		(self cue:)
	)

	(method (dispose &tmp theNextScript)
		(if (IsObject script)
			(script dispose:)
		)
		(if (IsObject timer)
			(timer dispose:)
		)
		(if (IsObject client)
			(client
				script:
					(= theNextScript
						(cond
							((IsObject next) next)
							(next
								(ScriptID next)
							)
						)
					)
			)
			(cond
				((not theNextScript) 0)
				((== gNewRoomNum gCurRoomNum)
					(theNextScript init: client)
				)
				(else
					(theNextScript dispose:)
				)
			)
		)
		(if (and (IsObject caller) (== gNewRoomNum gCurRoomNum))
			(caller cue: register)
		)
		(= script (= timer (= client (= next (= caller 0)))))
		(super dispose:)
	)

	(method (changeState newState)
		(= state newState)
	)

	(method (cue)
		(if client
			(self changeState: (+ state 1) &rest)
		)
	)

	(method (setScript newScript)
		(if (IsObject script)
			(script dispose:)
		)
		(if newScript
			(newScript init: self &rest)
		)
	)

	(method (handleEvent event)
		(if script
			(script handleEvent: event)
		)
		(event claimed:)
	)
)

(class Event of Obj
	(properties
		type 0
		message 0
		modifiers 0
		y 0
		x 0
		claimed 0
		port 0
	)

	(method (new mask &tmp event)
		(= event (super new:))
		(GetEvent (if argc mask else 32767) event)
		(return event)
	)

	(method (localize &tmp curPort)
		(if (not (& type $4000))
			(= curPort (GetPort))
			(cond
				((not port)
					(GlobalToLocal self)
				)
				((!= port curPort)
					(SetPort port)
					(LocalToGlobal self)
					(SetPort curPort)
					(GlobalToLocal self)
				)
			)
			(= port curPort)
		)
		(return self)
	)

	(method (globalize &tmp curPort)
		(if (not (& type $4000))
			(= curPort (GetPort))
			(cond
				((== port curPort)
					(LocalToGlobal self)
				)
				(port
					(SetPort port)
					(LocalToGlobal self)
					(SetPort curPort)
				)
			)
			(= port 0)
		)
		(return self)
	)
)

(class Cursor of Obj
	(properties
		view 0
		loop 0
		cel 0
		x 0
		y 0
		hotSpotX 0
		hotSpotY 0
		hidden 1
	)

	(method (init)
		(if (or hotSpotX hotSpotY)
			(SetCursor view loop cel hotSpotX hotSpotY)
		else
			(SetCursor view loop cel)
		)
	)

	(method (posn theX theY)
		(SetCursor theX theY)
	)

	(method (posnHotSpot theX theY)
		(= hotSpotX theX)
		(= hotSpotY theY)
		(self init:)
	)

	(method (setLoop whichLoop)
		(= loop whichLoop)
		(self init:)
	)

	(method (setCel whichCel)
		(= cel whichCel)
		(self init:)
	)

	(method (showCursor trueOrFalse)
		(if argc
			(= hidden trueOrFalse)
			(SetCursor hidden)
		)
	)
)
