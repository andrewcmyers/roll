roll: sources.cm toplevel/Main.sml ast/AbSyn.sml eval/eval-sig.sml	      \
    eval/distrib-sig.sml eval/sample-sig.sml eval/Evaluator.sml		      \
    eval/Distribution.sml eval/Sample.sml multiset/ms.sig multiset/ms-ineff.sml\
    multiset/ms.sml multiset/ms-eff.sml lcm/Random.sml lcm/Warm.sml parser/dice.lex\
    parser/dice.grm parser/Parser.sml toplevel/Roll.sml toplevel/Util.sml
	echo 'CM.make("sources.cm")' | sml

clean:
	rm -rf */.cm
	rm parser/dice.grm.*
	rm parser/dice.lex.*
