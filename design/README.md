This directory contains examples of some optimization cases that the plugin
tries to handle:

1) In join-constr-app.hs $j_sm2u must be inlined to eliminate SeqParseL and
   SeqParseR by case-of-case transformation.
2) In let-expr-fusion.hs $j_slCZ must be inlined to eliminate Yield, Skip and
   Stop by case-of-case transformation.
