import sys

from lingpy.evaluate.apa import EvalPSA
from lingpy.align.sca import PSA


gold = PSA(sys.argv[1])
test = PSA(sys.argv[2])

e = EvalPSA(gold, test)

print('r score: %f\n c score: %f\n sp score: %f' % (e.r_score(), e.c_score(), e.sp_score()))

e.diff() 

