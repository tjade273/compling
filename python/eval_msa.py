import sys
import os

from lingpy.evaluate.apa import EvalMSA
from lingpy.align.sca import MSA

gold_dir = sys.argv[1]
test_dir = sys.argv[2]

r = 0
c = 0
sp = 0 

i = 0

for filename in os.listdir(test_dir):
    if filename.endswith(".psa"):
        e = EvalMSA(MSA(os.path.join(gold_dir, filename)),
                        MSA(os.path.join(test_dir, filename)))
        r += e.r_score()
        c += e.c_score()
        sp += e.sp_score()

        i+= 1

print('r score: %f\n c score: %f\n sp score: %f' % (r/i, c/i, sp/i))
        
        
        
