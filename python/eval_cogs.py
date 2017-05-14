import sys
import os

from lingpy.evaluate.acd import pairs
from lingpy.compare.lexstat import LexStat

print(pairs(LexStat(sys.argv[1])))
