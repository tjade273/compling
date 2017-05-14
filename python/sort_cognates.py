import sys
import os

path = sys.argv[1]
n = int(sys.argv[2])

def getint(s):
	return int(s.split('\t')[0])

for filename in os.listdir(path):
	lines = []
	with open(os.path.join(path,filename),'r') as f:
		l = f.readlines()
		lines = l[:n]+sorted(l[n:],key=getint)
	with  open(os.path.join(path,filename), 'w') as f:
		f.writelines(lines)



