import sys
import os

path = sys.argv[1]
n = int(sys.argv[2])
for filename in os.listdir(path):
	lines = []
	with open(os.path.join(path,filename),'r') as f:
		l = f.readlines()
		lines = l[:n]+sorted(l[n:])
	with  open(os.path.join(path,filename), 'w') as f:
		f.writelines(lines)



