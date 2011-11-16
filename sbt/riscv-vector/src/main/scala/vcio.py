"""
Convert Verilog module declaration to Chisel io bundle
"""

import sys
import re

def usage():
  print "python vcio.py <verilog module> > outputfile"
  exit()

if len(sys.argv) != 2:
  usage()
filename = sys.argv[1]

patterns = ["\s*(input|output)\s*(reg)*\s*\[`(\w+)-\d+:\d+\]\s+(\w+).*", # matches multibit input/output with defined width
            "\s*(input|output)\s*(reg)*\s*\[(\d+):\d+\]\s+(\w+).*", # ' ' with number width
            "\s*(input|output)\s*(reg)*\s*(\w+).*", # single bit wire
            "\s*//.*" # keep comments
            ]

# [0-9]+'[bhd][0-9a-fA-F] -> Bits("$2$3",$1)
# wire -> val

def mbit_const(m):
  print m.group(3)
  print "val {0}\t\t= UFix({1}, '{2});".format(m.group(4), m.group(3), m.group(1))

def mbit_num(m):
  width = int(m.group(3)) + 1
  print "val {0}\t\t= UFix({1}, '{2});".format(m.group(4), width, m.group(1))

def sbit(m):
  print "val {0}\t\t= Bool('{1});".format(m.group(3), m.group(1))

def comment(m):
  print m.group(0)

replace = {  0 : mbit_const,
    1 : mbit_num,
    2 : sbit,
    3 : comment
    }

compiled = []
for p in patterns:
  compiled.append(re.compile(p))

with open(filename, 'r') as f:
  for line in f:
    matched = False
    for idx, c in enumerate(compiled):
      m = c.match(line.rstrip())
      if m:
        matched = True
        replace[idx](m)
        break
    # if not matched:
    #  print line.rstrip()
