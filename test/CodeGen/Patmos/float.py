#!/usr/bin/env python

import sys
from struct import *

flnum = float(sys.argv[1])


# > ... big endian (patmos)
# < ... little endian
formats = { 'Float':'>f', 'Double':'>d' }


def print_info(fmt, flnum):
  packed = pack(formats[fmt],flnum)
  tup = [ord(c) for c in packed]
  # comute decimal
  res = 0
  for byt in tup: res = (res << 8) + byt
  # 2 parts
  if len(tup)==8:
    part1 = part2 = 0
    for byt in tup[0:4]: part1 = (part1 << 8) + byt
    for byt in tup[4:8]: part2 = (part2 << 8) + byt
  # output
  print fmt
  print '  Dec:', res
  if len(tup)==8:
    print '    bytes 0-3:', part1
    print '    bytes 4-7:', part2
  print '  Hex:', hex(res)[2:].rjust(len(tup)*2,'0')
  print '  Bin:', bin(res)[2:].rjust(len(tup)*8,'0')
  print '  from', unpack(formats[fmt], packed)[0]

print_info('Float', flnum)
print_info('Double', flnum)
