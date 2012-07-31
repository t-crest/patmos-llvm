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
  # output
  print fmt
  print '  Dec:', res
  print '  Hex:', hex(res)[2:].rjust(len(tup)*2,'0')
  print '  Bin:', bin(res)[2:].rjust(len(tup)*8,'0')
  print '  from', unpack(formats[fmt], packed)[0]

print_info('Float', flnum)
print_info('Double', flnum)
