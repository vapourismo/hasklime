import sys
sys.path.append('..')

import threading
from hasklime import *

library = Library('./hasklime-example.so')

f = library.wrap('f', Bool, Bool)
g = library.wrap('g', ByteString, ByteString)

print(g(b'True'))
print(g(b'False'))
