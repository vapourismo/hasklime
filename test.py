import ctypes
import json

sharedLibrary = ctypes.CDLL('./hasklime-example.so')

def wrap(func):
	test.argtypes = [ctypes.c_char_p]
	test.restype = ctypes.c_char_p

	def wrapper(input)

test = sharedLibrary.test
test.argtypes = [ctypes.c_char_p]
test.restype = ctypes.c_char_p

result = json.loads(test(bytes(json.dumps([1, 2, 3]), 'utf8')))
print(result)
