import ctypes
import json
import threading

FreeFunPtr = ctypes.CFUNCTYPE(None, ctypes.c_void_p)

class JSON:
	unpackType = ctypes.c_void_p
	packType   = ctypes.c_char_p

	def unpack(library, ptr):
		if ptr == 0 or ptr == None:
			return None

		string = ctypes.c_char_p(ptr).value
		value = json.loads(string)

		library.freePtr(ptr)
		return value

	def pack(_, value):
		return ctypes.c_char_p(bytes(json.dumps(value), 'utf8'))

class Ref:
	unpackType = ctypes.c_void_p
	packType   = ctypes.c_void_p

	def unpack(library, ptr):
		return Ref(library, ptr)

	def pack(library, ref):
		return ref.ptr

	def __init__(self, library, ptr):
		self.library = library
		self.ptr = ptr

	def __str__(self):
		return '<Ref %s from %s>' % (self.ptr, self.library)

	def __del__(self):
		self.library.freeStablePtr(self.ptr)

def wrapFunction(library, fun, returnClass = None, *paramClasses):
	returnType = None
	numParams  = len(paramClasses)
	paramTypes = list(paramClasses)

	for i in range(numParams):
		paramTypes[i] = paramClasses[i].packType

	if returnClass != None:
		returnType = returnClass.unpackType

	fun.argtypes = paramTypes
	fun.restype = returnType

	def wrapper(*inputParams):
		if len(inputParams) != numParams:
			raise Exception('Invalid number of parameters')

		params = list(inputParams)

		for i in range(numParams):
			params[i] = paramClasses[i].pack(library, inputParams[i])

		value = fun(*params)

		return returnClass.unpack(library, value)

	return wrapper

class Library:
	def __init__(self, path):
		self.path = path
		self.library = ctypes.CDLL(path)

		self.freePtr = self.library.freePtr
		self.freePtr.argtypes = [ctypes.c_void_p]
		self.freePtr.restype = None

		self.freeStablePtr = self.library.freeStablePtr
		self.freeStablePtr.argtypes = [ctypes.c_void_p]
		self.freeStablePtr.restype = None

	def wrap(self, name, ret = None, *params):
		return wrapFunction(self, getattr(self.library, name), ret, *params)

	def __str__(self):
		return '<Library %s>' % self.library

library = Library('./hasklime-example.so')

create = library.wrap('create', Ref, JSON)
dump = library.wrap('dump', JSON, Ref)
increment = library.wrap('increment', Ref, Ref)

threads = []

class T(threading.Thread):
	def __init__(self, i):
		threading.Thread.__init__(self)
		self.i = i

	def run(self):
		x = create(self.i)

		for j in range(10000):
			x = increment(x)

		x = dump(x)
		if x != self.i + 10000:
			print(self.i, x)

for i in range(32):
	t = T(i)
	t.start()
	threads.append(t)

for t in threads:
	t.join()
