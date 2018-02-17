import ctypes
import json
import threading

FreeFunPtr = ctypes.CFUNCTYPE(None, ctypes.c_void_p)

class JSON:
	""" JSON-encoded string """

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

class StablePtr:
	""" Stable pointer """

	unpackType = ctypes.c_void_p
	packType   = ctypes.c_void_p

	def unpack(library, ptr):
		return StablePtr(library, ptr)

	def pack(library, ref):
		return ref.ptr

	def __init__(self, library, ptr):
		self.library = library
		self.ptr = ptr

	def __str__(self):
		return '<StablePtr %s from %s>' % (self.ptr, self.library)

	def __del__(self):
		self.library.freeStablePtr(self.ptr)

def wrapFunction(library, fun, returnClass = None, *paramClasses):
	""" Wrap the given function to make it easily usable. """

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
	""" Haskell library """

	def __init__(self, path):
		""" Load the library at the given path. """

		self.path = path
		self.library = ctypes.CDLL(path)

		self.freePtr = self.library.freePtr
		self.freePtr.argtypes = [ctypes.c_void_p]
		self.freePtr.restype = None

		self.freeStablePtr = self.library.freeStablePtr
		self.freeStablePtr.argtypes = [ctypes.c_void_p]
		self.freeStablePtr.restype = None

	def wrap(self, name, ret = None, *params):
		""" Wrap a function with the given name.  """
		return wrapFunction(self, getattr(self.library, name), ret, *params)

	def __str__(self):
		return '<Library %s>' % self.library
import ctypes
import json
import threading

FreeFunPtr = ctypes.CFUNCTYPE(None, ctypes.c_void_p)

class JSON:
	""" JSON-encoded string """

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

class StablePtr:
	""" Stable pointer """

	unpackType = ctypes.c_void_p
	packType   = ctypes.c_void_p

	def unpack(library, ptr):
		return StablePtr(library, ptr)

	def pack(library, ref):
		return ref.ptr

	def __init__(self, library, ptr):
		self.library = library
		self.ptr = ptr

	def __str__(self):
		return '<StablePtr %s from %s>' % (self.ptr, self.library)

	def __del__(self):
		self.library.freeStablePtr(self.ptr)

def wrapFunction(library, fun, returnClass = None, *paramClasses):
	""" Wrap the given function to make it easily usable. """

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
	""" Haskell library """

	def __init__(self, path):
		""" Load the library at the given path. """

		self.path = path
		self.library = ctypes.CDLL(path)

		self.freePtr = self.library.freePtr
		self.freePtr.argtypes = [ctypes.c_void_p]
		self.freePtr.restype = None

		self.freeStablePtr = self.library.freeStablePtr
		self.freeStablePtr.argtypes = [ctypes.c_void_p]
		self.freeStablePtr.restype = None

	def wrap(self, name, ret = None, *params):
		""" Wrap a function with the given name.  """
		return wrapFunction(self, getattr(self.library, name), ret, *params)

	def __str__(self):
		return '<Library %s>' % self.library
