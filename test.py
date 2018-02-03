import ctypes
import json

library = ctypes.CDLL('./hasklime-example.so')

freeResponse_ = library.freeResponse
freeResponse_.argtypes = [ctypes.c_void_p]
freeResponse_.restype = None

def freeResponse(ptr):
	if ptr != 0 and ptr != None:
		freeResponse_(ptr)

freeEnvironment_ = library.freeEnvironment
freeEnvironment_.argtypes = [ctypes.c_void_p]
freeEnvironment_.restype = None

def freeEnvironment(ptr):
	if ptr != 0 and ptr != None:
		freeEnvironment_(ptr)

def makeRequest(obj):
	return ctypes.c_char_p(bytes(json.dumps(obj), 'utf8'))

def parseResponse(ptr):
	if ptr == 0 or ptr == None:
		return None
	else:
		return json.loads(ctypes.c_char_p(ptr).value)

def fromMethod(method):
	method.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
	method.restype = ctypes.c_void_p

	def wrapper(state, request):
		responsePtr = method(state.environment, makeRequest(request))
		response = parseResponse(responsePtr)

		freeResponse(responsePtr)

		return response

	return wrapper

def fromProperty(property):
	property.argtypes = [ctypes.c_void_p]
	property.restype = ctypes.c_void_p

	def wrapper(state):
		responsePtr = property(state.environment)
		response = parseResponse(responsePtr)

		freeResponse(responsePtr)

		return response

	return wrapper

def fromFunction(func):
	func.argtypes = [ctypes.c_char_p]
	func.restype = ctypes.c_void_p

	def wrapper(request):
		responsePtr = func(makeRequest(request))
		response = parseResponse(responsePtr)

		freeResponse(responsePtr)

		return response

	return wrapper

def bindEnvironment(artifact):
	def wrapper(self, *args, **kwargs):
		return artifact(self.environment, *args, **kwargs)

	return wrapper

class ActivationError(Exception):
	pass

def fromActivate(activate):
	activate.argtypes = [ctypes.c_char_p]
	activate.restype = ctypes.c_void_p

	class Environment:
		def __init__(self, request):
			self.environment = activate(makeRequest(request))

			if self.environment == 0 or self.environment == None:
				raise ActivationError('Failed to activate')

		def __del__(self):
			freeEnvironment(self.environment)

		def __str__(self):
			return '<Environment %s>' % self.environment

	return Environment

activate = fromActivate(library.testActivate)
method = fromMethod(library.testMethod)
property = fromProperty(library.testProperty)
function = fromFunction(library.testFunction)

env = activate(1)

print(method(env, 13))
print(property(env))
print(function(37))
