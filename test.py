import ctypes
import json

def getFreeResponse(library):
	""" Extract the 'freeResponse' function from the given library. """

	freeResponse_ = library.freeResponse
	freeResponse_.argtypes = [ctypes.c_void_p]
	freeResponse_.restype = None

	def freeResponse(ptr):
		if ptr != 0 and ptr != None:
			freeResponse_(ptr)

	return freeResponse

def getFreeEnvironment(library):
	""" Extract the 'freeEnvironment' function from the given library. """

	freeEnvironment_ = library.freeEnvironment
	freeEnvironment_.argtypes = [ctypes.c_void_p]
	freeEnvironment_.restype = None

	def freeEnvironment(ptr):
		if ptr != 0 and ptr != None:
			freeEnvironment_(ptr)

	return freeEnvironment

def makeRequest(value):
	""" Turn the given value into a request parameter. """
	return ctypes.c_char_p(bytes(json.dumps(value), 'utf8'))

def parseResponse(ptr):
	""" Extract the response value from the given response pointer. """

	if ptr == 0 or ptr == None:
		return None
	else:
		return json.loads(ctypes.c_char_p(ptr).value)

def fromMethod(method, freeResponse):
	""" Adjust the given C function to fit the 'Method' interface. """

	method.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
	method.restype = ctypes.c_void_p

	def wrapper(state, request):
		responsePtr = method(state.environment, makeRequest(request))
		response = parseResponse(responsePtr)

		freeResponse(responsePtr)

		return response

	return wrapper

def fromProperty(property, freeResponse):
	""" Adjust the given C function to fit the 'Property' interface. """

	property.argtypes = [ctypes.c_void_p]
	property.restype = ctypes.c_void_p

	def wrapper(state):
		responsePtr = property(state.environment)
		response = parseResponse(responsePtr)

		freeResponse(responsePtr)

		return response

	return wrapper

def fromFunction(func, freeResponse):
	""" Adjust the given C function to fit the 'Function' interface. """

	func.argtypes = [ctypes.c_char_p]
	func.restype = ctypes.c_void_p

	def wrapper(request):
		responsePtr = func(makeRequest(request))
		response = parseResponse(responsePtr)

		freeResponse(responsePtr)

		return response

	return wrapper

class ActivationError(Exception):
	""" An error that occurs during activation of a plugin. """
	pass

def fromActivate(activate, freeEnvironment):
	""" Adjust the given C function to fit the 'Activate' interface. """

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
			return '<Environment @ StablePtr %s>' % self.environment

	return Environment

def createClass(path, activate, methods = [], properties = [], functions = []):
	""" Create a class for the plugin contained within the given path. """

	library         = ctypes.CDLL(path)
	freeResponse    = getFreeResponse(library)
	freeEnvironment = getFreeEnvironment(library)

	Environment = fromActivate(getattr(library, activate), freeEnvironment)

	class Plugin(Environment):
		def __str__(self):
			return '<Plugin %s @ StablePtr %s>' % (path, self.environment)

	for name in methods:
		method = fromMethod(getattr(library, name), freeResponse)
		setattr(Plugin, name, method)

	for name in properties:
		property = fromProperty(getattr(library, name), freeResponse)
		setattr(Plugin, name, property)

	for name in functions:
		function = fromFunction(getattr(library, name), freeResponse)
		setattr(Plugin, name, function)

	return Plugin

Test = createClass(
	path       = './hasklime-example.so',
	activate   = 'testActivate',
	methods    = ['testMethod'],
	properties = ['testProperty', 'testDelete'],
	functions  = ['testFunction']
)

class Test2(Test):
	def __del__(self):
		self.testDelete()
		Test.__del__(self)

for i in range(100):
	x = Test2(1337)

	print(x)
	print(x.testMethod(13))
	print(x.testProperty())
	print(Test.testFunction(37))
