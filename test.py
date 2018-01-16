import ctypes
import traceback
import io
import time
import threading
import json

CSenderFunPtr = ctypes.CFUNCTYPE(None, ctypes.c_char_p)

class Plugin():
	def __init__(self, path):
		sharedLibrary = ctypes.CDLL(path)

		activatePlugin = sharedLibrary.haskLimeActivate
		activatePlugin.argtypes = [CSenderFunPtr]
		activatePlugin.restype = CSenderFunPtr

		self._ourHandler = CSenderFunPtr(self._onMessage)
		self._theirSender = activatePlugin(self._ourHandler)

	def onMessage(self, message):
		pass

	def _onMessage(self, message):
		self.onMessage(json.loads(message))

	def sendMessage(self, message):
		self._theirSender(bytes(json.dumps({'tag': 'Message', 'messageContents': message}), 'utf8'))

	def kill(self):
		self._theirSender(bytes(json.dumps({'tag': 'Kill'}), 'utf8'))

	def join(self):
		self._theirSender(bytes(json.dumps({'tag': 'Join'}), 'utf8'))

class MyPlugin(Plugin):
	def onMessage(self, message):
		print(message)

plugin = MyPlugin('./hasklime-example.so')

for _ in range(1000):
	plugin.sendMessage('Test')

plugin.join()
