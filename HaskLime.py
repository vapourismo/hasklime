import ctypes
import json

# Correspondes to the Haskell type `FunPtr (CString -> IO ())`
CSenderFunPtr = ctypes.CFUNCTYPE(None, ctypes.c_char_p)

class Plugin:
	''' HaskLime plugin '''

	def __init__(self, path):
		''' Activate the HasKLime plugin at the given path. '''

		sharedLibrary = ctypes.CDLL(path)

		activatePlugin = sharedLibrary.haskLimeActivate
		activatePlugin.argtypes = [CSenderFunPtr]
		activatePlugin.restype = CSenderFunPtr

		self._ourHandler = CSenderFunPtr(self._onMessage)
		self._theirSender = activatePlugin(self._ourHandler)

	def _onMessage(self, message):
		self.onMessage(json.loads(message))

	def onMessage(self, message):
		''' Overwrite this method to handle message from the plugin. '''

		pass

	def sendMessage(self, message):
		''' Send a given message to the plugin. '''

		self._theirSender(bytes(json.dumps({'tag': 'Message', 'messageContents': message}), 'utf8'))

	def kill(self):
		''' Kill the HaskLime plugin.
		    You can still send messages to the plugin after this methods returns. '''

		self._theirSender(bytes(json.dumps({'tag': 'Kill'}), 'utf8'))

	def join(self):
		''' Wait for the HaskLime plugin to terminate naturally.
		    You can still send messages to the plugin after this method returns. '''

		self._theirSender(bytes(json.dumps({'tag': 'Join'}), 'utf8'))
