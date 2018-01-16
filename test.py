from HaskLime import Plugin

class MyPlugin(Plugin):
	def onMessage(self, message):
		print(message)

plugin = MyPlugin('./hasklime-example.so')

for _ in range(1000):
	plugin.sendMessage('Test')

plugin.join()
