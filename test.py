import HaskLime

Test = HaskLime.createClass(
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
