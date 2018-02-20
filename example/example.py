import threading
from HaskLime import Library, Ref, JSON

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

		for j in range(1000):
			x = increment(x)

		x = dump(x)
		if x != self.i + 1000:
			print(self.i, x)

for i in range(32):
	t = T(i)
	t.start()
	threads.append(t)

for t in threads:
	t.join()