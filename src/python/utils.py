
# A simple class for storing the average of the last [n] measurements
class MovingAverage:
    def __init__(self, max_samples):
        assert(max_samples > 0)
        self.max_samples = max_samples
        self.samples = []
        self.i = 0

    def add_sample(self, x):
        if len(self.samples) < self.max_samples:
            self.samples.append(x)
        else:
            self.samples[self.i] = x
            self.i = (self.i + 1) % self.max_samples

    def get_average(self):
        assert(len(self.samples) > 0)
        return sum(self.samples, 0) / float(len(self.samples))

