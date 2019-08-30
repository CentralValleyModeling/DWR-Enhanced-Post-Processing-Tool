from java.util.Arrays import asList
from java.util.function import Predicate, Consumer, Function, IntPredicate
from java.util.stream import Collectors

class jc(Consumer):
    def __init__(self, fn):
        self.accept=fn

class jf(Function):
    def __init__(self, fn):
        self.apply = fn

class jpInt(IntPredicate):
    def __init__(self, fn):
        self.test = fn

class jp(Predicate):
    def __init__(self, fn):
        self.test = fn
