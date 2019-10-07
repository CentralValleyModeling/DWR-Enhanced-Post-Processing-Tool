from java.util.function import Predicate, Consumer, Function, IntPredicate, DoublePredicate, Supplier
from java.util.function import ToDoubleFunction

class jc(Consumer):
    def __init__(self, fn):
        self.accept = fn


class jf(Function):
    def __init__(self, fn):
        self.apply = fn

class js(Supplier):
    def __init__(self, fn):
        self.apply = fn


class jpInt(IntPredicate):
    def __init__(self, fn):
        self.test = fn


class jpDouble(DoublePredicate):
    def __init__(self, fn):
        self.test = fn


class jp(Predicate):
    def __init__(self, fn):
        self.test = fn


class jdf(ToDoubleFunction):
    def __init__(self, fn):
        self.applyAsDouble = fn
