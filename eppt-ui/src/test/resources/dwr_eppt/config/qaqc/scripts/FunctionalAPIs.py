#  Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
#
#   EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
#   under the GNU General Public License, version 2. This means it can be
#   copied, distributed, and modified freely, but you may not restrict others
#   in their ability to copy, distribute, and modify it. See the license below
#   for more details.
#
#   GNU General Public License

from java.util.function import Predicate, Consumer, Function, IntPredicate, DoublePredicate, \
    BiFunction, ToDoubleFunction, Supplier, BinaryOperator


class jc(Consumer):
    def __init__(self, fn):
        self.accept = fn


class jf(Function):
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

class jbf(BiFunction):
    def __init__(self, fn):
        self.apply = fn

class jbo(BinaryOperator):
    def __init__(self, fn):
        self.apply = fn

class js(Supplier):
    def __init__(self, fn):
        self.get = fn
