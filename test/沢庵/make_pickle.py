#! /usr/bin/python
import pickle

class TestClass(object):
    quote_static_unquote_value = "ho ho ho"

    def __init__(self, internal_val_one):
        self.ivo = internal_val_one
        self.string_val = "oh hai there"
        self.float_val = "sup homez"

    def a_method(x):
        return x

if __name__ == '__main__':
    inst = TestClass(42)
    inst.float_val = 3.14

    f = open('takuan.pickle', 'wb')
    pickle.dump(inst, f, 0)
    f.close()
