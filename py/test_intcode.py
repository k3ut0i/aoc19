import unittest
from intcode import Intcode, intcode_run_with_input, Opcode
from array import array

irwi = intcode_run_with_input

class TestIntcodeExamplesDay05(unittest.TestCase):

    def test_simple_add(self): # day02
        ps = '1, 0, 0, 0, 99'
        i = Intcode(ps)
        i.run()
        self.assertEqual(i.memory.mem, array('i', [2, 0, 0, 0, 99]))

    def test_equal_io_pos(self):
        ps = '3,9,8,9,10,9,4,9,99,-1,8'
        self.assertEqual(irwi(ps, 8), 1)
        self.assertEqual(irwi(ps, -1), 0)

    def test_equal_io_imm(self):
        ps = '3,3,1108,-1,8,3,4,3,99'
        self.assertEqual(irwi(ps, 8), 1)
        self.assertEqual(irwi(ps, -33), 0)

    def test_lt_io_pos(self):
        ps = '3,9,7,9,10,9,4,9,99,-1,8'
        self.assertEqual(irwi(ps, 3), 1)
        self.assertEqual(irwi(ps, 9), 0)

    def test_lt_io_imm(self):
        ps = '3,3,1107,-1,8,3,4,3,99'
        self.assertEqual(irwi(ps, -1009), 1)
        self.assertEqual(irwi(ps, 88), 0)

    def test_jump_1(self):
        ps = '3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9'
        i1 = Intcode(ps)
        i2 = Intcode(ps)
        i1.input = 0
        i2.input = -22
        i1.run()
        i2.run()
        self.assertEqual(i1.output, 0)
        self.assertEqual(i2.output, 1)

class TestIntcodeRunAsync(unittest.TestCase):
    def equal8_io(self, inp, out):
        ps = '3,9,8,9,10,9,4,9,99,-1,8' # example prog from day05
        i = Intcode(ps)
        j = i.run_async()
        self.assertEqual(next(j), Opcode.INPUT)
        self.assertEqual(j.send(inp), (Opcode.OUTPUT, out))
        try:
            next(j)
        except StopIteration as si:
            self.assertEqual(si.args, (Opcode.HALT,))
        
    def test_equal8_io(self):
        self.equal8_io(8, 1)
        self.equal8_io(-1000, 0)
            
if __name__ == '__main__':
    unittest.main()
