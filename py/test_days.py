import unittest
import subprocess

days = {
    "02": "3716293 6429",
    "05": "13787043 3892695",
    "07": "929800 15432220",
    "09": "2351176124 73110",
    }

class TestIntcodeDays(unittest.TestCase):
    def t_dayout(self, day):
        p = subprocess.run(["python", f"day{day}.py", f"input/day{day}"],
                           capture_output = True, text = True)
        self.assertEqual(p.stdout.rstrip("\n"), days[day])

    def test_all_days(self):
        for day in days.keys():
            with self.subTest(day=day):
                self.t_dayout(day)

if __name__ == '__main__':
    unittest.main()
