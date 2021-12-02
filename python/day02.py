import unittest

def read_data_02(path):
  text_data = open(path)
  data = text_data.readlines()
  return(data)
  
def f02a(data):
  data = [i.split() for i in data]
  x = 0
  y = 0
  for i in data:
    if i[0] == 'down':
      y += int(i[1])
    elif i[0] == 'up':
      y += int(i[1]) * -1
    else:
      x += int(i[1])
  return(x * y)
 
def f02b(data):
  data = [i.split() for i in data]
  x = 0
  y = 0
  aim = 0
  for i in data:
    if i[0] == 'down':
      aim += int(i[1])
    elif i[0] == 'up':
      aim += int(i[1]) * -1
    else:
      x += int(i[1])
      y += int(i[1]) * aim
  return(x * y)
 
 # Define tests
# Tests to use the sample data and ensure the functions are working
# Run `$ python -m unittest day02.py` in terminal to run test.
# unittest module will recognize test methods as any method that begins with test

class Testf02(unittest.TestCase):
  def test_f02a(self):
    test_data = ["forward 5",
          "down 5",
          "forward 8",
          "up 3",
          "down 8",
          "forward 2"]
    actual = f02a(test_data)
    expected = 150
    self.assertEqual(actual, expected)
    
  def test_f02b(self):
    test_data = ["forward 5",
          "down 5",
          "forward 8",
          "up 3",
          "down 8",
          "forward 2"]
    actual = f02b(test_data)
    expected = 900
    self.assertEqual(actual, expected)
    
# Use the functions
data = read_data_02('./inst/input02.txt')
f02a(data)
f02b(data)
