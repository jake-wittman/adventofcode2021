import unittest
import numpy as np
# See the corresponding *.R code in R/ for the actual challenge information
# or the advent of code website

def read_data_01(path):
  text_data = open(path)
  text_data = text_data.readlines()
  text_data = np.asarray(text_data)
  data = [int(i) for i in text_data]
  return(data)

def f01a(data):
  change = 0 
  for i in range(1, len(data)):
    if data[i] > data[i - 1]:
      change += 1
  return(change)

def f01b(data):
  # Use convoled to get the sliding 
  sum_data = np.convolve(data, np.ones(3, dtype=int), 'valid')
  change = 0 # counter if new value is > previous
  for i in range(1, len(sum_data)):
    if sum_data[i] > sum_data[i - 1]:
      change += 1
  return(change)

# Tests to use the sample data and ensure the functions are working
# Run `$ python -m unittest day01.py` in terminal to run test.
# unittest module will recognize test methods as any method that begins with test
class Testf01a(unittest.TestCase):
  def test_f01a(self):
    test_data = np.array([199, 200, 208, 210, 200, 207, 240, 269, 260, 263])
    actual = f01a(test_data)
    expected = 7
    self.assertEqual(actual, expected)
    
class Testf01b(unittest.TestCase):
  def test_f01b(self):
    test_data = np.array([607, 618, 618, 617, 647, 716, 769, 792])
    actual = f01a(test_data)
    expected = 5
    self.assertEqual(actual, expected)

# Use the functions
data = read_data_01('./inst/input01.txt')
f01a(data)
f01b(data)
