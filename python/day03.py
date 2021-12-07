import unittest
import numpy as np

def read_data_03(path):
    text_data = open(path)
    text_data = text_data.readlines()
    return(text_data)

def f03a(data):
    counter = np.zeros((len(data), len(data[1])))
    for i in range(len(data) - 1):
      bits = data[i]
      for j in range(len(bits) - 1):
        focus = int(bits[j])
        counter[i, j] = focus
    return(counter)
  
  
