#!/usr/bin/python

from collections import namedtuple
import os
import time
import filecmp

Test = namedtuple("Test", ['name', 'source', 'expected', 'input'], defaults=[None])
Result = namedtuple("Result", ['name', 'optLevel', 'ok', 'time'])

tests = [ Test("helloworld", "helloworld.bf", "helloworld.out")
          , Test("dbfi", "dbfi.bf", "dbfi.out", "dbfi.bf.in")
          , Test("prime", "prime.bf", "prime.out", "prime.bf.in")
          , Test("mandelbrot", "mandelbrot.bf", "mandelbrot.out")
          , Test("hanoi", "hanoi.bf", "hanoi.out")
]

sourceDir = "bfsrc"
inputDir = "bfsrc"
expectedDir = "integration"
tmpDir = "benchmarkTmp"
compileCommand = "stack run bfc -- "

optLevels = []
# optLevels.append("-O0")
# optLevels.append("-O1")
optLevels.append("-O3")

def runTest(test, optLevel, baseDir):
  """Expected to run in a temporary directory"""
  sourcePath = os.path.join(baseDir, sourceDir, test.source)
  os.system("{} {} {}".format(compileCommand, optLevel, sourcePath))
  execName,_ = os.path.splitext(test.source)
  tmpOutput = execName + ".tmpout"

  execCmd = "./{} ".format(execName)
  if test.input:
    execCmd += " <{}".format(os.path.join(baseDir, inputDir, test.input))

  execCmd += " >{}".format(tmpOutput)
  start = time.perf_counter()
  os.system(execCmd)
  stop = time.perf_counter()
  timeTaken = stop - start

  result = filecmp.cmp(tmpOutput, os.path.join(baseDir, expectedDir, test.expected))

  return Result(test.name, optLevel, result, timeTaken)

if __name__ == '__main__':
  baseDir = os.getcwd()
  if not os.path.exists(tmpDir):
    os.makedirs(tmpDir)
  os.chdir(tmpDir)

  results = []
  for test in tests:
    for optLevel in optLevels:
      print("Testing {} {}".format(test.name, optLevel))
      result = runTest(test, optLevel, baseDir)
      results.append(result)

  os.chdir(baseDir)
  for tmpFile in os.listdir(tmpDir):
    f = os.path.join(tmpDir, tmpFile)
    if os.path.isfile(f):
      os.remove(f)
  os.removedirs(tmpDir)

  print()

  outputFormat = "{:16} : {:>10}  | {:4}"
  print(outputFormat.format("Test", "Time", "Good"))

  for result in results:
    print("{0.name:12} {0.optLevel} : {0.time:>10.3f}s | {0.ok}".format(result))

