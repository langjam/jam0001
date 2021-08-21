import os
import sys
from marksideways.runner import Runner

def main(args):
  if len(args) == 0:
    print("Usage: python ms.py codefile.md [args...]")
    return

  codepath = args[0]
  if not os.path.exists(codepath):
    print("File not found: " + args[0])
    return
  
  c = open(args[0], 'rt')
  text = c.read()
  c.close()

  Runner(args[0], text).run(args[1:])

if __name__ == "__main__":
  main(sys.argv[1:])
