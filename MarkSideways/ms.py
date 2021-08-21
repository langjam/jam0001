import os
import sys
from marksideways.runner import Runner

def main(ms_dir, args):
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

  Runner(ms_dir, args[0], text).run(args[1:])

if __name__ == "__main__":
  args = sys.argv
  dir = args[0].replace('\\', '/').split('/')
  dir.pop()
  if len(dir) == 0: dir = ['.']

  main(os.sep.join(dir), args[1:])
