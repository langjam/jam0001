import re, sys

def remove_comments(string):
    string = re.sub(r"<#.*?#>", '', string, re.MULTILINE | re.S)
    string = re.sub(r"%\{.*?%\}", '', string, re.MULTILINE | re.S)
    string = re.sub(r"'''.*?'''", '', string, re.MULTILINE | re.S)
    string = re.sub(r"--\[\[.*?\]\]", '', string, re.MULTILINE | re.S)
    string = re.sub(r"/\*.*?\*/", '', string, re.MULTILINE | re.S)
    string = re.sub(r"<!--.*?-->", '', string, re.MULTILINE | re.S)
    string = re.sub(r"\(\*.*?\*\)", '', string, re.MULTILINE | re.S)
    string = re.sub(r"\{-.*?-\}", '', string, re.MULTILINE | re.S)
    string = re.sub(r"=begin .*?=end", '', string, re.MULTILINE | re.S)
    string = re.sub(r"%.*?%", '', string, re.MULTILINE | re.S)
    string = re.sub(r"#.*", '', string)
    string = re.sub(r"//.*", '', string)
    string = re.sub(r"--.*", '', string)
    string = re.sub(r"⍝.*", '', string)
    string = re.sub(r"REM .*", '', string)
    string = re.sub(r"PLEASE NOTE .*", '', string)

    return string
    
if len(sys.argv) > 1:
    exec(remove_comments(open(sys.argv[1]).read()))
