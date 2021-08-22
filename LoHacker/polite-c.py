#!/usr/bin python
import os
import sys
import random
import re

def random_bool():
    return bool(random.getrandbits(1))


def get_previous_line(file, pos):
    last_pos = file.rfind('\n', 0,pos)
    first_pos = file.rfind('\n', 0, last_pos-1)
    return file[first_pos+1:last_pos]


def get_next_line(file, pos):
    first_pos = file.find('\n', pos)
    last_pos = file.find('\n', first_pos+1)
    return file[first_pos+1:last_pos]


def get_previous_comments(file, pos):
    comments = []
    previous_line = get_previous_line(file, pos)
    while is_comment(previous_line):
        comments.append(previous_line.strip())
        pos = pos-len(previous_line)
        previous_line = get_previous_line(file, pos)
    return comments


def is_comment(str):
    return str.find('//') != -1


def find_corresponding_bracket(file, pos):
    left_bracket_counter = 0;
    pos += 1
    while pos<len(file):
        if file[pos]== '{':
            left_bracket_counter +=1
        elif file[pos] == '}':
            if left_bracket_counter == 0:
                return pos;
            else: left_bracket_counter -= 1
        pos += 1


def find_thank_you(file, pos):
    open_bracket_pos = file.find('{', pos)
    close_bracket_pos = find_corresponding_bracket(file, open_bracket_pos)
    next_line = get_next_line(file, close_bracket_pos).strip()
    return next_line == '//thank you'


def prevent_do_while(file):
    if 'do ' in file:
        raise()


def check_loops(file):
    loop_pos =[m.start() for m in re.finditer('for|while', file)]
    for pos in loop_pos:
        comments = get_previous_comments(file, pos)
        if (not '//please, loop without doing weird things' in comments) or (not find_thank_you(file, pos)):
            raise()


def check_comparison(file):
    comparison_pos =[m.start() for m in re.finditer('(?<!\#include )\<|(?<!\.h)\>|==|!=', file)]
    for pos in comparison_pos:
        comments = get_previous_comments(file, pos)
        if (not "//please, don't get this comparison wrong" in comments):
            raise()


def check_logic(file):
    logic_pos =[m.start() for m in re.finditer('\|\||\&\&', file)]
    for pos in logic_pos:
        comments = get_previous_comments(file, pos)
        if (not "//please, operate following logic" in comments):
            raise()


def check_bitwise(file):
    bitwise_pos =[m.start() for m in re.finditer('\||\&|\^', file)]
    for pos in bitwise_pos:
        comments = get_previous_comments(file, pos)
        if (not "//please, this stuff is already complicated" in comments):
            raise()


def main():
    file = open(sys.argv[1])
    file = str(file.read())
    try:
        prevent_do_while(file)
    except:
        print('Unallowed keyword found')
        exit()
    try:
        check_loops(file)
        check_comparison(file)
        check_logic(file)
    except:
        command = 'echo "#include <stdio.h> \n int main(){while(1){printf(\\"You are too rude for me\\n\\");}}" | gcc -o output -xc -'
        os.system(command)
        exit()

    temp_out = open('temp_out.c', 'w')
    temp_out.write(file)
    temp_out.close()
    os.system('gcc temp_out.c -o3 -o output')
    os.remove('temp_out.c')
    print('Compilation successful')


if __name__ == '__main__':
    main()
