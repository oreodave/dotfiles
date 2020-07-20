#! /usr/bin/env python3
from subprocess import check_output

def get_pass():
    return check_output("pass mail", shell=True).strip("\n")
