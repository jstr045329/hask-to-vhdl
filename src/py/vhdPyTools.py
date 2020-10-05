"""Contains tools useful for VHDL generation"""
from numpy import pi
from secrets import randbits


def tab(n=1):
    return "    "*n


def ziptab(some_list, n=1):
    return [tab(n) + s for s in some_list]


def deg2Rad(deg):
    return deg * pi/180


def rad2Deg(rad):
    return rad * 180 / pi

def wrapInQuotes(s):
    return '"' + s + '"'


def dec2Hex(x, width, errOutOnNegativeZeroPadding):
    s = hex(x)
    s = s[2:]
    numZeros = width - len(s)
    if errOutOnNegativeZeroPadding and (numZeros < 0):
        raise ValueError("Number of zeros should never be negative")
    return "x" + wrapInQuotes("0" * numZeros + s)


def longCommentLine(w=100):
    return "-" * w


def machineGeneratedFileWarning(pyScriptName, comments=[]):
    y = []
    y.append(longCommentLine())
    y.append("-- WARNING: This is a machine generated file. Do not modify.")
    y.append("-- Run " + pyScriptName + " if you need another one.")
    if len(comments) > 0:
        y.append("--")
        y.extend(["-- " + s for s in comments])
        y.append("--")
    y.append(longCommentLine())
    y.append("")
    y.append("")
    return y


def ieeeHeader():
    y = []
    y.append("library IEEE;")
    y.append("use IEEE.std_logic_1164.ALL;")
    y.append("use IEEE.numeric_std.ALL;")
    y.append("")
    y.append("")
    return y


def packageHeader(pkgName):
    return ["package " + pkgName + " is", "", "",]

    
def packageFooter(*args, **kwargs):
    return ["end package;", "", "",]

    
def packageBodyHeader(pkgName):
    return ["package body " + pkgName + " is",]


def packageBodyFooter(*args, **kwargs):
    return ["end package body;", "", "",]


def elseTerm(i):
    return "" if i == 0 else "els"


def comma(i, vectorLength):
    return "," if i < (vectorLength-1) else ""


def vectorClosingParen(i, vectorLength):
    return ")" if i >= (vectorLength-1) else ""


def semicolon(i, vectorLength):
    return ";" if i >= (vectorLength-1) else ""


def spaceInVector(i, vectorLength):
    return " " if i < (vectorLength-1) else ""

def enforceOneLineEnding(s):
    """Ensures that a string contains exactly 1 line ending character at the end."""
    if len(s) == 0:
        return "\n"
    if s[-1] == "\n":
        return s
    return s + "\n"


def guaranteeUnique(existingSet, number_of_numbers, bits):
    new_set = set()
    for i in range(number_of_numbers):
        x = randbits(bits)
        while x in existingSet:
            x = randbits(bits)
        existingSet.add(x)
        new_set.add(x)
    return new_set, existingSet


def makeStimVec(vectorName, typeName, valueList):
    y = []
    y.append("constant %s : %s := (" % (vectorName, typeName,))
    for i in range(len(valueList)):
        oneValue = valueList[i]
        y.append(tab() + str(oneValue) + comma(i, len(valueList)))
    y.append(tab() + ");")
    y.append("")
    return y

