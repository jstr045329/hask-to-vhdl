#!/usr/bin/env python
"""This script takes command line arguments, puts them in a box, and prints the resulting 
comment block to the console"""
import sys
COMMENT_MARKER = "--"
LINE_LENGTH = 120
LINE_MARGIN = 10
DEMARCATION = '-' * LINE_LENGTH
ARGV_START_NUM = 1
TITLE_END = "endtitle"


def centerStr(s):
    numSpaces = int((LINE_LENGTH - len(s)) / 2) - 2
    return (" " * numSpaces) + s


def main():
    words = sys.argv[ARGV_START_NUM:]
    y = []
    y.append(DEMARCATION)
    s = " "
    scrapingTitle = True
    title = ""
    for oneWord in words:
        if oneWord.lower()   == TITLE_END:
            scrapingTitle = False
            y.append(COMMENT_MARKER + centerStr(title))
            y.append(COMMENT_MARKER)
            title = ""
        
        elif scrapingTitle:
            title += oneWord + " "
            
        elif len(s) > LINE_LENGTH - LINE_MARGIN:
            y.append(COMMENT_MARKER + s)
            s = " " + oneWord + " "
        else:
            s += oneWord + " "
    
    if len(title) > 0:
        y.append(COMMENT_MARKER + centerStr(title))
    
    if len(s) > 0:
        y.append(COMMENT_MARKER + s)
    
    y.append(DEMARCATION)
    
    for line in y:
        print(line)


if __name__ == "__main__":
    main()
