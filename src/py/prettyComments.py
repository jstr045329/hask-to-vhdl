#!/usr/bin/env python
"""This script takes command line arguments, puts them in a box, and prints the resulting 
comment block to the console"""
import sys
import pyperclip
LINE_LENGTH = 120
LINE_MARGIN = 10
TITLE_END = "endtitle"
DEFAULT_LANGUAGE = "hs"


comment_markers = {
    "bash": "#",
    "c": "//",
    "cpp": "//",
    "c++": "//",
    "hs": "--",
    "java": "//",
    "py": "#",
    "tcl": "#",
    "ver": "//", # short for verilog
    "vhd": "--",
    }


def centerStr(s):
    numSpaces = int((LINE_LENGTH - len(s)) / 2) - 2
    return (" " * numSpaces) + s


def main():
    if len(sys.argv) < 2:
        return
    if (sys.argv[1] in comment_markers.keys()):
        language = sys.argv[1]
        comment_marker = comment_markers[language]
        argv_start_num = 2
    else:
        comment_marker = comment_markers[DEFAULT_LANGUAGE]
        argv_start_num = 1
        
    num_dashes = LINE_LENGTH - len(comment_marker)
    demarcation = comment_marker + ('-' * num_dashes)
    
    words = sys.argv[argv_start_num:]
    y = []
    y.append(demarcation)
    s = " "
    scrapingTitle = True
    title = ""
    linesWritten = 0
    for oneWord in words:
        if oneWord.lower() == TITLE_END:
            scrapingTitle = False
            y.append(comment_marker + centerStr(title))
            title = ""
        
        elif scrapingTitle:
            title += oneWord + " "
            
        elif len(s) > LINE_LENGTH - LINE_MARGIN:
            if linesWritten == 0:
                y.append(comment_marker)
            y.append(comment_marker + s)
            s = " " + oneWord + " "
            linesWritten += 1
            
            
        else:
            s += oneWord + " "
    
    if len(title) > 0:
        y.append(comment_marker + centerStr(title))
    
    if len(s.split()) > 0:
        # If s is just a space, don't append a new line for that.
        y.append(comment_marker + s)
    
    if linesWritten > 0:
        y.append(comment_marker)
        
    y.append(demarcation)
    
    print("")
    for line in y:
        print(line)
    print("")

    resultStr = ""
    for line in y:
        resultStr += line
        resultStr += "\n"
    # pyperclip.copy('Copy to clipboard')
    print("\nCopied to clipboard\n\n")
    pyperclip.copy(resultStr)


if __name__ == "__main__":
    main()
