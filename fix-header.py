# This script reads source files given as arguments and rewrites them,
# replacing anything that looks like a copyright header with
# the contents of 'header.inc'. The |year| placeholder is replaced
# with the current year.

import datetime
import os
import shutil
import sys
import tempfile

# The rules for recognizing the headers look like this:
#  - A header is contained between two horizontal rules,
#    that is lines that consist of a sequence of at least
#    10 #'s surrounded by whitespace.
#  - All lines inside the header must be empty or comments.
#  - A header must contain the word 'Copyright'.


def is_horizontal_rule(line):
    """Check if a line looks like a horizontal rule."""
    line = line.strip()
    return all(ch == "#" for ch in line) and len(line) > 10


def is_comment(line):
    """
    Check if a line consists only of whitespace and
    (optionally) a comment.
    """
    line = line.strip()
    return line == "" or line.startswith("#")


def looks_like_a_license(text):
    """
    Check if TEXT is likely to be a license header.

    Note that TEXT does not contain initial /final horizontal rules,
    but does contain comment markers in each non-empty line.
    """
    return "Copyright" in text


def output_new_header(out_file):
    """
    Print new header text to OUT_FILE.
    """
    year = str(datetime.datetime.now().year)
    out_file.write("#" * 80)
    out_file.write("\n")
    with open("header.inc", "rb") as f:
        for line in f:
            out_file.write("# ")
            out_file.write(line.replace("|year|", year))
    out_file.write("#" * 80)
    out_file.write("\n")


def fix_file(in_file, out_file):
    """
    Perform replacement on text from file handle IN_FILE and write
    the result to OUT_FILE.
    """
    buffer = []
    buffering = False
    for line in in_file:
        if buffering:
            if is_horizontal_rule(line):
                text = "".join(buffer[1:])
                hr = buffer[0]
                # We'll either ignore or output the current buffer.
                del buffer[:]
                if looks_like_a_license(text):
                    # Found it, ignore buffered text and current line.
                    output_new_header(out_file)
                    buffering = False
                else:
                    # Not found, but the current line might
                    # be the start of a header, so we'll have to keep buffering.
                    out_file.write(hr)
                    out_file.write(text)
                    buffer.append(line)
            elif is_comment(line):
                buffer.append(line)
            else:
                out_file.write("".join(buffer))
                out_file.write(line)
                del buffer[:]
                buffering = False
        else:
            if is_horizontal_rule(line):
                buffer.append(line)
                buffering = True
            else:
                out_file.write(line)


def main(args):
    for in_name in args[1:]:
        with open(in_name, "rb") as in_file:
            temp_fd, temp_name = tempfile.mkstemp()
            with os.fdopen(temp_fd, "wb") as out_file:
                fix_file(in_file, out_file)
        shutil.move(temp_name, in_name)


if __name__ == "__main__":
    main(sys.argv)
