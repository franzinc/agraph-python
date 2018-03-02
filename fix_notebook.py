# This script is used to make final adjustments to notebooks generated
# from the tutorial files.  It expects a notebook file to be passed on
# stdin and will print the 'fixed' notebook on stdout.

import json
import re
import sys

# Links to other parts of the documentation will be replaced
# with absolute URLs using this as the root.
DOC_URL = 'https://franz.com/agraph/support/documentation/current/python/'

# Skip cells that start with these
bad_prefixes = [
    '```', # Output cells
    'Common substitutions',  # No idea why the builder even includes this
    'Notebook created',  # Metadata
    '<a',  # Anchors
]

def skip_cell(cell):
    """
    Check if a notebook cell should be removed.
    """
    src = cell.get('source')
    if not src:
        return True
    for prefix in bad_prefixes:
        if src[0].startswith(prefix):
            return True
    return False

def fix_cell(cell):
    """
    Apply modifications to a notebook cell.

    Currently this just fixes links pointing to other documents.
    Note that links to other tutorial examples are not touched, 
    so will point to notebooks rather than to static pages.
    """
    src = cell.get('source')
    for lineno, line in enumerate(src):
        line = re.sub(r'\(\.\./([^#]*)#', '(' + DOC_URL + r'\1.html#', line)
        src[lineno] = line
    return cell

def main():
    document = json.load(sys.stdin)
    document['cells'] = [fix_cell(cell) for cell in document['cells'] if not skip_cell(cell)]
    json.dump(document, sys.stdout)

if __name__ == '__main__':
    main()
