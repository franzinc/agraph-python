end_of_term = "\x07"
end_of_record = "\x08"

class CompactFormatCursor:
    def __init__(self, input):
        self.dict = {}
        self.input = input
        self.len = len(input)
        self.pos = 0

    def code(self):
        if self.pos >= self.len - 2: return None
        self.pos += 2
        return self.input[self.pos - 2 : self.pos]

    def string(self):
        end1 = self.input.find(end_of_term, self.pos)
        end2 = self.input.find(end_of_record, self.pos)
        end = min(end1, end2)
        if end == -1: end = max(end1, end2)
        if end == -1: return (None, None)

        string = self.input[self.pos : end]
        self.pos = end + 1
        return (string, end == end2)

    def term(self):
        code = self.code()
        if code is None:
            return (None, None)
        elif code[0] == "!":
            next = self.code()
            val, eor = self.string()
            typ = None
            if code[1] == "\"": typ = "N"
            elif code[1] == "#": typ = "L"
            elif code[1] == "$": typ = "T"
            else: raise Error("Unkown escape code %s" % code)
            self.dict[next] = (typ, val)
            code = self.code()

        string, eor = self.string()
        if code == "\"\"":
            return (string, eor)
        else:
            typ, val = self.dict[code]
            if typ == "N": return ("<%s%s>" % (val, string), eor)
            elif typ == "L": return ("%s@%s" % (string, val), eor)
            elif typ == "T": return ("%s^^<%s>" % (string, val), eor)

    def row(self):
        row = []
        while True:
            term, eor = self.term()
            if term is None: return None
            row.append(term)
            if eor: return row
