#!/usr/bin/env python
# -*- coding: utf-8 -*-

import datetime

from franz.allegrograph.exceptions import IOException, IllegalArgumentException
from franz.transport.agdirectlink import AGDirectLink
from franz.transport.agdirectlinkdebug import AGDirectLinkDebug
from franz.allegrograph.upi import UPI

class AGDTest(object):
    """ generated source for AGDTest

    """
    fail = 0
    ldb = -1
    javad = 0
    verbose = False

    @staticmethod
    def dj(level):
        r = AGDirectLink.debug(level)
        AGDTest.prv("Java debug " + str(r))

    @staticmethod
    def dl(ts, db):
        try:
            r = ts.sendOp1(":debug", 1, 0, db)
            AGDTest.prv("Lisp debug now: " + str(r))
        except (IOException, ), e:
            pass

    @staticmethod
    def pr(m):
        print m

    @staticmethod
    def prv(m):
        if AGDTest.verbose:
            print m

    @staticmethod
    def main(args):
        host = "localhost"
        port = 4567
        nn = 500
        ## TEMPORARY:
        nn = 10
        aa = 1000
        all = True
        p1 = True
        p2 = False
        for i in range(len(args)):
            if args[i] is None:
                pass
            elif args[i].lower() == "-d".lower():
                AGDTest.javad = int(args[i])
                i += 1
            elif args[i].lower() == "-p".lower():
                port = int(args[i])
                i += 1
            elif args[i].lower() == "-l".lower():
                AGDTest.ldb = int(args[i])
                i += 1
            elif args[i].lower() == "-n".lower():
                nn = int(args[i])
                i += 1
            elif args[i].lower() == "-a".lower():
                aa = int(args[i])
                i += 1
            elif args[i].lower() == "-v".lower():
                AGDTest.verbose = True
            elif args[i].lower() == "-p1".lower():
                all = False
                p1 = True
                p2 = False
            elif args[i].lower() == "-p2".lower():
                all = False
                p2 = True
                p1 = False
            elif args[i].lower() == "-all".lower():
                all = True
                p2 = False
                p1 = False
            i += 1
        AGDTest.dj(AGDTest.javad)
        if AGDTest.javad > 0:
            ts = AGDirectLinkDebug(host, port, 3, 1000)
        else:
            ts = AGDirectLink(host, port, 3, 1000)
        if AGDTest.ldb > 0:
            AGDTest.dl(ts, AGDTest.ldb)
        if all:
            r = ts.sendOp0(":verify", 1, -1)
            ra = r
            if (3 != len(ra)):
                raise IOException(":verify length=" + len(ra))
            if not ra[0] == ":verify":
                raise IOException(":verify ra[0]=" + ra[0])
            r = ts.sendOp1(":call", 1, 0, "user::test-0-1")
            if not isinstance(r, long):
                raise IOException(":call test-0-1 return type " + str(r))
            if not (17 == r):
                raise IOException(":call test-0-1 result " + str(r))             
            try:
                r = ts.sendOp1(":call", 1, 0, "user::undefined")
            except (IllegalArgumentException, ), e:
                AGDTest.pr("catch Lisp err: " + str(e))
            try:
                r = ts.sendOp2(":call", 1, 0, "cl:car", 1)
            except (IllegalArgumentException, ), e:
                AGDTest.pr("catch Lisp err: " + str(e))
            r = ts.sendOp1(":call", 1, -1, "cl:values")
            ra = r
            if not (2 == len(ra)):
                raise IOException(":call (values) r.length=" + ra.length)
            ## AGDTest datatypes byte short int long
            for i in range(300):
                AGDTest.test11(ts, i)
                AGDTest.test11(ts,-i)
            AGDTest.test11(ts, 100)
            AGDTest.test11(ts, 1000)
            AGDTest.test11(ts, 10000)
            AGDTest.test11(ts, 100000)
            AGDTest.test11(ts, 1000000)
            AGDTest.test11(ts, 10000000)
            AGDTest.test11(ts, 100000000)
            AGDTest.test11(ts, 1000000000)
            AGDTest.test11(ts, 10000000000l)
            AGDTest.test11(ts, 10000000000000000l)
            AGDTest.test11(ts, -345)
            AGDTest.test11(ts, -345234)
            AGDTest.test11(ts, -345345678)
            AGDTest.test11(ts, -34522222233333l)
            AGDTest.test11(ts, -3457777777777777777l)
            AGDTest.test11(ts, 0x7fffffffffffffffl)
            AGDTest.test11(ts, -0x8000000000000000l)
            AGDTest.test11u(ts, float(123.4))
            AGDTest.test11u(ts, float(123.4))
            upi = UPI()
            AGDTest.test11u(ts, upi)
            AGDTest.test11u(ts, [1, 2, 3])
            AGDTest.test11u(ts, [1, 2, 3])
            AGDTest.test11u(ts, [1, 2, 3])
            AGDTest.test11u(ts, [long(1), long(2), long(3)])
            AGDTest.test11u(ts, [long(1), long(2), long(2), long(2), long(2), long(2), long(3), long(3), long(4), long(5), long(3)])
            AGDTest.test11u(ts, [float(1), float(2), float(3)])
            AGDTest.test11u(ts, [float(1), float(2), float(3)])
            AGDTest.test11u(ts, "string")
            AGDTest.test11u(ts, "much longer string at least thirty two characters long")
            AGDTest.test11u(ts, "much longer string at least thirty two characters long with a run 88888888888888888888888 of many 8's and 999999999999999999999999999999   and then some more")
            AGDTest.test11u(ts, ["aa", "bb", "cc"])
            AGDTest.test11u(ts, ["much longer string at least thirty two characters long with a run 88888888888888888888888 of many 8's and 999999999999999999999999999999   and then some more", "bb", "cc"])
            AGDTest.test11u(ts, [None, "bb", "cc"])
            AGDTest.test11u(ts, ["aa", None, "cc"])
            AGDTest.test11u(ts, ["aa", "bb", None])
            AGDTest.test11u(ts, [None, None, None])
            AGDTest.test11u(ts, [UPI(), UPI(), UPI()])
            AGDTest.test11u(ts, [UPI(), UPI(-1), UPI(-2)])
        i = 0
        n = 0
        if p1:
            AGDTest.dl(ts, 0)
            begin = datetime.datetime.now()
            n = 10 * nn
            ## for-while
            while i < n:
                ts.sendOp0(":verify", 1, -1)
                i += 1
            delta = (datetime.datetime.now() - begin).seconds * 1000
            AGDTest.pr("" + str(n) + " calls to :verify " + str(delta) + " millisec.")
            AGDTest.pr("     " + str(1.0/(delta*0.001*(1.0/n))) + " calls per second.")
        if p2:
            AGDTest.dl(ts, 0)
            a = [0 for i in range(aa)]
            n = nn
            ## for-while
            for i in range(aa):
                a[i] = (1 << 30) - i
            begin = datetime.datetime.now()
            ## for-while
            for i in range(n):
                ts.sendOp2(":call", 1, 0, "user::test-1-1", a)
            delta = (datetime.datetime.now() - begin).seconds * 1000
            AGDTest.pr("" + str(n) + " calls with int[" + str(aa) + "] " + str(delta) + " millisec.")
            AGDTest.pr("     " + str(1.0/(delta*0.001*(1.0/(aa*n)))) + " int round-trips per second.")
        ts.disconnect()

    @staticmethod
    def test11(ts, n):
        r = ts.sendOp2(":call", 1, 0, "user::test-1-1", n)
        if not isinstance(r, long):
            raise IOException(":call test-1-1 return type " + r)
        if not (n == r):
            raise IOException(":call test-1-1(" + n + ") result " + r)

    @staticmethod
    def test11u(ts, n):
        r = ts.sendOp2(":call", 1, 0, "user::test-1-1", n)
        if not (type(n) == type(r)):
            raise IOException(":call test-1-1(" + n + ") result " + r)
        if isinstance(r, str) and n == r:
            return
        if isinstance(r, float) and n == r:
            return
        if isinstance(r, float) and n == r:
            return
        if isinstance(r, UPI) and n == r:
            return
        if isinstance(r, list) and len(n) == len(r):
            equal = True
            for i in range(len(n)):
                if not n[i] == r[i]:
                    equal = False
                    break
            if equal: return
        if isinstance(r, list) and list and isinstance(list[0], UPI):
            if AGDTest.testUPI(n, r):
                return
        raise IOException(":call AGDTest-1-1(" + str(n) + ") result not equal " + str(r))

    @staticmethod
    def testUPI(n, r):
        if (n.length != r.length):
            return False
        ## for-while
        i = 0
        while i < r.length:
            if not n[i] == r[i]:
                return False
            i += 1
        return True

if __name__ == '__main__':
    import sys
    AGDTest.main(sys.argv)

