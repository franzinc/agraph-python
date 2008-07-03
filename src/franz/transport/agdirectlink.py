#!/usr/bin/env python
# -*- coding: utf-8 -*-

##***** BEGIN LICENSE BLOCK *****
##Version: MPL 1.1
##
##The contents of this file are subject to the Mozilla Public License Version
##1.1 (the "License"); you may not use this file except in compliance with
##the License. You may obtain a copy of the License at
##http:##www.mozilla.org/MPL/
##
##Software distributed under the License is distributed on an "AS IS" basis,
##WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
##for the specific language governing rights and limitations under the
##License.
##
##The Original Code is the AllegroGraph Java Client interface.
##
##The Original Code was written by Franz Inc.
##Copyright (C) 2006 Franz Inc.  All Rights Reserved.
##
##***** END LICENSE BLOCK *****

from  __future__ import with_statement
import threading, socket
from time import sleep
from franz.exceptions import IllegalArgumentException, IOException, InterruptedException, UnimplementedMethodException
from franz.transport.agc import *
from franz.allegrograph.upi import UPI
from franz import util
from franz.util import Byte, Short, Character, Float


# * This class implements a custom socket transport layer for the Lisp/Java
# * interface to the AllegroGraph triple-store implementation.
# * 
# * Lisp advertises at host:port Java connects
# * Inspired by Jlinker Transport.java as of 2006-05-17
class Op(object):
    """ generated source for Op

    """
    def __init__(self, op, opix, vals):
        self.op = op
        self.opix = opix
        self.vals = vals


    def getOp(self):
        return self.op

    def getOpix(self):
        return self.opix

    def getVals(self):
        return self.vals

EXPERIMENTAL = True

class AGDirectLink(object):
    """ generated source for AGDirectLink

    """
    debugClient = 0
    agServerLevel = 0
    state = PORT_CLOSED
    ERR_PORT_CLOSED = -101
    ERR_PORT_STATE = -102
    ERR_PROTOCOL = -104
    ERR_PORT_IO = -107
    ERR_FLUSH_IO = -108
    ERR_THROW = -109
    ERR_BUSY = -111

    ## SET THIS TO False TO TEST 'pack' VERSION (WHICH SHOULD BE FASTER):
    ONE_BYTE_AT_A_TIME = True
        
    def __init__(self, host, port, pollCount, pollInterval, timeout=5000):
        self.byte_at_a_time_buffer = [0 for i in range(DEFAULT_BUFFER_SIZE)]
        self.endpos = 0
        self.strings_buffer = []
        self.timeout = timeout;
        client = None
        ee = None
        i = 0
        while client is None and i < pollCount:
            try:
                if i > 0:
                    sleep(pollInterval)
                sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                sock.settimeout(self.timeout)
                ## TEMPORARY:
                #host = socket.gethostbyname(socket.gethostname())
                print "HOST ", host, " PORT ", port
                ## END HOST
                addr = (host, port)
                sock.connect(addr)
                client = sock
            except Exception, e:
                lastE = e
            i += 1
        if client is None:
            if lastE is None:
                raise IOException("Failed to connect to server.")
            else:
                raise lastE
        ## NOT SEEING PYTHON EQUIVALENT:
        ##client.setTcpNoDelay(True)
        self.socket = client
        self.socket_cursor = 0
        self.chunk_size = 0
        self.socket_chunk = ""
        #self.inStream = client.getInputStream()
        #self.outStream = client.getOutputStream()
        self.state = PORT_IDLE
        self.softLock = threading.Event()
        self.softLock.set()  ## reverse default initial value
        ## locks for synchronized methods:
        self.disconnectLock = threading.RLock()
        self.grabSoftLockLock = threading.RLock()
        self.dropSoftLockLock = threading.RLock()
        self.opIndexLock = threading.RLock()
        flag = self.timeout / 10
        ## AS FAR AS I CAN TELL, PYTHON HAS NO 'available' METHOD:
#        while flag > 0:
#            try:
#                sleep(100)
#                flag -= 1
#                if 0 < self.inStream.available():
#                    flag = -1
#            except (InterruptedException, ), e:
#                pass
        if (flag == 0):
            self.socket.close()
            raise IOException("Connected but timed out.")
        #EXPERIMENT
        #reply = self.socket.recv(1)
        reply = self.receive_next_byte()
        if reply == TAG_ENDER:
            self.socket.close()
            raise IOException("Too many connections.")
        elif reply == TAG_NULL:
            self.socket.close()
            raise IOException("Connection rejected.")
        elif (reply == TAG_START + AG_DIRECT_LEVEL):
            self.socket.close()
            raise IOException("Unexpected initial reply " + reply)        
        ##TEMPORARY
        self.PIS_COUNT = 0
        self.PIL_COUNT = 0        

    connectFlag = False

    def throwIOErr(self, where, e):
        err = "Unknown"
        if e == -1:
            err = "End_of_file"
        elif e == AGDirectLink.ERR_PORT_STATE:
            err = "ERR_PORT_STATE"
        elif e == AGDirectLink.ERR_PROTOCOL:
            err = "ERR_PROTOCOL"
        elif e == AGDirectLink.ERR_PORT_IO:
            err = "ERR_PORT_IO"
        elif e == AGDirectLink.ERR_FLUSH_IO:
            err = "ERR_FLUSH_IO"
        elif e == AGDirectLink.ERR_THROW:
            err = "ERR_THROW"
        elif e == AGDirectLink.ERR_BUSY:
            err = "ERR_BUSY"
        elif e == AGDirectLink.ERR_PORT_CLOSED:
            err = "ERR_PORT_CLOSED"
        else:
            err = "Unknown err(" + e + ")"
        raise IOException(err + " in " + where)

    @staticmethod
    def debug(cl):
        oldc = AGDirectLink.debugClient
        if not cl < 0:
            AGDirectLink.debugClient = cl
        return "Client was ", oldc, " now ", AGDirectLink.debugClient

    def query(self):
        r = self.sendOp1(OP_VERIFY, 1, 0, AG_DIRECT_LEVEL)
        if isinstance(r, str):
            s = r
            if s.startsWith("AGDirect Version"):
                pl = "server level"
                plx = s.indexOf(pl)
                if plx < 0:
                    return True
                try:
                    self.agServerLevel = int(s.substring(plx + len(pl) + 1))
                except (Exception, ), e:
                    pass
                return True
        self.disconnect()
        return False

    #@synchronized(mlock)
    def disconnect(self):
        with self.disconnectLock:
            if self.state == PORT_CLOSED:
                return False
            elif self.state == PORT_IDLE:
                self.sendOp0(OP_DISCONNECT, -1, -1)
                try:
                    sleep(100)
                except (Exception, ), e:
                    pass
            else:
                try:
                    self.socket.close()
                    self.state = PORT_CLOSED
                    self.socket = None
                    #self.inStream = None
                    #self.outStream = None
                    return True
                except Exception:
                    return False

    #@synchronized
    def grabSoftLock(self, caller):
        with self.grabSoftLockLock:
            cur = threading.currentThread()
            if self.softLock.isSet():
                self.softLock.clear()
                self.softLock.thread = cur
            else:
                if (self.softLock.thread == cur):
                    return "Recursive call to" + caller
                else:
                    while not self.softLock.isSet():
                        try:
                            self.softLock.wait()
                        except Exception:
                            pass
                    self.softLock.clear()
                    self.softLock.thread = cur
            return ""
    
    #@synchronized
    def dropSoftLock(self):
        with self.dropSoftLockLock:
            if self.softLock.isSet(): ## is free
                return
            self.softLock.set()  ## unblock waiting threads

    def sendOp0(self, op, style, rx):
        """
        * Send an operation with zero parts to the server.
        * 
        * @param op A string that names the operation.
        * @param style 1 normal, 0 ignore results, -1 one-way
        * @return An Object instance that contains the result of the call. The
        *         class of the object depends on the call.
        """
        try:
            opix = self.sendOpHeader(op, style, 0)
            return self.sendOpTail(op, opix, style, rx)
        finally:
            self.state = PORT_IDLE
            self.dropSoftLock()

    def sendOp1(self, op, style, rx, arg):
        """
        Send an operation with one part to the server.
        """
        try:
            opix = self.sendOpHeader(op, style, 1)
            self.portOut_long(arg)
            return self.sendOpTail(op, opix, style, rx)
        finally:
            self.state = PORT_IDLE
            self.dropSoftLock()

    def sendOp2(self, op, style, rx, arg0, arg1):
        """
        Send an operation with two parts to the server.
        """
        try:
            opix = self.sendOpHeader(op, style, 2)
            self.portOut(arg0)
            self.portOut_long(arg1)
            return self.sendOpTail(op, opix, style, rx)
        finally:
            self.state = PORT_IDLE
            self.dropSoftLock()

    def sendOp1n(self, op, style, rx, arg0, args):
        """
        Send an operation with 1+n parts to the server
        """
        try:
            opix = self.sendOpHeader(op, style, 1 + len(args))
            self.portOut(arg0)
            ## for-while
            i = 0
            while i < len(args):
                self.portOutUnwrapped(args[i])
                i += 1
            return self.sendOpTail(op, opix, style, rx)
        finally:
            self.state = PORT_IDLE
            self.dropSoftLock()

    def sendOp2n(self, op, style, rx, arg0, arg1, args):
        """
        Send an operation with 2+n parts to the server
        """
        try:
            opix = self.sendOpHeader(op, style, 2 + len(args))
            self.portOut(arg0)
            self.portOut_long(arg1)
            ## for-while
            i = 0
            while i < len(args):
                self.portOutUnwrapped(args[i])
                i += 1
            return self.sendOpTail(op, opix, style, rx)
        finally:
            self.state = PORT_IDLE
            self.dropSoftLock()

    def sendOp3n(self, op, style, rx, arg0, arg1, arg2, args):
        try:
            opix = self.sendOpHeader(op, style, 3 + len(args))
            self.portOut(arg0)
            self.portOut_long(arg1)
            self.portOut(arg2)
            ## for-while
            i = 0
            while i < len(args):
                self.portOutUnwrapped(args[i])
                i += 1
            return self.sendOpTail(op, opix, style, rx)
        finally:
            self.state = PORT_IDLE
            self.dropSoftLock()

    opIndex = 0

    #@synchronized(mlock)
    def getOpIndex(self):
        with self.opIndexLock:
            self.opIndex += 1
            return self.opIndex

    def sendOpHeader(self, op, style, argCount):
        ret = 0
        opix = self.getOpIndex()
        gv = self.grabSoftLock("sendOpHeader()")
        if len(gv) > 0:
            raise IOException("sendOpHeader " + op + "cannot grab lock: " + gv)
        if self.state == PORT_IDLE:
            self.state = PORT_MESSAGE
            ret = self.portOutTag(TAG_OP)
            if not ret < 0:
                ret = self.portOut(op)
            if ret < 0:
                return ret
            if style < 0:
                ret = self.portOut(0)
            else:
                if (style == 0):
                    ret = self.portOut(-opix)
                else:
                    ret = self.portOut(opix)
            if not ret < 0:
                ret = self.portOut(argCount)
        elif self.state == PORT_CLOSED:
            ret = self.ERR_PORT_CLOSED
        else:
            ret = self.ERR_PORT_STATE
        if ret < 0:
            self.throwIOErr("sendOpHeader ", ret)
        return opix

    def sendOpTail(self, op, opix, style, rx):
        ret = 0
        ret = self.streamOutFlush()
        if ret < 0:
            self.throwIOErr("sendOp", ret)
        if style < 0:
            return None
        self.state = PORT_WAITING_REPLY
        return self.opResIn(op, opix, rx)

    def opResIn(self, op, opix, rx):
        #print "OPRESIN ",
        res = self.portInOp()
        if 2 > len(res):
            raise IOException("opResIn " + op + "[" + opix + "]" + " received " + len(res))
        if not op == res[0]:
            raise IOException("opResIn " + op + "[" + opix + "]" + " received " + res[0])
        rrx = res[1]
        if rrx < 0 and (-rrx == opix) and 3 < len(res):
            raise IOException("opResIn " + op + "[" + opix + "]" + " error in AllegroGraph server " + res[2] + " " + res[3])
        if rrx < 0:
            raise IOException("opResIn " + op + "[" + opix + "]" + " error in AllegroGraph server " + rrx + " " + len(res))
        if not (rrx == opix):
            raise IOException("opResIn " + op + "[" + opix + "]" + " expected " + opix + " received " + rrx)
        if (rx == -1):
            return res
        if (rx == -2):
            if (2 == len(res)):
                return None
            raise IOException("opResIn " + op + "[" + opix + "]" + " expected zero values, received " + len(res))
        if 0 <= rx and rx < len(res) - 2:
            return res[rx + 2]
        raise IOException("opResIn mismatch:" + op + "[" + opix + "]" + " expected " + rx + 1 + " results, received " + len(res) - 2)

    def streamOutFlush(self):
        return self.portFlush()

    haveCode = 1000000

    def tagToString(self, tag):
        if tag < TAG_START:
            return "NOT_A_TAG"
        if tag < TAG_INT_END:
            return "INTEGER"
        if tag == TAG_NULL:
            return "NULL"
        elif tag == TAG_BYTE:
            return "BYTE"
        elif tag == TAG_SHORT:
            return "SHORT"
        elif tag == TAG_INT:
            return "INT"
        elif tag == TAG_CHAR:
            return "CHAR"
        elif tag == TAG_FLOAT:
            return "FLOAT"
        elif tag == TAG_DOUBLE:
            return "DOUBLE"
        elif tag == TAG_SEQ:
            return "SEQ"
        elif tag == TAG_SPARSE:
            return "SPARSE"
        elif tag == TAG_OBJECT:
            return "OBJECT"
        elif tag == TAG_TRUE:
            return "BOOLEANtrue"
        elif tag == TAG_FALSE:
            return "BOOLEANfalse"
        elif tag == TAG_OPENd:
            return "OPENd"
        elif tag == TAG_OP:
            return "OP"
        elif tag == TAG_END:
            return "TAG_END"
        else:
            return "TAG_STRING"

    def isIntTag(self, tag):
        if tag < TAG_START:
            return False
        if tag < TAG_INT_END:
            return True
        return False

    def portInDupRep(self, next, i, w, source):
        if (next == TAG_DUP):
            j = self.portInLong()
            w[i] = w[i-j]
            #Array.set(w, i, Array[w, i - j])
            return i + 1
        if (next == TAG_REP):
            j = self.portInLong()
            n = self.portInLong()
            v = w[i - j]
            ## for-while
            for k in range(n):
                w[i] = v
                i += 1
            return i
        else:
            raise IOException(source + " subtag " + next)

    def portInSeqLong(self, lgth):
        wl = [long(0) for i in range(lgth)]
        i = 0
        while i < lgth:
            next = self.portIn_8()
            if self.isIntTag(next):
                wl[i] = self.portInLong(next)
                i += 1
            else:
                i = self.portInDupRep(next, i, wl, "portInSeqLong")
        return wl

    def portInSeqByte(self, lgth):
        wl = [0 for i in range(lgth)]
        ## for-while
        i = 0
        while i < lgth:
            next = self.portIn_8()
            if self.isIntTag(next):
                wl[i] = self.portInLong(next)
                i += 1
            else:
                i = self.portInDupRep(next, i, wl, "portInSeqByte")
        return wl

    def portInSeqShort(self, lgth):
        wl = [0 for i in range(lgth)]
        ## for-while
        i = 0
        while i < lgth:
            next = self.portIn_8()
            if self.isIntTag(next):
                wl[i] = self.portInLong(next)
                i += 1
            else:
                i = self.portInDupRep(next, i, wl, "portInSeqShort")
        return wl

    def portInSeqInt(self, lgth):
        wl = [0 for i in range(lgth)]
        ## for-while
        i = 0
        while i < lgth:
            next = self.portIn_8()
            if self.isIntTag(next):
                wl[i] = self.portInLong(next)
                i += 1
            else:
                i = self.portInDupRep(next, i, wl, "portInSeqInt")
        return wl

    def portInSeqFloat(self, lgth):
        wl = [float(0) for i in range(lgth)]
        ## for-while
        i = 0
        while i < lgth:
            next = self.portIn_8()
            if (next == TAG_FLOAT):
                wl[i] = self.at(next)
                i += 1
            else:
                i = self.portInDupRep(next, i, wl, "portInSeqFloat")
        return wl

    def portInSeqDouble(self, lgth):
        wl = [float(0.0) for i in range(lgth)]
        ## for-while
        i = 0
        while i < lgth:
            next = self.portIn_8()
            if (next == TAG_DOUBLE):
                wl[i] = self.portInDouble(next)
                i += 1
            else:
                i = self.portInDupRep(next, i, wl, "portInSeqDouble")
        return wl

    def portInSeqString(self, lgth):
        print "PISS ", lgth,
        #EXPERIMENT
        #wl = [None for i in range(lgth)]
        wl = [None] * lgth
        i = 0
        while i < lgth:
            next = self.portIn_8()
            if (next == TAG_DUP):
                j = int(self.portInLong())
                wl[i] = wl[i - j]
                i += 1
            else:
                if (next == TAG_REP):
                    j = int(self.portInLong())
                    n = int(self.portInLong())
                    v = wl[i - j]
                    ## for-while
                    k = 0
                    while k < n:
                        wl[i] = v
                        i += 1
                        k += 1
                else:
                    s = self.portInString(next)
                    if s is None:
                        wl[i] = None
                    else:
                        wl[i] = str(s)
                    i += 1
        print "EXIT PISS ",
        return wl

    def portInSeqObject(self, lgth):
        wl = [None for i in range(lgth)]
        i = 0
        while i < lgth:
            next = self.portIn_8()
            if (next == TAG_DUP):
                j = self.portInLong()
                wl[i] = wl[i - j]
                i += 1
            elif (next == TAG_REP):
                    j = self.portInLong()
                    n = self.portInLong()
                    v = wl[i - j]
                    for k in range(n):
                        wl[i] = v
                        i += 1
            else:
                wl[i] = self.streamInValue(next)
                i += 1
        return wl

    def portInSeqUPI(self, lgth):
        print "PISU ", lgth,
        wl = [None for i in range(lgth)]
        i = 0
        while i < lgth:
            next = self.portIn_8()
            if (next == TAG_DUP):
                j = int(self.portInLong())
                wl[i] = wl[i - j]
                i += 1
            elif (next == TAG_REP):
                j = int(self.portInLong())
                n = int(self.portInLong())
                v = wl[i - j]
                for k in range(n):
                    wl[i] = v
                    i += 1
            elif (next == TAG_UPI):
                wl[i] = self.portInUPI(next)
                i += 1
            else:
                wl[i] = UPI(self.portInLong(next))
                i += 1
        return wl

    def portInSeqBody(self, tag, lgth, sub):
        if (not tag == TAG_SEQ):
            raise IOException("portInSequence tag " + tag)
        i = 0
        if sub < TAG_START:
            raise IOException("portInSequence subtag " + sub)
        if sub < TAG_INT_END:
            return self.portInSeqLong(lgth)
        else:
            if sub < TAG_LSTR:
                if sub == TAG_BYTE:
                    return self.portInSeqByte(lgth)
                elif sub == TAG_UPI:
                    return self.portInSeqUPI(lgth)
                elif sub == TAG_SHORT:
                    return self.portInSeqShort(lgth)
                elif sub == TAG_INT:
                    return self.portInSeqInt(lgth)
                elif sub == TAG_LONG:
                    return self.portInSeqLong(lgth)
                elif sub == TAG_CHAR:
                    wc = [None for i in range(lgth)]
                    while i < lgth:
                        wc[i] = self.portInLong()
                        i += 1
                    return wc
                elif sub == TAG_FLOAT:
                    raise Exception("AAA WE SHOULDN'T BE SEEING FLOAT TAGS")
                    return self.portInSeqFloat(lgth)
                elif sub == TAG_DOUBLE:
                    return self.portInSeqDouble(lgth)
                elif sub == TAG_FALSE:
                    wbl = [False for j in range(lgth)]
                    while i < lgth:
                        b = self.portIn_8()
                        if b == TAG_TRUE:
                            wbl[i] = True
                            break
                        elif b == TAG_FALSE:
                            wbl[i] = False
                            break
                        else:
                            raise IOException("portInSequence boolean " + b)
                        i += 1
                    return wbl
                else:
                    return self.portInSeqObject(lgth)
            else:
                return self.portInSeqString(lgth)

    def portInSparse(self, tag):
        raise IOException("portInSparse not implemented " + tag)

    def portInObject(self, tag):
        raise IOException("portInObject not implemented" + tag)

    @classmethod
    def stringValue(cls, x):
        if x is None:
            return "Null"
        return str(x)

    def portInOpOb(self, tag):
        if (tag != TAG_OP):
            raise IOException("portInOp tag " + tag)
        op = self.portInString()
        opix = self.portInLong()
        lgth = self.portInLong()
        rr = [None for i in range(lgth)]
        ww = [op, long(opix), rr]
        ## for-while
        i = 0
        while i < lgth:
            rr[i] = self.streamInValue()
            i += 1
        if opix < 0:
            s1 = "Unknown"
            s2 = "Unknown"
            if lgth > 0:
                s1 = self.stringValue(rr[0])
            if lgth > 1:
                s2 = self.stringValue(rr[1])
            raise IllegalArgumentException("Operation " + op + "[" + -opix + "] signaled an error in server: " + s1 + " -- " + s2)
        return ww

    def portIn_8(self):
        res = 0
        try:
            # EXPERIMENT
            #chr = self.socket.recv(1)
            chr = self.receive_next_byte()
            # EXPERIMENT
            #res = util.character_to_integer(chr)
            res = ord(chr)
        except IOException, e:
            res = self.ERR_PORT_IO
        if res < 0:
            self.throwIOErr("portIn", res)
        return res

    def portOutInteger(self, v):
        """
        Write the bits in integral number to the socket, omitting 
        all-zeros bytes in the top-most bytes.
        """
        ## if 'x' is a character (lgth-1 string), need to convert
        ## it to an integer:
        if isinstance(v, str):
            v = util.character_to_integer(v)
        tag = 0
        top = 0
        lgth = 0
        sign = 0
        if v < 0:
            v = -(1 + v)
            sign = TAG_SIGN_MASK
        if v < TAG_IMM_TOP:
            tag = -1
            top = 0
            lgth = 1
        elif v < 0x100 + TAG_IMM_TOP:
            tag = 0
            top = 8
            lgth = 2
            v = v - TAG_IMM_TOP
        elif v < 0x10000:
            tag = 1
            top = 16
            lgth = 3
        elif v < 0x1000000:
            tag = 2
            top = 24
            lgth = 4
        elif v < 0x100000000l:
            tag = 3
            top = 32
            lgth = 5
        elif v < 0x10000000000l:
            tag = 4
            top = 40
            lgth = 6
        elif v < 0x1000000000000l:
            tag = 5
            top = 48
            lgth = 7
        elif v < 0x100000000000000l:
                tag = 6
                top = 56
                lgth = 8
        else:
            tag = 7
            top = 64
            lgth = 9
        rc = self.portReserveSpace(lgth)
        if rc < 0:
            return rc
        if tag < 0:
            tag = v
        else:
            tag = TAG_IMM_TOP + tag
        tag = tag | sign | TAG_START
        self.bufferOut_8(tag)
        self.output_bytes_in_long(v, top)
        return self.endpos
    
    def output_bytes_in_long(self, longValue, limit):
        """
        Write out a byte-at-a-time from 'longValue' to the output byte_at_a_time_buffer.
        Alternatively, consider packing the entire used portion of 'longValue'
        into a byte-string, and placing that in the byte_at_a_time_buffer.
        """
        if AGDirectLink.ONE_BYTE_AT_A_TIME:
            for shift in range(0, limit, 8):
                self.bufferOut_8(int(0xff & (longValue >> shift)))
        else:
            charCount = (limit // 8) + 1
            s = util.integer_to_string(longValue)[:charCount]
            self.strings_buffer.append(s)

    def portFlush(self):
        if self.endpos > 0:
            try:
                if AGDirectLink.ONE_BYTE_AT_A_TIME:
                    ## TODO: CONSIDER JOINING THE STRINGS AND DOING SINGLE SEND HERE:
                    for i in range(self.endpos):
                        self.socket.send(self.byte_at_a_time_buffer[i])
                else:
                    ## TODO: CONSIDER JOINING THE STRINGS AND DOING SINGLE SEND HERE:
                    for s in self.strings_buffer:
                        self.socket.send(s)
                    self.strings_buffer = []
            except (Exception, ), e:
                return self.ERR_FLUSH_IO
        self.endpos = 0
        return 0

    def portReserveSpace(self, size):
        rc = 0
        if self.endpos + size > len(self.byte_at_a_time_buffer):
            rc = self.portFlush()
        if rc < 0:
            return rc
        return self.endpos + size

    def portOutTag(self, tag):
        rc = self.portReserveSpace(1)
        if rc < 0:
            return rc
        rc = self.bufferOut_8(tag)
        return rc

    
    def portOutTagAndValue(self, tag, v):
        """
        Write out the tag 'tag' and the integer 'v', converting 'v' to
        a sequence of bytes.  
        """
        rc = 0
        if 0 < tag:
            rc = self.portOutTag(tag)
        if not rc < 0:
            rc = self.portOutInteger(v)
        return rc
    
    def portOut(self, x):
        return self.portOutUnwrapped(x)

    def portOut_boolean(self, x):
        if x:
            return self.portOutTag(TAG_TRUE)
        else:
            return self.portOutTag(TAG_FALSE)

    def portOut_byte(self, x):
        return self.portOutTagAndValue(TAG_BYTE, x)

    def portOut_character(self, x):
        return self.portOutTagAndValue(TAG_CHAR, x)

    def portOut_short(self, x):
        return self.portOutTagAndValue(TAG_SHORT, x)

    def portOut_int(self, x):
        return self.portOutTagAndValue(TAG_INT, x)

    def portOut_long(self, x):
        return self.portOutTagAndValue(0, x)

    def portOutSeqHead(self, source, lgth, tag):
        if (source == ""):
            source = ""
        rc = self.portReserveSpace(1)
        if rc < 0:
            return rc
        self.bufferOut_8(TAG_SEQ)
        rc = self.portOutInteger(lgth)
        if rc < 0:
            return rc
        rc = self.portReserveSpace(1)
        if rc < 0:
            return rc
        self.bufferOut_8(tag)
        return self.endpos

    def portOut_byte_array(self, x):
        rc = self.portReserveSpace(1)
        if rc < 0:
            return rc
        self.bufferOut_8(TAG_BYTES)
        rc = self.portOutInteger(len(x))
        if rc < 0:
            return rc
        ## for-while
        i = 0
        while i < len(x):
            rc = self.portReserveSpace(1)
            if rc < 0:
                return rc
            self.bufferOut_8(x[i])
            i += 1
        return self.endpos

    def portOut_upi(self, x):
        if (None == x.getUPI()):
            return self.portOut_long(x.getCode())
        rc = self.portReserveSpace(1)
        if rc < 0:
            return rc
        self.bufferOut_8(TAG_UPI)
        b = 0
        i = 0
        while b > -1:
            b = x.getByte(i)
            i += 1
            if b > -1:
                rc = self.portReserveSpace(1)
                if rc < 0:
                    return rc
                self.bufferOut_8(b)
        return self.endpos

    def portOut_upi_array(self, x):
        self.portOutSeqHead("UPI", len(x), TAG_UPI)
        ## for-while
        i = 0
        while i < len(x):
            self.portOut(x[i])
            i += 1
        return self.endpos

    def portOut_short_array(self, x):
        self.portOutSeqHead("short", len(x), TAG_SHORT)
        ## for-while
        i = 0
        while i < len(x):
            self.portOutInteger(x[i])
            i += 1
        return self.endpos

    def portOut_int_array(self, x):
        self.portOutSeqHead("int", len(x), TAG_INT)
        ## for-while
        i = 0
        while i < len(x):
            self.portOutInteger(x[i])
            i += 1
        return self.endpos

    def portOut_long_array(self, x):
        self.portOutSeqHead("long", len(x), TAG_LONG)
        ## for-while
        i = 0
        while i < len(x):
            self.portOutInteger(x[i])
            i += 1
        return self.endpos

    def portOut_float_array(self, x):
        self.portOutSeqHead("float", len(x), TAG_FLOAT)
        ## for-while
        i = 0
        while i < len(x):
            self.portOut(x[i])
            i += 1
        return self.endpos

    def portOut_double_array(self, x):
        self.portOutSeqHead("double", len(x), TAG_DOUBLE)
        ## for-while
        i = 0
        while i < len(x):
            self.portOut(x[i])
            i += 1
        return self.endpos

    ## THIS REQUIRES MORE THOUGHT; IT INVOLVES A DOUBLE BEING WRITTEN OUT
    ## AS A FLOAT (NOT A FLOAT WRITTEN AS A FLOAT), SO MAY NEED ADDITIONAL
    ## LOGIC.  WE DON'T CALL THIS RIGHT NOW (I DON'T THINK) - RMM
    def portOut_float(self, x):
        rc = self.portReserveSpace(7)
        if rc < 0:
            return rc
        b = util.float2raw_int(x)
        s = 0
        if b < 0:
            b = b ^ (-1 << 31)
            s = 1
        e = b >> 23
        b = b | (0xff << 23)
        b = b ^ (0xff << 23)
        self.bufferOut_8(TAG_FLOAT)
        self.bufferOut_8(s)
        self.bufferOut_16(e)
        for shift in range(0, 23, 8):
            self.bufferOut_8(int(0xffL & (b >> shift)))
        return self.endpos

    ## see 'double', think 'float'
    def portOut_double(self, x):
        rc = self.portReserveSpace(11)
        if rc < 0:
            return rc
        b = util.float2raw_long(x)
        s = 0
        if b < 0:
            b = b ^ (-1L << 63)
            s = 1
        e = b >> 52
        b = b | (0x7ffL << 52)
        b = b ^ (0x7ffL << 52)
        self.bufferOut_8(TAG_DOUBLE)
        self.bufferOut_8(s)
        self.bufferOut_16(e)
        ## for-while
        shift = 0
        while shift < 52:
            self.bufferOut_8(int(0xffL & (b >> shift)))
            shift += 8
        return self.endpos

    def portOut_string_array(self, x):
        self.portOutSeqHead("String", len(x), TAG_STRING)
        ## for-while
        i = 0
        while i < len(x):
            self.portOut(x[i])
            i += 1
        return self.endpos

    def portOut_string(self, x):
        if AGDirectLink.ONE_BYTE_AT_A_TIME:
            ## IF THE STRING 'x' IS LONG, WE SHOULD JUST FLUSH THE BUFFER AND
            ## THEN SEND THE WHOLE STRING, INSTEAD OF BREAKING IT DOWN:
            ## WE'LL FIRST DO IT THE CONSERVATIVE WAY; LATER WE WILL TRY THIS OUT:
            if (False):
                ## BUG: THIS 'rc' VALUE ISN'T WORKING (OR SOMETHING):
                rc = self.streamOutFlush()
                self.socket.send(x)
            else:
                rc = self.portOut_string_one_byte_at_a_time(x)
                return rc
        else:
            self.strings_buffer.append(x)
        return rc

    def portOut_string_one_byte_at_a_time(self, x):
        if x is None:
            return self.portOutNull()
        rc = 0
        lgth = len(x)
        if lgth < TAG_SSTR_MAX:
            rc = self.portReserveSpace(1)
            if rc < 0:
                return rc
            self.bufferOut_8(TAG_SSTR_START + lgth)
            ## for-while
            i = 0
            while i < lgth:
                self.portOutInteger(x[i])
                i += 1
        else:
            rc = self.portReserveSpace(1)
            if rc < 0:
                return rc
            self.bufferOut_8(TAG_LSTR)
            rc = self.portOutInteger(lgth)
            if rc < 0:
                return rc
            run = 0
            runChar = 0
            ## for-while
            i = 0
            while i < lgth:
                c = x[i]
                if (run == 0):
                    run = 1
                    runChar = c
                else:
                    if (runChar == c):
                        run += 1
                    else:
                        rc = self.portOutFragment(run, runChar)
                        if rc < 0:
                            return rc
                        run = 1
                        runChar = c
                i += 1
            rc = self.portOutFragment(run, runChar)
        return rc

    def portOutFragment(self, run, runChar):
        rc = self.endpos
        if run < TAG_FRAG_MIN:
            ## for-while
            j = 0
            while j < run:
                rc = self.portOutInteger(runChar)
                if rc < 0:
                    return rc
                j += 1
        else:
            rc = self.portReserveSpace(1)
            if not rc < 0:
                self.bufferOut_8(TAG_FRAG)
            if not rc < 0:
                rc = self.portOutInteger(run)
            if not rc < 0:
                rc = self.portOutInteger(runChar)
        return rc

    def portOutNull(self):
        return self.portOutTag(TAG_NULL)

    ## THIS LOGIC IS SEMI-WRONG: THERE WILL BE PRECIOUS FEW
    ## OBJECTS OF TYPE 'Byte' OR 'Short' FED TO THIS.  RATHER
    ## THERE WILL BE INTS OR STRINGS OF TYPE xsd:byte OR xsd:short
    ## THAT NEED TO BE WRITTEN IN A GIVEN ENCODING. THAT LOGIC ISN'T
    ## HERE YET:
    def portOutUnwrapped(self, arg):
        if arg is None:
            return self.portOutNull()
        elif isinstance(arg, bool):
            return self.portOut_boolean(arg)
        elif isinstance(arg, int):
            return self.portOut_int(arg)
        elif isinstance(arg, long):
            return self.portOut_long(arg)
        elif isinstance(arg, float):
            return self.portOut_double(arg)        
        elif isinstance(arg, UPI):
            return self.portOut_upi(arg)
        elif isinstance(arg, str):
            return self.portOut_string(arg)
        elif not arg: 
            return self.portOutNull()
        elif isinstance(arg, list):
            sample = arg[0]
            if isinstance(sample, int):
                return self.portOut_int_array(arg)
            elif isinstance(sample, long):
                return self.portOut_long_array(arg)
            elif isinstance(sample, float):
                #return self.portOut_float_array(arg)
                return self.portOut_double_array(arg)
            elif isinstance(sample, UPI):
                return self.portOut_upi_array(arg)
            elif isinstance(sample, str) or sample is None:
                return self.portOut_string_array(arg)
            ## these will be invoked rarely, if at all:
            elif isinstance(sample, Byte):
                return self.portOut_byte_array(arg)
            elif isinstance(sample, Character):
                return self.portOut_character_array(arg)
            elif isinstance(sample, Short):
                return self.portOut_short_array(arg)
            elif isinstance(sample, Float):
                return self.portOut_float_array(arg)
            else:
                raise IOException("Can't unwrapp array " + str(arg))
            ## drop through to exception here
        ## these will be invoked rarely, if at all:
        elif isinstance(arg, Byte):
            return self.portOut_byte(arg)
        elif isinstance(arg, Character):
            return self.portOut_character(arg)
        elif isinstance(arg, Short):
            return self.portOut_short(arg)
        elif isinstance(arg, Float):
            return self.portOut_float(arg)
        ## end of rare ones
        raise IOException("Cannot unwrap " + str(arg))

    def bufferOut_8(self, x):
        """
        Write a single byte of information to the socket.
        """
        byte = 0xFF & x;
        char = util.integer_to_character(byte)
        ##print "BUFFER OUT 8  '%s'   '%s' " % (x, byte)
        if AGDirectLink.ONE_BYTE_AT_A_TIME:
            self.byte_at_a_time_buffer[self.endpos] = char
        else:
            self.strings_buffer.append(char)
        self.endpos += 1            
        return self.endpos

    def bufferOut_16(self, x):
        """
        Write two bytes of information to the socket.
        
        TODO: IF THE STRING BUFFER LOOKS VIABLE, RECODE THIS TO SEND 
        THE TWO BYTES AS A SINGLE STRING OF LENGTH TWO:
        """
        byte = 0xff & x
        char = util.integer_to_character(byte)
        if AGDirectLink.ONE_BYTE_AT_A_TIME:
            self.byte_at_a_time_buffer[self.endpos] = char
        else:
            self.strings_buffer.append(char)
        self.endpos += 1
        byte = 0xff & (x >> 8)
        char = util.integer_to_character(byte)
        if AGDirectLink.ONE_BYTE_AT_A_TIME:
            self.byte_at_a_time_buffer[self.endpos] = char
        else:
            self.strings_buffer.append(char)
        self.endpos += 1
        return self.endpos

    def portInUPI(self, tag=None):
        if not tag:
            return self.portInUPI(self.streamInCode())
        ##
        if (tag != TAG_UPI):
            raise IOException("portInOp tag " + tag)
        u = UPI()
        ## for-while
        i = 0
        while i < UPI_WIDTH:
            u.addByte(self.portIn_8())
            i += 1
        return u

    def portInString(self, tag=None):
        if not tag:
            ## first read in a tag; then read in a string:
            #EXPERIMENT
            #return self.portInString(self.portIn_8())
            tag = self.portIn_8()
        lgth = 0
        if (tag == TAG_NULL):
            return None
        if tag < TAG_LSTR:
            raise IOException("portInString tag " + tag)
        if (tag == TAG_LSTR):
            lgth = self.portInLong()
        else:
            if not tag < TAG_SSTR_START and tag < TAG_SSTR_END:
                lgth = tag - TAG_SSTR_START
            else:
                raise IOException("portInString tag " + tag)
        v = []
        run = 0
        runChar = 0
        for i in range(lgth):
            if (run == 0):
                x = self.streamInCode()
                if (x == TAG_FRAG):
                    run = self.portInLong()
                    runChar = util.long_bits_to_character(self.portInLong())
                    v.append(runChar)
                    run -= 1
                else:
                    v.append(util.long_bits_to_character(self.portInLong(x)))
            else:
                v.append(runChar)
                run -= 1
        return ''.join(v)

    def portInLong(self, tag=None):
        if not tag:
            ## EXPERIMENT
            ##return self.portInLong(self.portIn_8())
            tag = self.portIn_8()
        ##
        if tag < TAG_INT_START or not tag < TAG_INT_END:
            raise IOException("portInLong tag " + str(tag))
        s = tag & (TAG_INT_MASK | TAG_SIGN_MASK)
        neg = False
        if s < TAG_IMM_TOP:
            return s
        if s > (TAG_SIGN_MASK - 1):
            neg = True
            s = s - TAG_SIGN_MASK
            if s < TAG_IMM_TOP:
                return (-s) - 1
        count = s - TAG_IMM_TOP + 1
        v = 0
        shift = 0
        w = long(0)
        j = 0
        while j < count:
            w = self.portIn_8()
            if w < 0:
                raise IOException("portInLong->portIn_8=" + w)
            v = v | (w << shift)
            shift += 8
            j += 1
        if (count == 1):
            v = v + TAG_IMM_TOP
        if neg:
            v = (-v) - 1
        return v

    def portInSequence(self, tag=None):
        print "PIS", 
        if not tag:
            ## EXPERIMENT
            #return self.portInSequence(self.streamInCode())
            tag = self.streamInCode()
        ##
        lgth = self.portInLong()
        sub = self.portIn_8()
        return self.portInSeqBody(tag, lgth, sub)

    ## NOT YET FULLY-IMPLEMENTED.  
    def portInFloat(self, tag=None):
        """
        Tricky: We need to read a float in, but Python does not support
        single-precision float as a native type.  We need to wrap the
        result in a 'Float' object.
        """
        if not tag:
            return self.portInFloat(self.streamInCode())
        ##
        if (tag != TAG_FLOAT):
            raise IOException("portInFloat tag " + tag)
        s = self.portIn_8()
        if s < 0:
            raise IOException("portInFloat->portIn_8=" + s)
        e0 = self.portIn_8()
        if e0 < 0:
            raise IOException("portInFloat->portIn_8=" + e0)
        e1 = self.portIn_8()
        if e1 < 0:
            raise IOException("portInFloat->portIn_8=" + e1)
        w = 0
        v = 0
        ## for-while
        for shift in range(0, 23, 8):
            w = self.portIn_8()
            if w < 0:
                raise IOException("portInFloat->portIn_8=" + w)
            v = v | (w << shift)
        v = v | (((e1 << 8) | e0) << 23)
        if (s != 0):
            v = v | (-1 << 31)
        #r = intBitsToFloat(v)
        raise UnimplementedMethodException("intBitsToFloat")
        #return r

    def streamInValue(self, tag=None):
        """
        Return one complete tagged item from the input stream.

        Primitive values are returned as object instances. Operation is returned
        as Object[]={opname, resname, part,...} Sequences are returned as vectors
        of (primitive) types Currently, there are no Object instances defined

        NOTE: WE RETURN WRAPPED OBJECTS FOR Short, Byte, Float, and Character HERE
        BUT THAT MAY OR MAY NOT BE A MISTAKE.  I DON'T YET UNDERSTAND THIS CODE WELL ENOUGH TO KNOW - RMM
        """
        #print "SIV ",
        if tag is None: 
            return self.streamInValue(self.streamInCode())
        if tag < TAG_START:
            raise IOException("streamInValue tag " + tag)
        if tag < TAG_INT_END:
            w = long(self.portInLong(tag))
        else:
            if tag == TAG_NULL:
                w = None
            elif tag == TAG_BYTE:
                w = Byte(self.portInLong())
            elif tag == TAG_SHORT:
                w = Short(self.portInLong())
            elif tag == TAG_INT:
                w = int(self.portInLong())
            elif tag == TAG_CHAR:
                char = util.long_bits_to_character(self.portInLong())                
                w = Character(char)
            elif tag == TAG_FLOAT:
                raise Exception("WE SHOULDN'T BE DOING THIS")
                w = Float(self.portInFloat(tag))
            elif tag == TAG_DOUBLE:
                w = float(self.portInDouble(tag))
            elif tag == TAG_SEQ:
                w = self.portInSequence(tag)
            elif tag == TAG_SPARSE:
                w = self.portInSparse(tag)
            elif tag == TAG_OBJECT:
                w = self.portInObject(tag)
            elif tag == TAG_TRUE:
                w = True
            elif tag == TAG_FALSE:
                w = False
            elif tag == TAG_OPENd:
                raise IOException("streamInValue tag " + tag)
            elif tag == TAG_OP:
                w = self.portInOp(tag)
            elif tag == TAG_END:
                raise IOException("streamInValue tag " + tag)
            elif tag == TAG_UPI:
                w = self.portInUPI(tag)
            elif tag == TAG_BYTES:
                w = self.portInBytes(tag)
            else:
                w = self.portInString(tag)
        return w

    def streamInCode(self, code=None):
        if not code:
            if (self.haveCode == 1000000):
                return self.portIn_8()
            r = self.haveCode
            self.haveCode = 1000000
            return r
        else:   
            if (self.haveCode == 1000000):
                self.haveCode = code
                return code
            return self.ERR_PROTOCOL

    def portInBytes(self, tag=None):
        if not tag:
            return self.portInBytes(self.streamInCode())
        ## handle tag:
        if (tag != TAG_BYTES):
            raise IOException("portInOp tag " + tag)
        b = [0 for i in range(self.portInLong())]
        for i in range(len(b)):
            b[i] = 0xff & self.portIn_8()
        return b

    def portInDouble(self, tag=None):
        """
        Python's float is a C/Java double, so that is what get's read in here.
        Also, when read back out, the Python float is tagged as a double.
        """
        if not tag:
            return self.portInDouble(self.streamInCode())
        ##
        if (tag != TAG_DOUBLE):
            raise IOException("portInDouble tag " + tag)
        s = self.portIn_8()
        if s < 0:
            raise IOException("portInDouble->portIn_8=" + s)
        e0 = self.portIn_8()
        if e0 < 0:
            raise IOException("portInDouble->portIn_8=" + e0)
        e1 = self.portIn_8()
        if e1 < 0:
            raise IOException("portInDouble->portIn_8=" + e1)
        v = long(0)
        ## for-while
        shift = 0
        while shift < 51:
            w = self.portIn_8()
            if w < 0:
                raise IOException("portInDouble->portIn_8=" + w)
            v = v | (w << shift)
            shift += 8
        v = v | (((e1 << 8) | e0) << 52)
        if (s != 0):
            v = v | (-1L << 63)
        r = util.long_bits_to_float(v)
        return r

    def portInOp(self, tag=None):
        if not tag:
            #EXPERIMENT
            tag = self.streamInCode()
        ##
        if (tag != TAG_OP):
            raise IOException("portInOp tag " + tag)
        op = self.portInString()
        opix = self.portInLong()
        lgth = self.portInLong()
        #w = [None for i in range(lgth + 2)]
        w = [None] * (lgth + 2)
        w[0] = op
        w[1] = long(opix)
        for i in range(lgth):
            w[i + 2] = self.streamInValue()
        s1 = "Unknown"
        s2 = "Unknown"
        if lgth > 0:
            s1 = self.stringValue(w[2])
        if lgth > 1:
            s2 = self.stringValue(w[3])
        if opix < 0:
            raise IllegalArgumentException("Operation " + str(op) + "[" + str(-opix) + "] signalled an error in server: " + str(s1) + " -- " + str(s2))
        return w
    
    def receive_next_byte(self):
        self.socket_cursor += 1;
        if self.socket_cursor >= len(self.socket_chunk):
            self.socket_chunk = self.socket.recv(8192)
            #self.socket_chunk = self.socket.recv(16384)            
            #self.socket_chunk = self.socket.recv(32768)                        
            self.socket_cursor = 0        
            #print "R ", len(self.socket_chunk), " ",             
        return self.socket_chunk[self.socket_cursor]
        
