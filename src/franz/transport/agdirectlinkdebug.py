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


from franz.exceptions import IllegalArgumentException, IllegalStateException
from franz.util import *
from franz.transport.agdirectlink import AGDirectLink
from franz.allegrograph.upi import UPI
from franz.util import *

class AGDirectLinkDebug(AGDirectLink):
    """ 
    Debugging help for AGDirect Link
    """
    def __init__(self, host, port, pc, pi, timeout=None):
        super(AGDirectLinkDebug, self).__init__(host, port, pc, pi, timeout)
        timeoutString = " timeout " + str(timeout) if timeout else ""
        self.dp(1, "new " + str(host) + ":" + str(port) + " " + str(pc) + "/" + str(pi) + timeoutString)

    def dp(self, level, text):
        if not level > self.debugClient:
            head = ""
            for i in range(level):
                head = head + "  "
            print head + text

    def sendOpHeader(self, op, style, argCount):
        self.dp(1, "opHeader: " + str(op) + " " + str(style) + " " + str(argCount))
        return super(AGDirectLinkDebug, self).sendOpHeader(op, style, argCount)

    def sendOpTail(self, op, opix, style, rx):
        self.dp(1, "opTail: " + op + " " + str(opix))
        return super(AGDirectLinkDebug, self).sendOpTail(op, opix, style, rx)

    def opResIn(self, op, opix, rx):
        r = super(AGDirectLinkDebug, self).opResIn(op, opix, rx)
        self.dp(1, "opRes=" + str(op) + " " + str(opix) + " " + str(r))
        return r

    def streamInValue(self, tag=None):
        if not tag: return super(AGDirectLinkDebug, self).streamInValue(tag) 
        w = super(AGDirectLinkDebug, self).streamInValue(tag)
        self.dp(2, "streamIn=" + str(tag) + ": " + str(w))
        return w

    def portInSeqBody(self, tag, len, sub):
        self.dp(2, "portInSequence:" + str(tag) + "(" + str(len) + ")" + str(sub))
        w = super(AGDirectLinkDebug, self).portInSeqBody(tag, len, sub)
        self.dp(2, "portInSequence=" + str(w))
        return w

    def portInOp(self, tag=None):
        if tag: self.dp(2, "portInOp:0x" + integer_to_hex(tag))
        return super(AGDirectLinkDebug, self).portInOp(tag)

    def portIn_8(self):
        res = super(AGDirectLinkDebug, self).portIn_8()
        self.dp(5, "portIn_8=" + str(res) + "  0x" + str(integer_to_hex(res)) + "  0b" + str(integer_to_binary(res)))
        return res

    def portOutInteger(self, x):
        xx = character_to_integer(x) if isinstance(x, str) else x
        self.dp(4, "portOutInteger: " + str(x) + "  " + str(xx))
        return super(AGDirectLinkDebug, self).portOutInteger(x)

    def portInLong(self, tag=None):
        if not tag: return super(AGDirectLinkDebug, self).portInLong(tag) 
        self.dp(2, "portInLong: tag=0x" + integer_to_hex(tag))
        r = super(AGDirectLinkDebug, self).portInLong(tag)
        self.dp(2, "portInLong= " + str(r))
        return r

    def portInString(self, tag=None):
        if not tag: return super(AGDirectLinkDebug, self).portInString(tag)     
        self.dp(2, "portInString: tag 0x" + integer_to_hex(tag))
        v = super(AGDirectLinkDebug, self).portInString(tag)        
        self.dp(2, "portInString= " + v)
        return v

    def portInDouble(self, tag):
        r = super(AGDirectLinkDebug, self).portInDouble(tag)
        self.dp(2, "portInDouble= " + str(r))
        return r

    def portInFloat(self, tag):
        r = super(AGDirectLinkDebug, self).portInFloat(tag)
        self.dp(2, "portInFloat= " + str(r))
        return r

    def portInBytes(self, tag):
        b = super(AGDirectLinkDebug, self).portInBytes(tag)
        self.dp(2, "portInBytes= " + str(len(b)))
        return b

    def portInUPI(self, tag):
        b = super(AGDirectLinkDebug, self).portInUPI(tag)
        self.dp(2, "portInUPI= " + str(b))
        return b

    def portFlush(self):
        if self.endpos > 0:
            pc0 = self.byte_at_a_time_buffer[0]            
            c0 = character_to_integer(pc0) if isinstance(pc0, str)  else pc0
            pc1 = self.byte_at_a_time_buffer[1]
            c1 = character_to_integer(pc1) if isinstance(pc1, str)  else pc1
            self.dp(4, "portFlush: " + str(self.endpos) + " bytes: " + str(c0) + " " + str(c1) + "...")
        return super(AGDirectLinkDebug, self).portFlush()

    def portOutTag(self, tag):
        rc = super(AGDirectLinkDebug, self).portOutTag(tag)
        self.dp(4, "portOut tag= " + str(tag))
        return rc

    def portOut_int(self, x):
        self.dp(2, "portOut int: " + str(x))
        return super(AGDirectLinkDebug, self).portOut_int(x)
    
    def portOut_long(self, x):
        self.dp(2, "portOut long: " + str(x))
        return super(AGDirectLinkDebug, self).portOut_long(x)

    def portOut_string(self, x):
        self.dp(2, "portOut String: " + x);
        return super(AGDirectLinkDebug, self).portOut_string(x);

    def portOut_upi (self, x):
        self.dp(2, "portOut UPI: " + str(x));
        return super(AGDirectLinkDebug, self).portOut_upi(x);

    def portOut(self, x):
#        if isinstance(x, int):
#            self.dp(2, "REDUNDANT portOut int: " + str(x))
        if isinstance(x, long):        
            self.dp(2, "portOut long: " + str(x))
        elif isinstance(x, str):        
            #self.dp(2, "portOut str: " + str(x))  APPEARS TO BE REDUNDANT, GIVEN portOut_string DEBUGGING
            pass
        elif isinstance(x, UPI):        
            self.dp(2, "portOut UPI: " + str(x))
        elif isinstance(x, list):  
            if not list: ## empty list   
                self.dp(2, "portOut array: " + str(x))
            sample = x[0]
            if isinstance(sample, long):
                self.dp(2, "portOut long array: " + str(x))
            elif isinstance(sample, str):
                self.dp(2, "portOut str array: " + str(x))
            elif isinstance(sample, int):
                self.dp(2, "portOut int array: " + str(x))
            elif isinstance(sample, UPI):
                self.dp(2, "portOut UPI array: " + str(x))
        return super(AGDirectLinkDebug, self).portOut(x)

    def portOutSeqHead(self, caller, len, tag):
        self.dp(2, "portOut seq: of " + caller + "[" + str(len) + "] tag=" + str(integer_to_hex(tag)))
        return super(AGDirectLinkDebug, self).portOutSeqHead(caller, len, tag)

    def bufferOut_8(self, x):
        if x < 0:
            print "BREAK NEGATIVE"
        self.dp(5, "bufferOut_8: " + str(x) + "  0x" + integer_to_hex(x) + "  0b" + integer_to_binary(x))
        return super(AGDirectLinkDebug, self).bufferOut_8(x)

    def bufferOut_16(self, x):
        self.dp(5, "bufferOut_16: " + str(x))
        return super(AGDirectLinkDebug, self).bufferOut_16(x)


