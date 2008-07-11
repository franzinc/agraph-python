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
import threading, time, subprocess, weakref
from franz.allegrograph.exceptions import *
from franz.transport.agc import *
from franz.transport.agconnector import AGConnector
from franz.transport.agdirectconnector import AGDirectConnector
from franz.transport import agconnector
from franz.allegrograph.allegrograph import AllegroGraph
from franz.allegrograph.cursor import Cursor
#from franz.namespaceregistry import NamespaceRegistry

class AllegroGraphConnection(object):
    """ 
    Implement access to AllegroGraph triple stores through a server interface.
 
    One instance of this class can support access to several open
    triple stores, each with its own AllegroGraph instance.  A Java
    application may connect to multiple servers by creating multiple
    instances of this class.
    """
    def __init__(self):
        self.pollCount = AllegroGraphConnection.defaultPollCount
        self.pollInterval = AllegroGraphConnection.defaultPollInterval
        self.timeout = AllegroGraphConnection.defaultTimeout
        self.serverId = -1
#        self.c = System.getProperty("com.franz.ag.exec", "")
#        self.p = Preferences()
#        self.ag = Preferences()
#        self.defaultLispCommand = c
        self.agc = None
        self.mode = AllegroGraphConnection.defaultMode
        self.host = AllegroGraphConnection.defaultHost
        self.port = AllegroGraphConnection.defaultPort
        self.port2 = AllegroGraphConnection.defaultPort2
        self.debug = AllegroGraphConnection.defaultDebug
        self.lispCommand = AllegroGraphConnection.defaultLispCommand
        self.keep = AllegroGraphConnection.defaultServerKeep
        self.debugMask = 0
        self.allTS = []
        ## TODO: MAKE THIS SYNCHRONIZED:
        self.valueMap = weakref.WeakValueDictionary()
        self.ag_process = None
        self.disableLock = threading.RLock()
        self.registeredTripleStoresLock = threading.RLock()
        
    defaultMode = "direct"
    defaultHost = ""
    defaultPort = 4567
    defaultPort2 = 4568
    defaultDebug = 0
    defaultLispCommand = ""
    defaultServerKeep = False
    defaultPollCount = 4
    defaultPollInterval = 3
    defaultTimeout = 10
## COMMENT BACK IN IF NECESSARY:
#    initialns = NamespaceRegistry(NamespaceRegistry.RDFandOwl)
#    nsregs = NamespaceRegistry(initialns)
    geoSubs = [None] * 256

    agPrefPath = "franz.com/allegrograph/java";
    agLispKey = "lispcommand";
    
    ## NOT SURE HOW TO USE THESE; DON'T KNOW THE SYSTEM COMMANDS:
#    c = System.getProperty("com.franz.ag.exec", "");
#    if c.equals(""):
#        p = Preferences.userRoot();
#        ag = p.node(agPrefPath);
#        c = ag.get(agLispKey, "");
#    if c.equals(""):
#        p = Preferences.systemRoot();
#        ag = p.node(agPrefPath);
#        c = ag.get(agLispKey, "");
#    defaultLispCommand = c;
    
    RENEW = "renew"
    ACCESS = "access"
    OPEN = "open"
    CREATE = "create"
    REPLACE = "replace"

    @staticmethod
    def getInitialNamespaceRegistry():
        return AllegroGraphConnection.initialns
    
    def addValueMapItem(self, key, value):
        """
        Record a value-map entry (not sure who uses it)
        TODO: FIGURE OUT IF THE MAP SHOULD HAVE WEAK KEYS, OR WEAK VALUES, OR BOTH (HARDER)
        TODO: MAKE THIS SYNCHRONIZED
        """
        self.valueMap[key] = value        

    @staticmethod
    def getDefaultPollCount():
        return AllegroGraphConnection.defaultPollCount

    @staticmethod
    def getDefaultPollInterval():
        return AllegroGraphConnection.defaultPollInterval

    def getPollCount(self):
        return self.pollCount

    def getPollInterval(self):
        return self.pollInterval

#     * Set the static default values of the poll count and poll interval 
#     * parameters.
#     * @param count an integer that determines the number of connection attempts
#     * @param interval an integer that determines the interval between 
#     *     attempts to connect
#     *     
#     *     <p>A positive value greater than zero is used to set the
#     *     corresponding parameter.  If a zero or negative value is
#     *     specified, the corresponding parameter is unchanged.
#     *    <p>The initial static defaults are 2 for poll count and 500
#     *    for poll interval.  
    @staticmethod
    def setDefaultPolling(count, interval):
        if count > 0:
            AllegroGraphConnection.defaultPollCount = count
        if interval > 0:
            AllegroGraphConnection.defaultPollInterval = interval

    
#     * Set the current values of the poll count and poll interval 
#     * parameters.
#     * @param count an integer that determines the number of connection attempts
#     * @param interval an integer that determines the interval between 
#     *     attempts to connect
#     *     
#     *     <p>A positive value greater than zero is used to set the
#     *     corresponding parameter.
#     *     <p> If a zero value is specified, set the current value of the 
#     *     parameter from the static default value.
#     *     <p>If a negative value is
#     *     specified, the corresponding parameter is unchanged.
#     *     
#     *     <p>The inital values are set from the static default values.
#     *     <p>If an application encounters timeout 
#     *    exceptions when attempting a connection one or both of these
#     *    parameters should be given a larger value. 
    def setPolling(self, count, interval):
        if not count < 0:
            self.pollCount = count if count > 0 else self.defaultPollCount
        if not interval < 0:
            self.pollInterval = interval if interval > 0 else self.defaultPollInterval

    def getServer(self):
        if self.agc is None:
            raise IllegalStateException("Server is not enabled.")
        return self.agc

    @staticmethod
    def getDefaultMode():
        return AllegroGraphConnection.defaultMode

    def getMode(self):
        return self.mode

    @staticmethod
    def getDefaultPort():
        return AllegroGraphConnection.defaultPort

    @staticmethod
    def setDefaultPort(port):
        AllegroGraphConnection.defaultPort = AllegroGraphConnection.port

    def getPort(self):
        return self.port

    def setPort(self, p):
        self.port = p

    @staticmethod
    def getDefaultPort2():
        return AllegroGraphConnection.defaultPort2

    @staticmethod
    def setDefaultPort2(port):
        AllegroGraphConnection.defaultPort2 = AllegroGraphConnection.port

    def getPort2(self):
        return self.port2

    def setPort2(self, p):
        self.port2 = p

    @staticmethod
    def getDefaultHost():
        return AllegroGraphConnection.defaultHost

    @staticmethod
    def setDefaultHost(host):
        AllegroGraphConnection.defaultHost = AllegroGraphConnection.host

    def getHost(self):
        return self.host

    def setHost(self, newhost):
        self.host = newhost

    @staticmethod
    def getDefaultDebug():
        return AllegroGraphConnection.defaultDebug

    @staticmethod
    def setDefaultDebug(db):
        AllegroGraphConnection.defaultDebug = db

    def getDebug(self):
        return self.debug

    def setDebug(self, db):
        self.debug = db

    @staticmethod
    def getDefaultCommand():
        return AllegroGraphConnection.defaultLispCommand

    @staticmethod
    def setDefaultCommand(cmd):
        if cmd is None:
            raise IllegalArgumentException("command cannot be null")
        AllegroGraphConnection.defaultLispCommand = cmd

    def getCommand(self):
        return self.lispCommand

    def setCommand(self, cmd):
        if cmd is None:
            raise IllegalArgumentException("command cannot be null")
        self.lispCommand = cmd

    @staticmethod
    def getDefaultServerKeep():
        return AllegroGraphConnection.defaultServerKeep

    def setDefaultServerKeep(self, v):
        self.defaultServerKeep = v

    def getServerKeep(self):
        return self.keep

    def setServerKeep(self, v):
        self.keep = v

#     * Enable the interface by connecting to an AllegroGraph server.
#     * The connection is determined by the following values:
#     * <ul>
#     *      <li>The integer port number where the AllegroGraph server is
#     *            listening is queried with getPort() and modified with 
#     *            setPort().
#     *      <li> The name of the host where the AllegroGraph server is
#     *            listening is queried with getHost() and modified with 
#     *            setHost(). This can normally be "localhost" or "".
#     *      <li> The connection mode is queried with getMode() and
#     *           modified with setDirect() or setJLinker().
#     *      <li> The integer port number for the second socket in the
#     *            AllegroGraph/Java connection. Some implementations of the
#     *            AllegroGraph server use two sockets. This argument is ignored
#     *            if the server uses only one socket.
#     *  </ul>
#     * @throws IOException
#     *             if the interface cannot be enabled.
#     *  <p>
#     *  Only one instance with mode "jlinker" can be enabled at any one time
#     *  in each running Java application.
#     *  <ul>
#     *     <li>Direct Mode Exceptions:
#     *         <ul>
#     *         <li>"Too many connections." -- The AllegroGraph Version 1.2
#     *                  server allows only one connection at a time.
#     *         <li>"Connection rejected." -- The AllegrGraph server 
#     *                  application filter function rejected the connection.
#     *         <li>"Connected but timed out."  -- The expected server
#     *                  acknowledgment byte did not arrive in the expected
#     *                  time interval.
#     *         <li>"Unexpected initial reply "  -- A byte arrived from the
#     *                  server but it was unexpected.
#     *         <li>other socket error
#     *         </ul>
#     *      <li>JLinker Mode Exceptions:
#     *          <ul>
#     *          <li>"Only one enabled JLinker instance allowed."
#     *          <li>"Connection failed with null explanation."
#     *          <li>"Connection failed with empty explanation."
#     *          <li>other jlinker error
#     *          <li>other socket error
#     *          </ul>
#     * 
#     * 
#     * @return true if the connection was not enabled and now is.
#     *     Return false if the connection was already enabled.
    def enable_socket_connection(self):
        if self.agc is None:            
            self.agc = AGDirectConnector.createConnector(self.mode)
        elif -1 < self.agc.query():
                return False
        self.agc.initialize(self.port, self.port2, self.host, self.pollCount, self.pollInterval, self.debug, self.timeout)
        self.agc.enable_socket_connection()
        if self.debug > 0:
            self.traceServer(True)
        v = None
        db = 0
        w = None
        try:
            w = self.agc.serverOption("client-debug", "")
        except Exception:
            pass
        if v:
            db = agconnector.toInt(w)        
        l = 0
        try:
            w = self.agc.serverOption("client-batch", "")
        except Exception:
            pass
        if w:
            l = agconnector.toInt(w)
        if l > 0:
            Cursor.defaultLookAhead = l
        pr = 0
        try:
            w = self.agc.serverOption("server-level", "")
        except Exception:
            pass
        if w:
            pr = agconnector.toInt(w)
        if pr < AGU_PROTOCOL_LEVEL:
            raise AllegroGraphException(
                "AllegroGraph server is out of date: " + pr + "<" + AGU_PROTOCOL_LEVEL)
        elif pr > AGU_PROTOCOL_LEVEL:
            raise AllegroGraphException(
                "Library agraph.jar is out of date: " + pr + ">" + AGU_PROTOCOL_LEVEL)        
        try:
            self.agc.serverOption("client-level", AGU_PROTOCOL_LEVEL)
        except Exception:
            pass
        self.debugMask = db | 0x40000000
        self.serverId = self.agc.serverId()
        if self.serverId < 101:
            raise AllegroGraphException("AllegroGraph server is out of date serverId: " + self.serverId)
        return True

    def interrupt(self):
        id = self.serverId
        if id < 0:
            return -10
        if (id == 0):
            return -9
        if id < 101:
            return -8
        if not self.isEnabled():
            return -7
        r = 0
        if self.isBusy():
            temp = AllegroGraphConnection()
            temp.setHost(self.getHost())
            temp.setPort(self.getPort())
            temp.enable_socket_connection()
            r = temp.agc.interruptServer(id)
            temp.disable(False)
        return r

    def ifDebug(self, index):
        mask = 0
        if index > 0:
            mask = 1 << index - 1
        if self.debugMask & mask > 0:
            return True
        return False

    def finalize(self):
        self.disable(True)
        if not self.keep:
            self.stopServer()

    def isEnabled(self):
        if self.agc is None:
            return False
        return -1 < self.agc.query()

    def isBusy(self):
        if self.agc is None:
            return False
        return 0 < self.agc.query()

    def exists(self, name, directory):
        return self.getServer().exists(name, directory)

    def help_access(self, verb, name=None, directory=None, ag_store=None):
        if ag_store:
            self.ag.connect(verb, self)
            return self.ag
        else:    
            return AllegroGraph(self, verb, name, directory)

    def create(self, name=None, directory=None, ag_store=None):
        return self.help_access(AllegroGraphConnection.CREATE, name=name, directory=directory, ag_store=ag_store)  

    def open(self, name=None, directory=None, ag_store=None):
        return self.help_access(AllegroGraphConnection.OPEN, name=name, directory=directory, ag_store=ag_store)  

    def access(self, name=None, directory=None, ag_store=None):
        return self.help_access(AllegroGraphConnection.ACCESS, name=name, directory=directory, ag_store=ag_store)  

    def renew(self, name=None, directory=None, ag_store=None):
        return self.help_access(AllegroGraphConnection.RENEW, name=name, directory=directory, ag_store=ag_store)  

    def replace(self, name=None, directory=None, ag_store=None):
        return self.help_access(AllegroGraphConnection.REPLACE, name=name, directory=directory, ag_store=ag_store)  

    def getChunkSize(self):
        w = self.getServer().serverOption("chunk-size", -1)
        if w: return agconnector.longValue(w);
        else: raise AllegroGraphException("Cannot get chunk size.")

    def setChunkSize(self, s):
        if s < 1: raise IllegalArgumentException("ChunkSize must be positive.")
        else: self.getServer().serverOption("chunk-size", long(s))

    def getDefaultExpectedResources(self):
        w = self.getServer().serverOption("expected", -1)
        if w: return agconnector.longValue(w)
        else: raise AllegroGraphException("Cannot get expected resource number.")

    def setDefaultExpectedResources(self, s):
        if s < 1: raise IllegalArgumentException( "ExpectedResources must be positive.")
        else: self.getServer().serverOption("expected", long(s))        

    def evalInServer(self, expression):
        return self.getServer().evalInServer(expression)

#    def bindInServer_0(self, var, value):
#        if (None == environment):
#            self.evalInServer("(" + AGJ_BIND + " '" + var + " " + value + ")")
#        else:
#            self.evalInServer("(" + AGJ_BIND + " '" + var + " '" + value + ")", environment)

    def traceServer(self, onoff):
        try:
            self.getServer().traceServer(onoff, None)
            return 0
        except (AllegroGraphException, ), e:
            return -1

    def serverLevel(self, level):
        return self.getServer().serverLevel(level)

    def serverTrace(self, outFile):
        try:
            self.getServer().traceServer(True, outFile)
            return 0
        except (AllegroGraphException, ), e:
            return -1

    def __str__(self):
        return str(type(self)) + "<" + str(self.serverId) + " " + str(self.mode) + ">"

    def enableHasValueReasoning(self):
        self.evalInServer("(db.agraph.servers::ag-add-has-value)")

    def federate(self, name, parts, supersede):
        return AllegroGraph.createFederatedTripleStoreComponent(self, name, parts, supersede)

    def findStore(self, name, directory=None):
        return AllegroGraph(self, None, name, directory)

    def deleteStore(self, name, directory=None):
        self.deleteStore(name, directory)

    @staticmethod
    def getDefaultTimeout():
        return AllegroGraphConnection.defaultTimeout

    @staticmethod
    def setDefaultTimeout(defaultTimeout):
        AllegroGraphConnection.AllegroGraphConnection.defaultTimeout = AllegroGraphConnection.defaultTimeout

    def getTimeout(self):
        return self.timeout

    def setTimeout(self, timeout):
        self.timeout = self.timeout

    def disable(self, close=True):
        with self.disableLock:
            self.getServer()
            all = self.allTS.toArray()
            if close:
                ## for-while
                i = 0
                while i < len(all):
                    s = all[i]
                    if not s.tsx < 0:
                        try:
                            self.agc.closeTripleStore(s, True)
                            s.tsx = -1
                        except Exception, e:
                            pass
                    i += 1
            if self.agc is not None and -1 < self.agc.query():
                self.agc.disable()
            self.agc = None
            self.serverId = 0

    def startServer(self, log=None):
        """
         * Start a system process running an AllegroGraph server.
         * 
         * @param log The name of a log file or null.  If non-null, the AG server
         *           writes a log to this file in the home directory.
         * @throws IOException from Runtime.exec().
         * 
         * <p>
         * The local command required to start the AllegroGraph server
         * may be specified once for some installation by running the main()
         * method in this class as described in the method documentation.
         * A user preference takes precedence over a system preference.
         * <p>
         * Or, the local command may be specified each time the Java application 
         * is run by specifying the system property "com.franz.ag.exec".  The 
         * setting of the system property takes precedence over a user or system 
         * preference setting.
         * <p>
         * Or, the local command may be specified explicitly from the application
         * program with the setCommand() or setDefaultCommand() methods.
        """
     
        if self.ag_process is not None:
            raise IllegalStateException("AllegroGraph has already started.")
        if self.lispCommand == "":
            raise IllegalStateException("ACL command must be set")
        options1 = ""
        options2 = ""
        if log is not None:
            options1 = "-log"
            options2 = log
        cmd = [self.lispCommand, "--", "-mode", self.mode, "-port", ""+ self.port, "-port2", 
               ""+ self.port2, options1, options2,]
        if self.debug > 0:
            m = ""
            ## for-while
            for i in range(len(cmd)):
                m = m + " " + cmd[i]
            print "startServer command:" + m
        self.ag_process = subprocess.Popen([cmd])
        try:
            time.sleep(1)  ## sleep time in seconds
        except InterruptedException, e:
            pass

    def stopServer(self, killNotStarted=False):
        rc = 0
        if self.debug > 0:
            print "stopServer: " , self.ag_process , " " , self.agc
        if self.ag_process is None:
            if not killNotStarted:
                return -1
        if self.agc is None:
            try:
                if self.debug > 0:
                    print "stopServer calling enable"
                en = self.enable_socket_connection()
                if self.debug > 0:
                    print "stopServer enable returned " + en
            except (Exception, ), e:
                if self.debug > 0:
                    print "connect " + e
                self.agc = None
                rc += 1
        if self.agc is not None:
            if 0 > self.agc.query():
                if self.debug > 0:
                    print "stopServer calling re-enable"
                self.agc.enable_socket_connection()
            if -1 < self.agc.query():
                if self.debug > 0:
                    print "stopServer calling stop-agj-application"
                r = ""
                try:
                    ra = self.agc.evalInServer("(" + AG_STOP_AGJ_APPLICATION + " nil)")
                except (Exception, ), e:
                    ra = ["ERROR: " + e]
                    rc += 1
                if len(ra) < 1:
                    r =  ["ERROR: zero values"]
                    rc += 1
                else:
                    if isinstance(ra[0], str):
                        r = ra[0]
                    else:
                        if ra[0] is not None:
                            r = "ERROR: odd type " + ra[0]
                            rc += 1
                if self.debug > 0:
                    print "stopServer stop-agj-ap => " + r
                if self.debug > 0:
                    print "stopServer calling disable"
                self.disable()
                try:
                    time.sleep(3)
                except (InterruptedException, ), e:
                    pass
            else:
                if self.debug > 0:
                    print "stopServer re-enable failed"
        if self.ag_process is not None:
            if self.debug > 0:
                print "stopServer calling destroy"
            self.ag_process.destroy()
            try:
                self.ag_process.waitFor()
            except (InterruptedException, ), e:
                pass
        return rc
    
#####################################################################################
## Refactored methods
#####################################################################################

    def addTS(self, ts):
        with self.registeredTripleStoresLock:
            self.allTS.append(ts)
            
    def removeTS(self, ts):
        with self.registeredTripleStoresLock:
            self.allTS.remove(ts)

#####################################################################################
## eigher eliminate namespace registry logic or harmonize it with Sesame namespaces
#####################################################################################

    def getNamespaces(self):
        if self.nsregs is None:
            return
        return self.nsregs.stringArray()

    def getNamespaceRegistry(self):
        if (None == self.nsregs):
            return
        return NamespaceRegistry(self.nsregs)

    def nsregsInit(self):
        """
        Anything this ugly deserves to die.  - RMM
        """        
        if self.nsregs is not None:
            return self.nsregs
        if self.initialns is None:
            raise IllegalStateException("The default NamespaceRegistry is suppressed in" + " this instance of AllegroGraphConnection")
        self.nsregs = NamespaceRegistry(self.initialns)
        return self.nsregs

    def registerNamespace(self, prefix, full):
        self.nsregsInit().register(prefix, full)

    def registerNamespaces(self, ns_or_defs):
        if isinstance(ns_or_defs, list):
            self.nsregsInit()
            ## for-while
            i = 0
            while i < len(ns_or_defs):
                self.nsregs.register(ns_or_defs[i], ns_or_defs[i + 1])
                i = i + 2
        else:
            self.nsregsInit().register(ns_or_defs)

    def setNamespaceRegistry(self, ns):
        if ns is None:
            self.nsregs = None
        else:
            self.nsregs = NamespaceRegistry(ns)

#####################################################################################
## 
#####################################################################################

    ## NOT SURE IF WE NEED THIS, BUT DON'T KNOW THE SYSTEM CALL EQUIVALENTS IN PYTHON:  - RMM
    @staticmethod
    def main(args):
        sys = ""
        usr = ""
        ## for-while
        i = 0
        while i < args.length:
            string = args[i]
            if string.lower() == "-system".lower():
                sys = args[i]
                i += 1
            else:
                if string.lower() == "-user".lower():
                    usr = args[i]
                    i += 1
            i += 1
        ps = Preferences.systemRoot()
        ags = ps.node(AllegroGraphConnection.agPrefPath)
        agcs = ags[AllegroGraphConnection.agLispKey, ""]
        pms = "System value of " + AllegroGraphConnection.agPrefPath + "/" + AllegroGraphConnection.agLispKey
        if agcs == "":
            print "Current " + pms + " is unset."
        else:
            print "Current " + pms + " is " + agcs
        pu = Preferences.userRoot()
        agu = pu.node(AllegroGraphConnection.agPrefPath)
        agcu = agu[AllegroGraphConnection.agLispKey, ""]
        pmu = "User value of " + AllegroGraphConnection.agPrefPath + "/" + AllegroGraphConnection.agLispKey
        if agcu == "":
            print "Current " + pmu + " is unset."
        else:
            print "Current " + pmu + " is " + agcu
        if not sys == "":
            ags.put(AllegroGraphConnection.agLispKey, sys)
            print "Modified " + pms + " to " + sys
        if not usr == "":
            agu.put(AllegroGraphConnection.agLispKey, usr)
            print "Modified " + pmu + " to " + usr


if __name__ == '__main__':
    import sys
    AllegroGraphConnection.main(sys.argv)

