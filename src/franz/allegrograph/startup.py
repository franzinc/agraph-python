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

import os
import traceback

from franz import util
from franz.exceptions import NiceException, RuntimeException, AllegroGraphException, IOException, ConnectException, FakeException
#from franz.agconnector import AGConnector
from franz.transport.agconnection import AllegroGraphConnection
from franz.transport.agdirectlink import AGDirectLink
from franz.allegrograph.quad import Quad

class StartUp(object):
    """ 

    """
    debug = 0
    quiet = False
    DEFAULT_AGSERVER_LOCATION_MAP = {"Darwin": "/Applications/AllegroGraph/AllegroGraphJavaServer", }
    Quad.COMPLAIN_BITTERLY = True

    @staticmethod
    def startServer(agConn, agJavaServerPath):
        if not StartUp.quiet:
            print "Starting server ..."
        try:
            agConn.startServer()
        except ConnectException, ex:
            ## This isn't a good method, but we don't know the right one.
            ## For Mac OS, it returns 'Darwin':
            thisOS = os.uname()[0]
            defaultPath = StartUp.DEFAULT_AGSERVER_LOCATION_MAP.get(thisOS)
            if defaultPath is not None:
                if not os.path.exists(defaultPath):
                    defaultPath = None
            if defaultPath is not None and not defaultPath == agJavaServerPath:
                print "Failed to start AllegroGraph server, possibly because "
                if agJavaServerPath is None:
                    print "  no path provided to location of AllegroGraphJavaServer"
                else:
                    print "  incorrect path '" + agJavaServerPath + "'provided to location of AllegroGraphJavaServer"
                print "    Retrying at location '" + defaultPath + "'"
                StartUp.startServer(agConn, defaultPath)
                return
            if agJavaServerPath is not None:
                raise NiceException("Failed to start AllegroGraph server at location " + agJavaServerPath, ex)
            else:
                raise ex
        if not StartUp.quiet:
            print "  started."

    @staticmethod
    def helpStartUpTripleStore(accessOption, host, dbName, dbDirectory, access_options):
        ## TODO: PUT THIS SOMEWHERE NICER:
        AGDirectLink.PERFORMANCE_TRACE = False
        port = 4567
        tripleFile = ""
        agJavaServerPath = None
        exitWait = 0
        startServer = agJavaServerPath is not None
        host = '192.168.1.102'
        print "HOST: ", host
        
        for flag, value in access_options.iteritems():
            if flag == "-p":
                port = int(value)
            elif flag == "-h":
                    host = value
            elif flag == "-d":
                dbDirectory = value
            elif flag == "-n":
                dbName = value
            elif flag == "-t":
                tripleFile = value
            elif flag == "-w":
                exitWait = int(value)
            ## BELOW HERE IS WRONG; THESE FLAGS SHOULD TAKE VALUES:
            elif flag == "-z":
                StartUp.debug = 1
            elif flag == "-zz":
                StartUp.debug = 2
            elif flag == "-x":
                startServer = True
            elif flag == "-q":
                StartUp.quiet = True
            elif flag == "-l":
                agJavaServerPath = value
        if util.is_null_string(host):
            raise NiceException("Name of host (server machine) is required")
        if util.is_null_string(dbName):
            raise NiceException("Database name is required")
        if util.is_null_string(dbDirectory):
            print("Database folder argument (-d) is required.")
            raise NiceException("Can't start up AllegroGraph because no database directory argument has been supplied.")
        print "port=", port, "  dbDirectory=", dbDirectory, "  dbName=", dbName
        agConn = AllegroGraphConnection()
        if agJavaServerPath is not None:
            AllegroGraphConnection.setDefaultCommand(agJavaServerPath)
            startServer = True
        agConn.setPort(port)
        agConn.setHost(host)
        agConn.setDebug(StartUp.debug)
        ## WE MESS WITH DEBUG SETTING HERE:
        print "ENABLING DEBUGGING AT LEVEL 0"
        agConn.setDebug(0)
        ## END MESSING
        if startServer:
            StartUp.startServer(agConn, agJavaServerPath)
        if not StartUp.quiet:
            print "Enabling connection ..."
        try:
            agConn.enable()
        except FakeException, ex:
            traceback.print_stack()
            if False and not startServer:
                StartUp.startServer(agConn, None)
                agConn.enable()
            else:
                raise ex
        if not StartUp.quiet:
            print "Connected to ", agConn
        agStore = None
        try:
            if accessOption == AllegroGraphConnection.RENEW:
                agStore = agConn.renew(dbName, dbDirectory)
            elif accessOption == AllegroGraphConnection.ACCESS:
                agStore = agConn.access(dbName, dbDirectory)
            elif accessOption == AllegroGraphConnection.OPEN:
                agStore = agConn.open(dbName, dbDirectory)
            elif accessOption == AllegroGraphConnection.REPLACE:
                agStore = agConn.replace(dbName, dbDirectory)
            elif accessOption == AllegroGraphConnection.CREATE:
                agStore = agConn.create(dbName, dbDirectory)
        except (FakeException, ), ex:
            traceback.print_stack()
            if not StartUp.quiet:
                print "Caught exception while renewing triple store:\n", ex
            if not StartUp.quiet:
                print "Stopping server ..."
            result = agConn.stopServer(False)
            if not StartUp.quiet:
                if result == 0:
                    print "OK"
                elif result == 1:
                    print "Nothing to terminate."
                else:
                    print "Oh oh."
            return
        tripleCount = agStore.numberOfTriples()
        print "Connected database " , dbName , " contains " , tripleCount , " triples"
        return agStore

    @staticmethod
    def startUpTripleStore(accessOption, host, dbName, dbDirectory, access_options):
        return StartUp.helpStartUpTripleStore(accessOption, host, dbName, dbDirectory, access_options)

    @staticmethod
    def shutDownTripleStore(agStore):
        if agStore is None:
            return
        if not StartUp.quiet:
            print "All done.  Closing triple store."
        try:
            agStore.closeTripleStore()
            if not StartUp.quiet:
                print "Stopping the server."
            agStore.getConnection().stopServer(False)
        except (Exception, ), ex:
            if isinstance(ex, (RuntimeException)):
                raise ex
            else:
                raise NiceException("Failure shutting down AllegroGraph server.", ex)

    @staticmethod
    def main(args):
        hostName = "localhost"
        databaseName = "/tmp/test"
        dbDirectory = "/Users/bmacgregor/Desktop/AGFolder"
        agStore = StartUp.startUpTripleStore(AllegroGraphConnection.RENEW, hostName, databaseName, dbDirectory, args)
        StartUp.shutDownTripleStore(agStore)

if __name__ == '__main__':
    import sys
    StartUp.main(sys.argv)

