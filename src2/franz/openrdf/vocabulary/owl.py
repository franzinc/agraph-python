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


from franz.openrdf.exceptions import *

class OWL:
    NAMESPACE = "http://www.w3.org/2002/07/owl#"
#    TYPE = None
#    PROPERTY = None
#    XMLLITERAL = None
     
    ## map of uri strings to URI objects:
    name2URI = {}
    
    @staticmethod
    def initialize(factory):
        """
        Initialize the constant using factory 'factory'
        """
        OWL.CLASS = factory.createURI(namespace=OWL.NAMESPACE, localname="Class")
        OWL.INDIVIDUAL = factory.createURI(namespace=OWL.NAMESPACE, localname="Individual")
        OWL.EQUIVALENTCLASS = factory.createURI(namespace=OWL.NAMESPACE, localname="equivalentClass")
        OWL.EQUIVALENTPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="equivalentProperty")
        OWL.SAMEAS = factory.createURI(namespace=OWL.NAMESPACE, localname="sameAs")
        OWL.DIFFERENTFROM = factory.createURI(namespace=OWL.NAMESPACE, localname="differentFrom")
        OWL.ALLDIFFERENT = factory.createURI(namespace=OWL.NAMESPACE, localname="AllDifferent")
        OWL.DISTINCTMEMBERS = factory.createURI(namespace=OWL.NAMESPACE, localname="distinctMembers")
        OWL.OBJECTPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="ObjectProperty")
        OWL.DATATYPEPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="DatatypeProperty")
        OWL.INVERSEOF = factory.createURI(namespace=OWL.NAMESPACE, localname="inverseOf")
        OWL.TRANSITIVEPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="TransitiveProperty")
        OWL.SYMMETRICPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="SymmetricProperty")
        OWL.FUNCTIONALPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="FunctionalProperty")
        OWL.INVERSEFUNCTIONALPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="InverseFunctionalProperty")
        OWL.RESTRICTION = factory.createURI(namespace=OWL.NAMESPACE, localname="Restriction")
        OWL.ONPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="onProperty")
        OWL.ALLVALUESFROM = factory.createURI(namespace=OWL.NAMESPACE, localname="allValuesFrom")
        OWL.SOMEVALUESFROM = factory.createURI(namespace=OWL.NAMESPACE, localname="someValuesFrom")
        OWL.MINCARDINALITY = factory.createURI(namespace=OWL.NAMESPACE, localname="minCardinality")
        OWL.MAXCARDINALITY = factory.createURI(namespace=OWL.NAMESPACE, localname="maxCardinality")
        OWL.CARDINALITY = factory.createURI(namespace=OWL.NAMESPACE, localname="cardinality")
        OWL.ONTOLOGY = factory.createURI(namespace=OWL.NAMESPACE, localname="Ontology")
        OWL.IMPORTS = factory.createURI(namespace=OWL.NAMESPACE, localname="imports")
        OWL.INTERSECTIONOF = factory.createURI(namespace=OWL.NAMESPACE, localname="intersectionOf")
        OWL.VERSIONINFO = factory.createURI(namespace=OWL.NAMESPACE, localname="versionInfo")
        OWL.PRIORVERSION = factory.createURI(namespace=OWL.NAMESPACE, localname="priorVersion")
        OWL.BACKWARDCOMPATIBLEWITH = factory.createURI(namespace=OWL.NAMESPACE, localname="backwardCompatibleWith")
        OWL.INCOMPATIBLEWITH = factory.createURI(namespace=OWL.NAMESPACE, localname="incompatibleWith")
        OWL.DEPRECATEDCLASS = factory.createURI(namespace=OWL.NAMESPACE, localname="DeprecatedClass")
        OWL.DEPRECATEDPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="DeprecatedProperty")
        OWL.ANNOTATIONPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="AnnotationProperty")
        OWL.ONTOLOGYPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="OntologyProperty")
        ## OWL DL and OWL Full
        OWL.ONEOF = factory.createURI(namespace=OWL.NAMESPACE, localname="oneOf")
        OWL.HASVALUE = factory.createURI(namespace=OWL.NAMESPACE, localname="hasValue")
        OWL.DISJOINTWITH = factory.createURI(namespace=OWL.NAMESPACE, localname="disjointWith")
        OWL.UNIONOF = factory.createURI(namespace=OWL.NAMESPACE, localname="unionOf")
        OWL.COMPLEMENTOF = factory.createURI(namespace=OWL.NAMESPACE, localname="complementOf")
        
        ## (re)build 'name2URI' dictionary
        OWL.name2URIMap = {}
        for uri in [OWL.CLASS, OWL.INDIVIDUAL, OWL.EQUIVALENTCLASS, OWL.EQUIVALENTPROPERTY, 
                OWL.SAMEAS, OWL.DIFFERENTFROM, OWL.ALLDIFFERENT, OWL.DISTINCTMEMBERS, 
                OWL.OBJECTPROPERTY, OWL.DATATYPEPROPERTY, OWL.INVERSEOF, OWL.TRANSITIVEPROPERTY, 
                OWL.SYMMETRICPROPERTY, OWL.FUNCTIONALPROPERTY, OWL.INVERSEFUNCTIONALPROPERTY, 
                OWL.RESTRICTION, OWL.ONPROPERTY, OWL.ALLVALUESFROM, OWL.SOMEVALUESFROM, 
                OWL.MINCARDINALITY, OWL.MAXCARDINALITY, OWL.CARDINALITY, OWL.ONTOLOGY, 
                OWL.IMPORTS, OWL.INTERSECTIONOF, OWL.VERSIONINFO, OWL.PRIORVERSION, 
                OWL.BACKWARDCOMPATIBLEWITH, OWL.INCOMPATIBLEWITH, OWL.DEPRECATEDCLASS, 
                OWL.DEPRECATEDPROPERTY, OWL.ANNOTATIONPROPERTY, OWL.ONTOLOGYPROPERTY, 
                OWL.ONEOF, OWL.HASVALUE, OWL.DISJOINTWITH, OWL.UNIONOF, OWL.COMPLEMENTOF, ]:
            OWL.name2URIMap[str(uri)] = uri

            
    @staticmethod
    def reinitialize(factory, store=None):
        """
        Initialize the values in the factory, or
        reinitialize the values in factory with more efficient
        resources and literals (one's that know what store they
        belong to).
        """
        OWL.CLASS = factory.createURI(namespace=OWL.NAMESPACE, localname="Class")
        OWL.INDIVIDUAL = factory.createURI(namespace=OWL.NAMESPACE, localname="Individual")
        OWL.EQUIVALENTCLASS = factory.createURI(namespace=OWL.NAMESPACE, localname="equivalentClass")
        OWL.EQUIVALENTPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="equivalentProperty")
        OWL.SAMEAS = factory.createURI(namespace=OWL.NAMESPACE, localname="sameAs")
        OWL.DIFFERENTFROM = factory.createURI(namespace=OWL.NAMESPACE, localname="differentFrom")
        OWL.ALLDIFFERENT = factory.createURI(namespace=OWL.NAMESPACE, localname="AllDifferent")
        OWL.DISTINCTMEMBERS = factory.createURI(namespace=OWL.NAMESPACE, localname="distinctMembers")
        OWL.OBJECTPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="ObjectProperty")
        OWL.DATATYPEPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="DatatypeProperty")
        OWL.INVERSEOF = factory.createURI(namespace=OWL.NAMESPACE, localname="inverseOf")
        OWL.TRANSITIVEPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="TransitiveProperty")
        OWL.SYMMETRICPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="SymmetricProperty")
        OWL.FUNCTIONALPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="FunctionalProperty")
        OWL.INVERSEFUNCTIONALPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="InverseFunctionalProperty")
        OWL.RESTRICTION = factory.createURI(namespace=OWL.NAMESPACE, localname="Restriction")
        OWL.ONPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="onProperty")
        OWL.ALLVALUESFROM = factory.createURI(namespace=OWL.NAMESPACE, localname="allValuesFrom")
        OWL.SOMEVALUESFROM = factory.createURI(namespace=OWL.NAMESPACE, localname="someValuesFrom")
        OWL.MINCARDINALITY = factory.createURI(namespace=OWL.NAMESPACE, localname="minCardinality")
        OWL.MAXCARDINALITY = factory.createURI(namespace=OWL.NAMESPACE, localname="maxCardinality")
        OWL.CARDINALITY = factory.createURI(namespace=OWL.NAMESPACE, localname="cardinality")
        OWL.ONTOLOGY = factory.createURI(namespace=OWL.NAMESPACE, localname="Ontology")
        OWL.IMPORTS = factory.createURI(namespace=OWL.NAMESPACE, localname="imports")
        OWL.INTERSECTIONOF = factory.createURI(namespace=OWL.NAMESPACE, localname="intersectionOf")
        OWL.VERSIONINFO = factory.createURI(namespace=OWL.NAMESPACE, localname="versionInfo")
        OWL.PRIORVERSION = factory.createURI(namespace=OWL.NAMESPACE, localname="priorVersion")
        OWL.BACKWARDCOMPATIBLEWITH = factory.createURI(namespace=OWL.NAMESPACE, localname="backwardCompatibleWith")
        OWL.INCOMPATIBLEWITH = factory.createURI(namespace=OWL.NAMESPACE, localname="incompatibleWith")
        OWL.DEPRECATEDCLASS = factory.createURI(namespace=OWL.NAMESPACE, localname="DeprecatedClass")
        OWL.DEPRECATEDPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="DeprecatedProperty")
        OWL.ANNOTATIONPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="AnnotationProperty")
        OWL.ONTOLOGYPROPERTY = factory.createURI(namespace=OWL.NAMESPACE, localname="OntologyProperty")
        ## OWL DL and OWL Full
        OWL.ONEOF = factory.createURI(namespace=OWL.NAMESPACE, localname="oneOf")
        OWL.HASVALUE = factory.createURI(namespace=OWL.NAMESPACE, localname="hasValue")
        OWL.DISJOINTWITH = factory.createURI(namespace=OWL.NAMESPACE, localname="disjointWith")
        OWL.UNIONOF = factory.createURI(namespace=OWL.NAMESPACE, localname="unionOf")
        OWL.COMPLEMENTOF = factory.createURI(namespace=OWL.NAMESPACE, localname="complementOf")

        ## (re)build 'name2URI' dictionary
        OWL.name2URIMap = {}
        for uri in [OWL.CLASS, OWL.INDIVIDUAL, OWL.EQUIVALENTCLASS, OWL.EQUIVALENTPROPERTY, 
                OWL.SAMEAS, OWL.DIFFERENTFROM, OWL.ALLDIFFERENT, OWL.DISTINCTMEMBERS, 
                OWL.OBJECTPROPERTY, OWL.DATATYPEPROPERTY, OWL.INVERSEOF, OWL.TRANSITIVEPROPERTY, 
                OWL.SYMMETRICPROPERTY, OWL.FUNCTIONALPROPERTY, OWL.INVERSEFUNCTIONALPROPERTY, 
                OWL.RESTRICTION, OWL.ONPROPERTY, OWL.ALLVALUESFROM, OWL.SOMEVALUESFROM, 
                OWL.MINCARDINALITY, OWL.MAXCARDINALITY, OWL.CARDINALITY, OWL.ONTOLOGY, 
                OWL.IMPORTS, OWL.INTERSECTIONOF, OWL.VERSIONINFO, OWL.PRIORVERSION, 
                OWL.BACKWARDCOMPATIBLEWITH, OWL.INCOMPATIBLEWITH, OWL.DEPRECATEDCLASS, 
                OWL.DEPRECATEDPROPERTY, OWL.ANNOTATIONPROPERTY, OWL.ONTOLOGYPROPERTY, 
                OWL.ONEOF, OWL.HASVALUE, OWL.DISJOINTWITH, OWL.UNIONOF, OWL.COMPLEMENTOF, ]:
            OWL.name2URIMap[str(uri)] = uri
        
    
    @staticmethod
    def name2URI (name, exception_if_failure=True):
        """
        Given a URI string, return the OpenRDF URI object.
        """
        matchingURI = OWL.name2URIMap.get(name)
        if matchingURI: return matchingURI
        elif exception_if_failure:
            raise IllegalArgumentException("Passed a non-OWL URI to 'XMLSchema.name2URI.")
        else: return None
    


