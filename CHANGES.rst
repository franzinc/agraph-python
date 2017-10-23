==========================================
AllegroGraph Python client release history
==========================================

Release 100.0.2
===============

Various fixes related to data export
------------------------------------

Specifically the following adjustments have been done:

   - Changed the default RDF export format to N-Quads.
   - Fixed a bug where errors returned during export
     caused an encoding error.
   - Provided a default format (CSV) for tuple queries.  
     
Release 100.0.1
===============

bug24892: Time parsing fixes
----------------------------

The Python client used to fail when trying to retrieve a
datetimeValue() of a literal that contained time zone
information. This has been corrected.

All datetime objects created by the Python API are now timezone-aware.

rfe15005: duplicate suppression control API
-------------------------------------------

It is now possible to set and query the duplicate suppression policy of
a repository from Python, using three new methods of the connection
object:

    - getDuplicateSuppressionPolicy()
    - setDuplicateSuppressionPolicy()
    - disableDuplicateSuppression()

New export methods
------------------

A new mechanism for exporting data has been added. It utilizes a new
``output`` parameter that has been added to the following methods:

   - RepositoryConnection.getStatements()
   - RepositoryConnection.getStatementsById()
   - TupleQuery.evaluate()
   - GraphQuery.evaluate()

Setting the new parameter to a file name or a file-like object
will cause the data that would normally be returned by the call
to be saved to the specified file instead. Serialization format
can be controlled by setting another new parameter,
``output_format``.

Release 100.0.0
===============

New versioning scheme
---------------------

Client versions no longer match the server version. Major version
number has been bumped to 100 to avoid confusion.

bug24819: Circular import
-------------------------

Importing com.franz.openrdf.query.query failed due to a circular
import. Thanks to Maximilien de Bayser for reporting this.

bug24826: removeStatement uses context instead of object
--------------------------------------------------------

The removeStatement method of RepositoryConnection was broken.
Patch by Maximilien de Bayser.

Release 6.2.2.0.4
=================

bug24728: Incorrect conversion between boolean literals and Python values
-------------------------------------------------------------------------

The booleanValue() method of the Literal class used to work
incorrectly.  It would return True for any literal that is not empty,
including the "false"^^xsd:boolean literal.  This has been corrected -
the function will now return expected values for literals of type
xsd:boolean.  Result for other types remains undefined.

Release 6.2.2.0.1
=================

bug24680: to_native_string is broken on Python 2
------------------------------------------------

The Python client sometimes failed while processing values with
non-ascii characters, showing the following error message:

UnicodeEncodeError: 'ascii' codec can't encode characters in position ??: ordinal not in range(128)

This has been corrected.

Release 6.2.2.0.0
=================

Released with AllegroGraph 6.2.2. Change log for this and all previous
Python client releases can be found in AllegroGraph release notes:
https://franz.com/agraph/support/documentation/current/release-notes.html
