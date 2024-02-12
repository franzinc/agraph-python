
Prerequisites
-------------

The tutorial examples can be run on any system which supports Python 3.7+. The
tutorial assumes that AllegroGraph and the Python client have been installed and
configured using the procedure described in the :ref:`install` document.

Setting the environment for the tutorial
----------------------------------------

Before running any of the tutorial examples it is necessary to set a
few variables that describe the location of the AllegroGraph server
and credentials used to access it. By default the Python client looks
for these parameters in environment variables.

The variables that must be set are:

   - ``AGRAPH_HOST``: Specifies the IP address or hostname of the
     agraph server. Defaults to ``'127.0.0.1'``.

   - ``AGRAPH_PORT``: Specifies the port of the agraph server at
     ``AGRAPH_HOST``. The port used by the server is specified in the
     agraph.cfg file. The default is ``10035``.

   - ``AGRAPH_USER``: Specifies the username for authentication. For
     best results, this should name an agraph user with superuser
     privileges. If a non-superuser is specified, that user must have
     **Start Session** privileges to run example 6 and examples
     dependent on that example, and **Evaluate Arbitrary Code**
     privileges to run example 17. There is no default.

   - ``AGRAPH_PASSWORD``: Specifies the password for authentication of
     ``AGRAPH_USER``. There is no default..

The ``agraph.cfg`` file (see the :docs:`Server Installation
<server-installation.html>` document) specifies values like the port
used by the server and the catalogs defined. Refer to that file if
necessary (or ask your system administrator if you do not have access
to the file).

Troubleshooting
~~~~~~~~~~~~~~~

If the tutorial examples throw a connection error in :ref:`example6`,
see the discussion :docs:`Session Port Setup
<server-installation.html#sessionport>` in the :docs:`Server
Installation <server-installation.html>` document.

Terminology
-----------

We need to clarify some terminology before proceeding.

"RDF" is the `Resource Description Framework <http://www.w3.org/RDF/>`__
defined by the `World Wide Web Consortium <http://www.w3.org/>`__ (W3C).
It provides an elegantly simple means for describing multi-faceted
resource objects and for linking them into complex relationship graphs.
AllegroGraph Server creates, searches, and manages such RDF graphs.

A "URI" is a `Uniform Resource Identifier
<https://tools.ietf.org/html/rfc3986>`__. It is label used to uniquely
identify various types of entities in an RDF graph. A typical URI
looks a lot like a web address:
``<http://www.company.com/project/class#number>``. In spite of the
resemblance, a URI is not a web address. It is simply a unique label.

A "triple" is a data statement, a "fact", stored in RDF format. It
states that a resource has an attribute with a value. It consists of
three fields:

-  Subject: The first field contains the URI that uniquely identifies
   the resource that this triple describes.
-  Predicate: The second field contains the URI identifying a property
   of this resource, such as its color or size, or a relationship
   between this resource and another one, such as parentage or
   ownership.
-  Object: The third field is the value of the property. It could be a
   literal value, such as "red", or the URI of a linked resource.

A "quad" is a triple with an added "context" field, which is used to
divide the repository into "subgraphs." This context or subgraph is just
a URI label that appears in the fourth field of related triples.

.. It appears as the fourth field of a TRIPLE, because TRIPLES naturally have FIVE fields. Sigh.

A "resource description" is defined as a collection of triples that all
have the same URI in the subject field. In other words, the triples all
describe attributes of the same thing.

A "statement" is a client-side Python object that describes a triple.

In the context of AllegroGraph Server:

   - A "catalog" is a list of repositories owned by an AllegroGraph
     server.

   - A "repository" is a collection of triples within a catalog.

   - A "context" is a subgraph of the triples in a repository.

   - If contexts are not in use, the triples are stored in the default
     graph.

Creating Users with WebView
---------------------------

Each connection to an AllegroGraph server runs under the credentials of
a registered AllegroGraph user account.

Initial Superuser Account
~~~~~~~~~~~~~~~~~~~~~~~~~

The installation instructions for AllegroGraph advise you to create a
**superuser** called "test". This is the user which is used by the
Python tutorial if the environment variables ``AGRAPH_USER`` and
``AGRAPH_PASSWORD`` are not set.

Users, Permissions, Access Rules, and Roles
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AllegroGraph user accounts may be given any combination of the following
three permissions:

-  Superuser
-  Start Session
-  Evaluate Arbritrary Code

In addition, a user account may be given read, write or read/write
access to individual repositories.

It is easiest to run the Python tutorial as a Superuser. That way you do
not have to worry about permissions.

If you run as a non-Superuser, you need **Start Session** permission
for :ref:`example6` and other examples that require a session.  You
also need read/write access on the appropriate catalogs and
repositories.

You can also define a **role** (such as "librarian") and give the role
a set of permissions and access rules. Then you can assign this shared
role to several users. This lets you manage their permissions and
access by editing the role instead of the individual user accounts.

A **superuser** automatically has all possible permissions and unlimited
access. A superuser can also create, manage and delete other user
accounts. Non-superusers cannot view or edit account settings.

A user with the **Start Sessions** permission can use the AllegroGraph
features that require spawning a dedicated session, such as
transactions and Social Network Analysis (SNA). If you try to use these
features without the appropriate permission, you'll encounter errors.

A user with permission to **Evaluate Arbitrary Code** can run Prolog
Rule Queries. This user can also do anything else that allows
executing Lisp code, such as defining select-style generators, doing
eval-in-server, or loading server-side files.

WebView
~~~~~~~

WebView is AllegroGraph's browser-based graphical user interface for
user and repository management. It allows you to create, query, and
maintain repositories interactively.

To connect to WebView, simply direct your web browser to the
AllegroGraph port of your server. If you have installed AllegroGraph
locally (and used the default port number), use:

::

    http://localhost:10035

You will be asked to log in. Use the superuser credentials described in
the previous section.

The first page of WebView is a summary of your catalogs and
repositories. Select :menuselection:`Admin --> Users` from the
navigation menu at the top of the page.

|img-user-menu|

This exposes the ``Users and Roles`` page. This is the page for
creating and managing user accounts.

To create a new user, click the :guilabel:`[add a user]` link.

|img-user-add|

This exposes a small form where you can enter the username and
password. Click :guilabel:`OK` to save the new account.

|img-user-dialog|

The new user will appear in the list of users. Click the
:guilabel:`[edit]` link to open a control panel for the new user
account:

|img-user-click-edit|

Use the checkboxes to apply permissions to this account (start
session is needed by :ref:`example6`).

|img-user-perms|

It is important that you set up access permissions for the new
user. Use the form to create an access rule by selecting read, write
or read/write access, naming a catalog (or :guilabel:`*` for all), and
naming a repository within that catalog (or :guilabel:`*` for
all). Click the :guilabel:`[add]` link.

|img-user-add-access|

This creates an access rule for your new user. The access rule will
appear in the permissions display:

|img-user-done|

This new user can log in and perform transactions on any repository in
the system.

To repeat, the "test" superuser is all you need to run all of the
tutorial examples. This section is for the day when you want to issue
more modest credentials to some of your users.
