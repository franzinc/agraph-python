################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

"""
Helper function for opening connections.
"""
import os

from franz.openrdf.repository import Repository
from franz.openrdf.repository.repositoryconnection import RepositoryConnection
from franz.openrdf.sail import AllegroGraphServer


# Note that the default values come from AllegroGraphServer.__init__
def ag_connect(
    repo,
    catalog=None,
    create=True,
    fail_if_exists=False,
    clear=False,
    session=False,
    autocommit=False,
    lifetime=None,
    loadinitfile=False,
    host=None,
    port=None,
    protocol=None,
    user=None,
    password=None,
    cainfo=None,
    sslcert=None,
    verifyhost=None,
    verifypeer=None,
    indices=None,
    proxy=os.environ.get("AGRAPH_PROXY"),
    vector_store=False,
    embedder=None,
    api_key=None,
    model=None,
):
    """
    Create a connection to an AllegroGraph repository.

    When closed the connection will take care of releasing all intermediate resources
    that were created in order to open it.

    :param repo: Repository name.
    :type repo: string
    :param catalog: Catalog name (optional, root catalog is the default).
    :type catalog: string
    :param create: if `True` (default) create the repository if it does not exist.
    :type create: bool
    :param fail_if_exists: if `True` and the repository exists raise an exception.
                           This applies only if `create` is `True`.
                           The default value is `False`.
    :type fail_if_exists: bool
    :param clear: if `True` delete all data after creating the connection.
                  The default is `False`.
    :type clear: bool
    :param session: If ``True`` start a session after creating the connection.
                    The default is ``False``.
    :type session: bool
    :param autocommit: When opening a session: if ``True``, commits are done on each
                       request, otherwise you will need to call :meth:`.commit` or
                       :meth:`.rollback` as appropriate for your application.
                       The default value is ``False``.
    :type autocommit: bool
    :param lifetime: Time (in seconds) before the session expires when idle.
                     Note that the client maintains a thread that ping the
                     session before this happens.
                     Ignored if not starting a session.
    :type lifetime: int
    :param loadinitfile: if ``True`` then the current initfile will be loaded
                         for you when the session starts. The default is ``False``.
                         Ignored if not starting a session.
    :type loadinitfile: bool
    :param host: AllegroGraph server host (default: ``'127.0.0.1'`` or the value
                 of the AGRAPH_HOST environment variable if that is defined.`
                 Can also be used to supply protocol and port number
                 (e.g. ``https://localhost:10036``).
    :type host: string
    :param protocol: Either ``"http"`` or ``"https"``.
                     The default is ``"http"``.
                     Overrides the protocol specified in ``host``.
    :type protocol: string
    :param port: AllegroGraph server port (default: `10035` for http and `10036`
                 for https, or the AGRAPH_PORT environment variable if that is defined).
                 Overrides the port number provided in ``host``.
    :type port: int
    :param user: Username for authentication (default: value of the ``AGRAPH_USER``
                 environment variable).
    :type user: string
    :param password:  Password for authentication (default: value of the ``AGRAPH_PASSWORD``
                      environment variable).
    :type password: string
    :param cainfo: Path to file or directory with CA certificates.
    :type cainfo: string
    :param sslcert: Path to a client certificate to use for
                    authentication instead of username and password.
    :type sslcert: string
    :param verifyhost: See https://curl.haxx.se/libcurl/c/CURLOPT_SSL_VERIFYHOST.html
    :type verifyhost: int
    :param verifypeer: See https://curl.haxx.se/libcurl/c/CURLOPT_SSL_VERIFYPEER.html
    :type verifypeer: int
    :param indices: List of indices to create if creating a new repository.
    :type indices: list[string]
    :param proxy: Proxy specification string. The format is SCHEME://HOST:PORT.
                  Supported schemes are 'http', 'socks4' and 'socks5'.
                  Note that for SOCKS proxies DNS requests are performed by the
                  proxy server.
                  The default value is taken from the AGRAPH_PROXY environment
                  variable.
    :type proxy: string
    :param vector_store: True to create a vector store
    :type vector_store:bool
    :param embedder: name of service to create embeddings
    :type embedder: string
    :param api_key: embedding service api key if required
    :type api_key: string
    :param model: name of embedding model (default varies by embedder)
    :type model: string

    :return: A :class:`.RepositoryConnection` object.
    :rtype: franz.openrdf.repositoryconnection.RepositoryConnection
    """
    server = AllegroGraphServer(
        host=host,
        port=port,
        protocol=protocol,
        user=user,
        password=password,
        sslcert=sslcert,
        cainfo=cainfo,
        verifyhost=verifyhost,
        verifypeer=verifypeer,
        proxy=proxy,
    )
    cat_handle = server.openCatalog(catalog)
    repo_exists = repo in cat_handle.listRepositories()
    if not repo_exists:
        if create:
            repo_handle = cat_handle.createRepository(
                repo, indices, vector_store, embedder, api_key, model
            )
        else:
            raise Exception("Store %s does not exist." % repo)
    else:
        if fail_if_exists and create:
            raise Exception("Store %s already exists." % repo)
        mode = Repository.RENEW if clear else Repository.OPEN
        repo_handle = cat_handle.getRepository(repo, mode)
    conn = RepositoryConnection(repo_handle, close_repo=True)
    if session:
        # conn.close will close it if necessary
        conn.openSession(
            autocommit=autocommit, lifetime=lifetime, loadinitfile=loadinitfile
        )
    return conn
