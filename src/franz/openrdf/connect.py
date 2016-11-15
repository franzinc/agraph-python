###############################################################################
# Copyright (c) 2006-2016 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

"""
Helper function for opening connections.
"""

from franz.openrdf.repository import Repository
from franz.openrdf.sail import AllegroGraphServer


def ag_connect(repo, catalog=None, create=True, fail_if_exists=False, clear=False,
               host='localhost', port=10035, user=None, password=None, cainfo=None, sslcert=None,
               verifyhost=None, verifypeer=None, indices=None):
    """
    Create a connection to an AllegroGraph repository.

    :param repo: Repository name.
    :param catalog: Catalog name (optional, root catalog is the default).
    :param create: if `True` (default) create the repository if it does not exist.
    :param fail_if_exists: if `True` and the repository exists raise an exception.
                           This applies only if `create` is `True`.
                           The default value is `False`.
    :param clear: if `True` delete all data after creating the connection.
                  The default is `False`.
    :param host: AllegroGraph server host (default: `"localhost"`).
    :param port: AllegroGraph server port (default: `10035`).
    :param user: Username for authentication.
    :param password:  Password for authentication.
    :param cainfo: Path to file or directory with CA certificates.
    :param sslcert: Path to a client certificate to use for
                    authentication instead of username and password.
    :param verifyhost: See https://curl.haxx.se/libcurl/c/CURLOPT_SSL_VERIFYHOST.html
    :param verifypeer: See https://curl.haxx.se/libcurl/c/CURLOPT_SSL_VERIFYPEER.html
    :param indices: List of indices to create if creating a new repository.
    :return: A RepositoryConnection object.
    :rtype: franz.openrdf.repositoryconnection.RepositoryConnection
    """
    server = AllegroGraphServer(host, port, user, password, cainfo=cainfo, sslcert=sslcert,
                                verifyhost=verifyhost, verifypeer=verifypeer)
    cat_handle = server.openCatalog(catalog)
    repo_exists = repo in cat_handle.listRepositories()
    if not repo_exists:
        if create:
            repo_handle = cat_handle.createRepository(repo, indices)
        else:
            raise Exception('Store %s does not exist.' % repo)
    else:
        if fail_if_exists and create:
            raise Exception('Store %s already exists.' % repo)
        mode = Repository.RENEW if clear else Repository.OPEN
        repo_handle = cat_handle.getRepository(repo, mode)
    return repo_handle.getConnection()
