#
# pytest franz/openrdf/tests/mmr.py
#


import os

import pytest

# from franz.openrdf.connect import AllegroGraphServer
from franz.openrdf.connect import ag_connect
from franz.openrdf.sail import AllegroGraphServer
from franz.openrdf.tests.conftest import min_version

pytestmark = min_version(8, 0)


def test_mmr():
    conn = ag_connect("firstmmr", create=True)

    conn.create_MMR_cluster(
        host="127.1", port=10035, user="test", password="xyzzy", instanceName="first"
    )

    conn.grow_MMR_cluster(
        host="127.1",
        port=10035,
        name="secondmmr",
        user="test",
        password="xyzzy",
        instanceName="second",
    )

    conn.grow_MMR_cluster(
        host="127.1",
        port=10035,
        name="thirdmmr",
        user="test",
        password="xyzzy",
        instanceName="third",
    )

    conn.stop_MMR_instance(instanceName="second")

    conn.grow_MMR_cluster(
        host="127.1",
        port=10035,
        name="fourthmmr",
        user="test",
        password="xyzzy",
        instanceName="fourth",
    )

    conn.start_MMR_instance(instanceName="second")

    conn.remove_MMR_instance(instanceName="third")

    print(conn.get_MMR_status(form="text"))
