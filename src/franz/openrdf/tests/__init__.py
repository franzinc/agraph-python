import os


def assert_env_var_defined(name):
    value = os.environ.get(name)
    if value is None:
        raise Exception("{0} is not defined".format(name))
    print("{name}: {value}".format(name=name, value=value))


def setup():
    assert_env_var_defined("AGRAPH_PORT")
    assert_env_var_defined("AGRAPH_SSL_PORT")
