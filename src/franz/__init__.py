# This should keep the .dev suffix in general, until a release is
# made.  The pre-release makefile target takes care of stripping the
# .dev out.  The post-release target increments the version and adds
# .dev back.  Note that PEP-440 specifies that versions are automatically
# padded with zeros - that is, 123.1 is the same as 123.1.0.0....
# (and also 123.1.dev = 123.1.0.0.dev).

__version__ = '100.0.0'
