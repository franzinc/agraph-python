# This should keep the .dev suffix in general, until a release is
# made.  The pre-release makefile target takes care of stripping the
# .dev out.  The post-release target increments the version and adds
# .dev back.  The first four numbers are the AG version. The fifth
# number is incremented with each release. Note that PEP-440 specifies
# that versions are automatically padded with zeros - that is, 6.2.1 =
# 6.2.1.0.0 (and also 6.2.0.dev = 6.2.0.0.dev).
__version__ = '6.2.1.0.2'
