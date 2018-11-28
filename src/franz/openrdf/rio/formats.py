################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

from __future__ import unicode_literals
from builtins import object
from past.builtins import basestring
import os

# A metaclass that converts static dicts inside a Format to instances
from six import add_metaclass


class FormatMeta(type):
    def __init__(cls, name, bases, dct):
        super(FormatMeta, cls).__init__(name, bases, dct)
        for attr, value in dct.items():
            if attr.isupper() and isinstance(value, dict):
                setattr(cls, attr, cls(**value))


@add_metaclass(FormatMeta)
class Format(object):
    """
    Represents the concept of a data serialization format. Data formats are
    identified by a name and can have one or more associated
    MIME types, zero or more associated file extensions and can specify a
    default character encoding.

    This is a base class used by format subclasses for specific types of data,
    such as triples and tuples

    When a subclass of this class is declared all upper case static fields
    that are dictionaries are converted to class instances. Data in the
    dictionary is passed as arguments to the constructor.
    """

    # A global dictionary mapping extensions to formats.
    # Subclasses should override this.
    _ext_map = {}

    @classmethod
    def register(cls, fmt):
        """ Register a format object."""
        for ext in fmt.file_extensions:
            cls._ext_map['.' + ext.lower()] = fmt

    @classmethod
    def format_for_file_name(cls, filename):
        """ 
        Try to guess appropriate RDF format from a file name.
        Return a pair (format, compression) where format is
        an RDF format or None (if no matching format was found)
        and compression is a supported compression method
        (currently either None or "gzip").

        :param filename: File path.
        :type filename: string
        :return: Format (or ``None``) and compression type.
        :rtype: (RDFFormat|None, string|None)
        """
        compression = None
        root, ext = os.path.splitext(filename)
        if ext.lower() == ".gz":
            compression = "gzip"
            _, ext = os.path.splitext(root)
        fmt = cls._ext_map.get(ext.lower())
        return fmt, compression

    @staticmethod
    def mime_type_for_format(output_format):
        """
        Get the preferred MIME type for a data format.
        Raise an error if the format is `None`.
        The format can be a string, in which case it is returned as-is.

        :param output_format: the format to get the MIME type for.
        :type output_format: franz.openrdf.rio.formats.Format|str
        :return: A MIME type.
        :rtype: string
        """
        if output_format is None:
            raise Exception('Unable to determine file format.')
        if isinstance(output_format, basestring):
            return output_format
        return output_format.mime_types[0]

    def __init__(self, name, mime_types=None, charset="UTF-8",
                 file_extensions=None, register=True):
        """
        Initialize a new format object.

        :param name: Human-readable name of the format.
        :param mime_types: A list of MIME types used for this format.
                           The first element of this list will be used
                           as the content-type header during uploads.
        :param charset: Character set used by the format.
        :param file_extensions: List of file extensions for this format.
        :param register: If True file extensions will be added to the map
                         used by :func:`Format.format_for_file_name`.
        """
        self.name = name
        self.mime_types = mime_types
        self.charset = charset
        self.file_extensions = file_extensions
        if register:
            self.register(self)

    # This is used by Sphinx when rendering default values,
    # so it should display something readable.
    def __repr__(self):
        cls = self.__class__
        for name in dir(cls):
            if getattr(cls, name) is self:
                # We could use inspect.mro to find
                # where the thing is really defined,
                # but it's not worth the effort.
                return '{cls}.{name}: {display_name}'.format(
                    cls=cls.__name__, name=name, display_name=self.name)
        return '<Format: {name}>'.format(name=self.name)
