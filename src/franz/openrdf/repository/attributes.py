"""
Functions and structures related to triple attributes and attribute filters.
"""

from typing import List, Union


class AttributeDefinition:
    """
    Defines basic properties of an attribute, such as its name,
    cardinality and the set of allowed values.

    :ivar name: Unique name of this attribute. Must consist only
        of letters, digits, underscores, dashes and non-ascii characters.
    :ivar allowed_values: Either ``None`` or a list of strings that
        defines the set of legal values for this attribute.
    :ivar ordered: If true then values of this attribute can be compared
        in filters. Values are ordered by their index within
        the ``allowed_values`` list (which is required for ordered attributes).
    :ivar minimum_number: Defines the minimum number of values for this
        attribute that must be associated with each triple.
    :ivar maximum_number: Defines the maximum number of values for this
        attribute that can be associated with each triple.
    """

    def __init__(
        self,
        name,
        allowed_values=None,
        ordered=False,
        minimum_number=None,
        maximum_number=None,
    ):
        self.name = name  # type: str
        self.allowed_values = allowed_values  # type: Union[List[str], None]
        self.ordered = ordered  # type: bool
        self.minimum_number = minimum_number  # type: Union[int, None]
        self.maximum_number = maximum_number  # type: Union[int, None]


class ContextAttributeType(type):
    """
    Metaclass for 'context' classes that convert ``Class.something``
    and ``Class['something']`` to ``Class('something')``.
    """

    def __getattr__(cls, name):
        return cls(name)

    def __getitem__(cls, index):
        return cls(index)


class ContextAttribute(metaclass=ContextAttributeType):
    """
    Superclass for contextual triple attributes used in filters.
    """

    context = ""  # Subclasses must override this

    def __init__(self, name):
        self.name = name

    def __str__(self):
        return "%s.%s" % (self.context, self.name)

    def __lt__(self, other):
        return Lt(self, other)

    def __le__(self, other):
        return Le(self, other)

    def __eq__(self, other):
        return Equal(self, other)

    def __gt__(self, other):
        return Gt(self, other)

    def __ge__(self, other):
        return Ge(self, other)

    def __lshift__(self, other):
        return Subset(self, other)

    def __rshift__(self, other):
        return Superset(self, other)

    def __rlshift__(self, other):
        return Subset(other, self)

    def __rrshift__(self, other):
        return Superset(other, self)


class UserAttribute(ContextAttribute):
    """
    User attribute reference to be used in filters.

    A user attribute name 'foo' can be referenced in three ways:

       - ``UserAttribute('foo')``
       - ``UserAttribute.foo``
       - ``UserAttribute['foo']``

    Filters involving objects of this class can be constructed
    using the following operators:

       - ``<``, ``<=``, ``==``, ``>=`` ,``>`` - note that ``==``
         is translated as ``equals``, not ``attribute=``.
       - ``<<`` and ``>>`` - meaning 'subset' and 'superset'.
    """

    context = "user"


class TripleAttribute(ContextAttribute):
    """
    Triple attribute reference to be used in filters.

    A triple attribute name 'foo' can be referenced in three ways:

       - ``TripleAttribute('foo')``
       - ``TripleAttribute.foo``
       - ``TripleAttribute['foo']``

    Filters involving objects of this class can be constructed
    using the following operators:

       - ``<``, ``<=``, ``==``, ``>=`` ,``>`` - note that ``==``
         is translated as ``equals``, not ``attribute=``.
       - ``<<`` and ``>>`` - meaning 'subset' and 'superset'.
    """

    context = "triple"


def quote_and_escape_value(value):
    # type: (str) -> str
    """
    Quote a string so it can be read from Lisp.
    """
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def attribute_set_to_expr(attribute_set):
    """
    Convert an attribute set (to be used in a filter) to string.
    """
    if isinstance(attribute_set, (str, bytes)):
        return quote_and_escape_value(attribute_set)
    elif isinstance(attribute_set, ContextAttribute):
        return "%s.%s" % (attribute_set.context, attribute_set.name)
    else:  # Must be a list of values
        values = " ".join(attribute_set_to_expr(v) for v in attribute_set)
        return "(" + values + ")"


def attribute_filter_to_expr(attribute_filter):
    # type: (AttributeFilter|str) -> str
    """
    Convert an attribute filter to string.
    """
    if isinstance(attribute_filter, AttributeFilter):
        return attribute_filter.to_expr()
    return attribute_filter


class AttributeFilter:
    """
    Superclass of objects that can be used as static attribute filters.

    Note that raw strings are also accepted in this role.

    Objects of this class can be combined using bitwise logical operators.
    """

    def to_expr(self):
        """
        Convert this object to an S-expression and return it as a string.
        :return: S-expression representing this object.
        """
        # type: () -> str
        raise NotImplementedError()

    def __and__(self, other):
        # type: (AttributeFilter|str) -> AttributeFilter
        args = []
        if isinstance(self, And):
            args += self.args
        else:
            args.append(self)
        if isinstance(other, And):
            args += other.args
        else:
            args.append(other)
        return And(*args)

    def __or__(self, other):
        # type: (AttributeFilter|str) -> AttributeFilter
        args = []
        if isinstance(self, Or):
            args += self.args
        else:
            args.append(self)
        if isinstance(other, Or):
            args += other.args
        else:
            args.append(other)
        return Or(*args)

    def __ror__(self, other):
        # type: (str) -> AttributeFilter
        return Or(other, self)

    def __rand__(self, other):
        # type: (str) -> AttributeFilter
        return And(other, self)

    def __invert__(self):
        # type: () -> AttributeFilter
        if isinstance(self, Not):
            return self.args[0]
        return Not(self)

    def __str__(self):
        return self.to_expr()


class SetOp(AttributeFilter):
    """
    An attribute filter created by applying an operator
    to one or more attribute sets.
    """

    op = ""  # Subclasses must override this

    def __init__(self, *args):
        self.args = args

    def to_expr(self):
        sets = " ".join(attribute_set_to_expr(v) for v in self.args)
        return "(%s%s%s)" % (self.op, " " if sets else "", sets)


class FilterOp(AttributeFilter):
    """
    An attribute filter created by applying an operator
    to other filters. Note that the arguments can be either
    AttributeFilter instances or strings.
    """

    op = ""  # Subclasses must override this

    def __init__(self, *args):
        # type: (*AttributeFilter|str) -> None
        self.args = args

    def to_expr(self):
        filters = " ".join(attribute_filter_to_expr(v) for v in self.args)
        return "(%s%s%s)" % (self.op, " " if filters else "", filters)


class And(FilterOp):
    """
    A conjunction of attribute filters.
    """

    op = "and"


class Or(FilterOp):
    """
    A disjunction of attribute filters.
    """

    op = "or"


class Not(FilterOp):
    """
    Inverse attribute filter (matches whatever the argument does not match).
    """

    op = "not"


class Empty(SetOp):
    """
    A filter that matches if the argument is an empty attribute set.
    """

    op = "empty"


class Overlap(SetOp):
    """
    A filter that matches if attribute sets given as arguments
    have a common value.
    """

    op = "overlap"


class Subset(SetOp):
    """
    A filter that matches if the first argument is a subset
    of the second argument (both arguments must be attribute sets).
    """

    op = "subset"


class Superset(SetOp):
    """
    A filter that matches if the first argument is a supersset
    of the second argument (both arguments must be attribute sets).
    """

    op = "superset"


class Equal(SetOp):
    """
    A filter that matches if its arguments are identical attribute sets.
    """

    op = "equal"


class OrderedFilter(SetOp):
    """
    A filter for comparing ordered attributes. Both arguments must be either
    empty or singleton sets representing values of an ordered attribute.
    """

    pass


class Lt(OrderedFilter):
    """
    An ordered filter that matches if the first argument is less
    than the second.
    """

    op = "attribute-set<"


class Le(OrderedFilter):
    """
    An ordered filter that matches if the first argument is less than
    or equal to the second.
    """

    op = "attribute-set<="


class Eq(OrderedFilter):
    """
    An ordered filter that matches if the first argument is equal to the second.
    """

    op = "attribute-set="


class Ge(OrderedFilter):
    """
    An ordered filter that matches if the first argument is greater than
    or equal to the second.
    """

    op = "attribute-set>="


class Gt(OrderedFilter):
    """
    An ordered filter that matches if the first argument is greater than
    the second.
    """

    op = "attribute-set>"
