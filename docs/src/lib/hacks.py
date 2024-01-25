# This module is a Sphinx extension that contains custom directives and other
# Sphinx-related modifications that are too extensive to be written directly
# in conf.py
import doctest
import os

import sphinx.ext.doctest
from docutils import nodes
from docutils.parsers.rst import directives
from lexer import CustomLexer
from sphinx.ext.autosummary import (
    Autosummary,
    get_documenter,
    get_import_prefixes_from_env,
    import_by_name,
)
from sphinx.ext.doctest import (
    DocTestBuilder,
    SphinxDocTestRunner,
    TestcodeDirective,
    TestoutputDirective,
    TestsetupDirective,
)
from sphinx.highlighting import lexers
from sphinx.util import parselinenos
from sphinx.util.inspect import safe_getattr


# Stolen from: http://stackoverflow.com/questions/20569011/blorp
class ExtAutoSummary(Autosummary):
    """
    Extended version of the autosummary directive.

    It allows the user to easily add all methods/attributes/functions
    of a class or module to the summary.

    To do this, prefix the class/module name in the list of things to
    display with one of: 'methods:', 'attributes:' or 'funcs:'.

    Use 'methods_without_init:' if you do not want the initializer to
    be documented.
    """

    @staticmethod
    def get_members(app, obj, typ, include_public=None):
        if not include_public:
            include_public = []
        items = []
        for name in vars(obj):
            try:
                documenter = get_documenter(app, safe_getattr(obj, name), obj)
            except AttributeError:
                continue
            if documenter.objtype == typ:
                items.append(name)
        items.sort()
        public = [x for x in items if x in include_public or not x.startswith("_")]
        return public, items

    def get_items(self, names):
        env = self.state.document.settings.env
        prefixes = get_import_prefixes_from_env(env)

        ext_names = []

        def process_obj(name, typ, include_public=None):
            obj_name = name[name.index(":") + 1 :]
            full_name, obj, _, _ = import_by_name(obj_name, prefixes=prefixes)
            members, _ = self.get_members(env.app, obj, typ, include_public)
            ext_names.extend(["~%s.%s" % (full_name, member) for member in members])

        for elt in names:
            if elt.startswith("methods:"):
                process_obj(elt, "method", ("__init__",))
            elif elt.startswith("methods_without_init:"):
                process_obj(elt, "method")
            elif elt.startswith("attributes:"):
                process_obj(elt, "attribute")
            elif elt.startswith("funcs:"):
                process_obj(elt, "function")
            else:
                ext_names.append(elt)
        return super(ExtAutoSummary, self).get_items(ext_names)


class VisibleTestSetup(TestsetupDirective):
    """
    A hacked 'testsetup' directive that does not hide the setup code.
    """

    def run(self):
        # Ugly hack required to make the super call work.
        self.name = "testsetup"
        result = super(VisibleTestSetup, self).run()
        text = result[0].rawsource
        groups = result[0].get("groups")
        return [
            nodes.literal_block(text, text, testnodetype="testsetup", groups=groups)
        ]


def skip_member(app, what, name, obj, skip, options):
    """
    Determine of a name should be skipped when listing methods.

    We override this to allow __init__ to be listed.
    """
    del app, what, obj, options
    if name == "__init__":
        return False
    return skip


def sort_lines(text):
    return "\n".join(x for x in sorted(text.split("\n")) if x)


SORT = doctest.register_optionflag("SORT")
doctest.COMPARISON_FLAGS |= SORT


class SortingOutputChecker(doctest.OutputChecker):
    """
    A doctest output checker that sorts lines before comparison.
    """

    def check_output(self, want, got, optionflags):
        # old style classes?! In 2017?!?!
        # return super(SortingOutputChecker, self).check_output(
        #    sort_lines(want), sort_lines(got), optionflags)
        return doctest.OutputChecker.check_output(
            self, sort_lines(want), sort_lines(got), optionflags
        )


def merge(*args, **kwargs):
    result = {}
    for arg in args:
        result.update(arg)
    for key, value in kwargs.items():
        result[key] = value
    return result


def conditionalize(directive):
    """
    Add :if: option to a directive.
    """

    class NewDirective(directive):
        option_spec = merge(
            directive.option_spec,
            {
                "if": directives.unchanged_required,
            },
        )

        def run(self):
            env = self.state.document.settings.env
            app = env.app
            if self.options.get("if"):
                # Logic copied from ifconfig
                ns = dict((confval.name, confval.value) for confval in app.config)
                ns.update(app.config.__dict__.copy())
                ns["builder"] = app.builder.name
                if not eval(self.options.get("if"), ns):
                    return []
            return directive.run(self)

    return NewDirective


class CustomTestcodeDirective(TestcodeDirective):
    """
    Testcode hacked to support 'emphasize-lines'.
    """

    option_spec = merge(
        TestcodeDirective.option_spec,
        {
            "emphasize-lines": directives.unchanged_required,
        },
    )

    def get_hl(self):
        linespec = self.options.get("emphasize-lines")
        if linespec:
            nlines = len(self.content)
            hl_lines = parselinenos(linespec, nlines)
            return [x + 1 for x in hl_lines if x < nlines]
        return None

    def run(self):
        # #$%$^&!!!!!!!!
        self.name = "testcode"
        (node,) = TestcodeDirective.run(self)
        hl_lines = self.get_hl()
        if hl_lines:
            node["highlight_args"] = {"hl_lines": hl_lines}
        return [node]


class CustomTestoutputDirective(TestoutputDirective):
    """
    Nothing custom here, but this overrides self.name in run(),
    so subclasses might actually work.
    """

    def run(self):
        # #$%$^&!!!!!!!!
        self.name = "testoutput"
        (node,) = TestoutputDirective.run(self)
        node["testnodetype"] = "testoutput"
        return [node]


# Check if a test group should be skipped
# We skip groups starting with '-', but if the EXAMPLE env
# var is set we will run ONLY the specified group.
def should_skip(group_name):
    example = os.environ.get("EXAMPLE")
    if example:
        return group_name != example
    return group_name.startswith("-")


def hack_sphinx_doctest():
    """
    Monkey-patch doctest to support more directives.
    Specifically the 'sort' directive.
    And also to ignore test groups with names starting with '-'.
    """

    class HackedRunner(SphinxDocTestRunner):
        def run(self, test, *args, **kwargs):
            old_checker = self._checker
            if test.examples and test.examples[0].options.get(SORT):
                self._checker = SortingOutputChecker()
            try:
                # old style classes?! In 2017?!?!
                return SphinxDocTestRunner.run(self, test, *args, **kwargs)
            # return super(HackedRunner, self).run(test, *args, **kwargs)
            finally:
                self._checker = old_checker

    old_test_group = DocTestBuilder.test_group

    def test_group(self, group):
        if not should_skip(group.name):
            return old_test_group(self, group)

    sphinx.ext.doctest.SphinxDocTestRunner = HackedRunner
    sphinx.ext.doctest.DocTestBuilder.test_group = test_group


def setup(app):
    """
    Register our extensions with Sphinx.

    Called by Sphinx.
    """
    # Evil hacks
    hack_sphinx_doctest()
    # Register the extautosummary extension
    app.setup_extension("sphinx.ext.autosummary")
    app.add_directive("extautosummary", ExtAutoSummary)
    app.add_directive("visibletestsetup", VisibleTestSetup)
    app.add_directive("exttestcode", conditionalize(CustomTestcodeDirective))
    app.add_directive("condtestoutput", conditionalize(CustomTestoutputDirective))
    # Make sure __init__ is included when documenting classes.
    app.connect("autodoc-skip-member", skip_member)
    lexers["python_rdf"] = CustomLexer(startinline=True)
    return {"parallel_read_safe": True}
