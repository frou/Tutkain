from sublime import Region

from Tutkain.src import forms

from .util import ViewTestCase


class TestForms(ViewTestCase):
    def test_find_next(self):
        self.set_view_content("a")
        self.assertEquals(forms.find_next(self.view, 0), Region(0, 1))
        self.set_view_content("(a)")
        self.assertEquals(forms.find_next(self.view, 0), Region(0, 3))
        self.set_view_content("(a)")
        self.assertEquals(forms.find_next(self.view, 3), None)
        self.set_view_content(":foo")
        self.assertEquals(forms.find_next(self.view, 0), Region(0, 4))
        self.set_view_content(":foo/bar")
        self.assertEquals(forms.find_next(self.view, 0), Region(0, 8))
        self.set_view_content("(identity 1/2)")
        self.assertEquals(forms.find_next(self.view, 10), Region(10, 13))
        self.set_view_content("(identity 0.2)")
        self.assertEquals(forms.find_next(self.view, 10), Region(10, 13))
        self.set_view_content("(a (b) c)")
        self.assertEquals(forms.find_next(self.view, 2), Region(3, 6))
        self.set_view_content("(a (b) c)")
        self.assertEquals(forms.find_next(self.view, 3), Region(3, 6))
        self.set_view_content("abc?")
        self.assertEquals(forms.find_next(self.view, 0), Region(0, 4))
        self.set_view_content("<abc>")
        self.assertEquals(forms.find_next(self.view, 0), Region(0, 5))
        self.set_view_content("=abc=")
        self.assertEquals(forms.find_next(self.view, 0), Region(0, 5))
        self.set_view_content("+abc+")
        self.assertEquals(forms.find_next(self.view, 0), Region(0, 5))
        self.set_view_content("!abc!")
        self.assertEquals(forms.find_next(self.view, 0), Region(0, 5))
        self.set_view_content("*abc*")
        self.assertEquals(forms.find_next(self.view, 0), Region(0, 5))
        self.set_view_content("äöå")
        self.assertEquals(forms.find_next(self.view, 0), Region(0, 3))
        self.set_view_content("(ns ^:foo bar.baz)")
        self.assertEquals(forms.find_next(self.view, 3), Region(4, 9))
        self.set_view_content("""(foo 'bar)""")
        self.assertEquals(forms.find_next(self.view, 4), Region(5, 9))
        self.set_view_content("""(foo 'bar baz)""")
        self.assertEquals(forms.find_next(self.view, 4), Region(5, 9))
        self.set_view_content("""(foo #'bar baz)""")
        self.assertEquals(forms.find_next(self.view, 4), Region(5, 10))
        self.set_view_content("""(foo #_(bar) baz)""")
        self.assertEquals(forms.find_next(self.view, 4), Region(5, 12))
        self.set_view_content("""(foo #?(:cljs bar) baz)""")
        self.assertEquals(forms.find_next(self.view, 4), Region(5, 18))
        self.set_view_content("""(foo #bar/baz [:quux 2])""")
        self.assertEquals(forms.find_next(self.view, 4), Region(5, 23))
        self.set_view_content("""(inc 1)\n ;; bar""")
        self.assertEquals(forms.find_next(self.view, 7), None)
        self.set_view_content('"a b"')
        self.assertEquals(forms.find_next(self.view, 1), Region(0, 5))
        self.set_view_content("""'(foo (bar) baz)""")
        self.assertEquals(forms.find_next(self.view, 11), Region(12, 15))
        self.set_view_content("""^{:foo 1}""")
        self.assertEquals(forms.find_next(self.view, 6), Region(7, 8))
        self.set_view_content(""";; foo bar""")
        self.assertEquals(forms.find_next(self.view, 2), Region(3, 6))
        self.assertEquals(forms.find_next(self.view, 6), Region(7, 10))
        self.set_view_content("""(foo)(bar)""")
        self.assertEquals(forms.find_next(self.view, 5), Region(5, 10))
        self.set_view_content("""{:a 1, :b 2}""")
        self.assertEquals(forms.find_next(self.view, 5), Region(7, 9))

    def test_find_previous(self):
        self.set_view_content("a")
        self.assertEquals(forms.find_previous(self.view, 1), Region(0, 1))
        self.set_view_content("(a)")
        self.assertEquals(forms.find_previous(self.view, 1), None)
        self.set_view_content("(a)")
        self.assertEquals(forms.find_previous(self.view, 3), Region(0, 3))
        self.set_view_content(":foo")
        self.assertEquals(forms.find_previous(self.view, 4), Region(0, 4))
        self.set_view_content(":foo/bar")
        self.assertEquals(forms.find_previous(self.view, 8), Region(0, 8))
        self.set_view_content("(identity 1/2)")
        self.assertEquals(forms.find_previous(self.view, 13), Region(10, 13))
        self.set_view_content("(identity 0.2)")
        self.assertEquals(forms.find_previous(self.view, 13), Region(10, 13))
        self.set_view_content("(a (b) c)")
        self.assertEquals(forms.find_previous(self.view, 6), Region(3, 6))
        self.set_view_content("(a (b) c)")
        self.assertEquals(forms.find_previous(self.view, 7), Region(3, 6))
        self.set_view_content("abc?")
        self.assertEquals(forms.find_previous(self.view, 4), Region(0, 4))
        self.set_view_content("<abc>")
        self.assertEquals(forms.find_previous(self.view, 5), Region(0, 5))
        self.set_view_content("=abc=")
        self.assertEquals(forms.find_previous(self.view, 5), Region(0, 5))
        self.set_view_content("+abc+")
        self.assertEquals(forms.find_previous(self.view, 5), Region(0, 5))
        self.set_view_content("!abc!")
        self.assertEquals(forms.find_previous(self.view, 5), Region(0, 5))
        self.set_view_content("*abc*")
        self.assertEquals(forms.find_previous(self.view, 5), Region(0, 5))
        self.set_view_content("äöå")
        self.assertEquals(forms.find_previous(self.view, 3), Region(0, 3))
        self.set_view_content("(ns ^:foo bar.baz)")
        self.assertEquals(forms.find_previous(self.view, 10), Region(4, 9))
        self.set_view_content("""(foo 'bar)""")
        self.assertEquals(forms.find_previous(self.view, 9), Region(5, 9))
        self.set_view_content("""(foo 'bar baz)""")
        self.assertEquals(forms.find_previous(self.view, 10), Region(5, 9))
        self.set_view_content("""(foo #'bar baz)""")
        self.assertEquals(forms.find_previous(self.view, 11), Region(5, 10))
        self.set_view_content('(foo "bar")')
        self.assertEquals(forms.find_previous(self.view, 10), Region(5, 10))
        self.set_view_content("""(foo #_(bar) baz)""")
        self.assertEquals(forms.find_previous(self.view, 13), Region(5, 12))
        self.set_view_content("""(foo #?(:cljs bar) baz)""")
        self.assertEquals(forms.find_previous(self.view, 19), Region(5, 18))
        self.set_view_content(""";; bar\n(inc 1)""")
        self.assertEquals(forms.find_previous(self.view, 7), None)
        self.set_view_content('"a b"')
        self.assertEquals(forms.find_previous(self.view, 3), Region(0, 5))
        self.set_view_content("""'(foo (bar) baz)""")
        self.assertEquals(forms.find_previous(self.view, 12), Region(6, 11))
        self.set_view_content("""^{:foo 1}""")
        self.assertEquals(forms.find_previous(self.view, 6), Region(2, 6))
        self.set_view_content(""";; foo bar""")
        self.assertEquals(forms.find_previous(self.view, 10), Region(7, 10))
        self.assertEquals(forms.find_previous(self.view, 7), Region(3, 6))
        self.set_view_content("""(foo)(bar)""")
        self.assertEquals(forms.find_previous(self.view, 5), Region(0, 5))
        self.set_view_content("""{:a 1, :b 2}""")
        self.assertEquals(forms.find_previous(self.view, 7), Region(4, 5))

    def test_seek_backward(self):
        def is_function(form):
            return self.view.match_selector(form.begin(), "variable.function")
        self.set_view_content("""(map inc (range 10))""")
        self.assertTrue(forms.seek_backward(self.view, 0, is_function).empty())
        self.assertEquals(Region(1, 4), forms.seek_backward(self.view, 5, is_function))
        self.assertEquals(
            Region(10, 15), forms.seek_backward(self.view, 16, is_function)
        )
        self.assertEquals(Region(1, 4), forms.seek_backward(self.view, 19, is_function))
        self.assertTrue(forms.seek_backward(self.view, 20, is_function).empty())
