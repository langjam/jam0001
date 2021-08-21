import unittest
from conjugate_third_person import conjugate_third_person, get_funcdef_name


class TestConjugateThirdPerson(unittest.TestCase):
    def test_on_be(self):
        actual_result = conjugate_third_person("be")
        expected_result = "is"
        self.assertEqual(actual_result, expected_result)

    def test_on_have(self):
        actual_result = conjugate_third_person("have")
        expected_result = "has"
        self.assertEqual(actual_result, expected_result)

    def test_on_do(self):
        actual_result = conjugate_third_person("do")
        expected_result = "does"
        self.assertEqual(actual_result, expected_result)

    def test_on_do(self):
        actual_result = conjugate_third_person("go")
        expected_result = "goes"
        self.assertEqual(actual_result, expected_result)

    def test_on_make(self):
        actual_result = conjugate_third_person("make")
        expected_result = "makes"
        self.assertEqual(actual_result, expected_result)

    def test_on_echo(self):
        actual_result = conjugate_third_person("echo")
        expected_result = "echoes"
        self.assertEqual(actual_result, expected_result)

    def test_on_moo(self):
        actual_result = conjugate_third_person("moo")
        expected_result = "moos"
        self.assertEqual(actual_result, expected_result)

    def test_on_video(self):
        actual_result = conjugate_third_person("video")
        expected_result = "videos"
        self.assertEqual(actual_result, expected_result)

    def test_on_catch(self):
        actual_result = conjugate_third_person("catch")
        expected_result = "catches"
        self.assertEqual(actual_result, expected_result)

    def test_on_toss(self):
        actual_result = conjugate_third_person("toss")
        expected_result = "tosses"
        self.assertEqual(actual_result, expected_result)

    def test_on_mash(self):
        actual_result = conjugate_third_person("mash")
        expected_result = "mashes"
        self.assertEqual(actual_result, expected_result)

    def test_on_fix(self):
        actual_result = conjugate_third_person("fix")
        expected_result = "fixes"
        self.assertEqual(actual_result, expected_result)

    def test_on_waltz(self):
        actual_result = conjugate_third_person("waltz")
        expected_result = "waltzes"
        self.assertEqual(actual_result, expected_result)

    def test_on_fizz(self):
        actual_result = conjugate_third_person("fizz")
        expected_result = "fizzes"
        self.assertEqual(actual_result, expected_result)

    def test_on_quiz(self):
        actual_result = conjugate_third_person("quiz")
        expected_result = "quizzes"
        self.assertEqual(actual_result, expected_result)

    def test_on_bury(self):
        actual_result = conjugate_third_person("bury")
        expected_result = "buries"
        self.assertEqual(actual_result, expected_result)

    def test_on_destroy(self):
        actual_result = conjugate_third_person("destroy")
        expected_result = "destroys"
        self.assertEqual(actual_result, expected_result)

class TestGetFuncdefName(unittest.TestCase):
    def test_when_lowercase(self):
        actual_result = get_funcdef_name("print a blank line")
        expected_result = "Prints a blank line"
        self.assertEqual(actual_result, expected_result)

    def test_when_starts_capital(self):
        actual_result = get_funcdef_name("Print a blank line")
        expected_result = "Prints a blank line"
        self.assertEqual(actual_result, expected_result)

    def test_when_single_word_lowercase(self):
        actual_result = get_funcdef_name("meow")
        expected_result = "Meows"
        self.assertEqual(actual_result, expected_result)

    def test_when_single_word_starts_capital(self):
        actual_result = get_funcdef_name("Meow")
        expected_result = "Meows"
        self.assertEqual(actual_result, expected_result)

    def test_on_irregular_verb(self):
        actual_result = get_funcdef_name("have a picnic")
        expected_result = "Has a picnic"
        self.assertEqual(actual_result, expected_result)


if __name__ == "__main__":
    unittest.main()
