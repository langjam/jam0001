es_endings = ("ch", "s", "sh", "x", "o", "z")
vowel_o_endings = ("ao", "eo", "io", "oo", "uo")
vowel_z_endings = ("az", "ez", "iz", "oz", "uz")
vowel_y_endings = ("ay", "ey", "iy", "oy", "uy")


def conjugate_third_person(verb: str):
    if verb == "be":
        return "is"
    elif verb == "have":
        return "has"
    elif verb.endswith(vowel_o_endings) or verb.endswith(vowel_y_endings):
        # An exception to the other rules containing "o" or "y".
        return verb + "s"
    elif verb.endswith(vowel_z_endings):
        return verb + "zes"
    elif verb.endswith(es_endings):
        return verb + "es"
    elif verb.endswith("y"):
        return verb.rstrip("y") + "ies"
    else:
        return verb + "s"


def get_funcdef_name(func_call: str):
    words = func_call.split()
    first_word = conjugate_third_person(words[0]).title()
    if len(words) == 1:
        return first_word
    else:
        rest_of_name = " ".join(words[1:])
        return first_word + " " + rest_of_name
