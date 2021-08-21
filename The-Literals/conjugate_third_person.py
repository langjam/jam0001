es_endings = ("ch", "s", "sh", "x", "o", "z")
vowel_o_endings = ("ao", "eo", "io", "oo", "uo")
vowel_z_endings = ("az", "ez", "iz", "oz", "uz")


def conjugate_third_person(verb: str):
    if verb == "be":
        return "is"
    elif verb == "have":
        return "has"
    elif verb.endswith(vowel_o_endings):
        # An exception to the fact that "o" is in es_endings
        return verb + "s"
    elif verb.endswith(vowel_z_endings):
        return verb + "zes"
    elif verb.endswith(es_endings):
        return verb + "es"
    elif verb.endswith("y"):
        return verb.rstrip('y') + "ies"
    else:
        return verb + "s"
