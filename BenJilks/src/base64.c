#include "base64.h"
#include <math.h>
#include <stdbool.h>

static const char *lookup = 
    "0123456789"
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "_$";

static int digit_value(char digit)
{
    // FIXME: Very slow
    for (int i = 0; i < 64; i++)
    {
        if (digit == lookup[i])
            return i;
    }

    return NAN;
}

double base64_decode_double(const char *str)
{
    double result = 0;
    int digits_after_point = 0;

    bool is_negetive = false;
    if (*str == '-')
    {
        is_negetive = true;
        str += 1;
    }

    for (const char *c = str; *c; c++)
    {
        if (digits_after_point == 0)
        {
            if (*c == '.')
                digits_after_point += 64;
            else
                result = result*64 + digit_value(*c);
        }
        else
        {
            if (*c == '.')
                return NAN;

            result += (double)digit_value(*c) / digits_after_point;
            digits_after_point *= 64;
        }

        // Check for error
        if (result == NAN)
            return NAN;
    }

    if (is_negetive)
        result = -result;
    return result;
}

