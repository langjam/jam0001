#include "parser.h"
#include "runtime.h"
#include "lang.h"
#include <iostream>

int main(int argc, char* argv[])
{
    str_t source = S("a = 15\ni = a * 2");

    if(argc > 1)
    {
        bool verbose = false;
        for(int i = 0; i < argc; i++)
        {
            if(CONVERT_STR(argv[i]) == S("-v"))
            {
                verbose = true;
            }
        }

        lang_execute_from_file(argv[1], verbose);
    }
    
    return 0;
}
