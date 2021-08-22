#ifndef _NAMED_ENUM_H_
#define _NAMED_ENUM_H_

#define _GENERATE_ENUM_VALUE(name, unused) \
    name,
#define _GENERATE_ENUM_NAME(name, unused) \
    #name ,

#define GENERATE_NAMED_ENUM_H(name, list) \
    enum name {\
        list(_GENERATE_ENUM_VALUE) \
    }; \
    extern const char * name ## _names[]; 

#define GENERATE_NAMED_ENUM_C(name, list ) \
    const char * name ## _names[] = {\
        list(_GENERATE_ENUM_NAME)\
    }
    
// jaaaaank
#define _GENERATE_LUT_PAIR(left, right) \
    right ,
#define GENERATE_LUT_TABLE_H(name, list) \
    extern const int name ## _lut[];
#define GENERATE_LUT_TABLE_C(name, list) \
    const int name ## _lut[] = {\
        list(_GENERATE_LUT_PAIR) \
    };
#endif