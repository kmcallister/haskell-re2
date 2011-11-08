#include <re2/re2.h>
#include "cre2.h"

#define OPT(opt) (reinterpret_cast<RE2::Options *>(opt))

cre2_options *cre2_opt_new(void) {
    return reinterpret_cast<void*>(new RE2::Options());
}

void cre2_opt_delete(cre2_options *opt) {
    delete OPT(opt);
}


#define OPT_bool(name)  \
void cre2_opt_##name(cre2_options *opt, int flag) {  \
    OPT(opt)->set_##name(bool(flag));                \
}

OPT_bool(posix_syntax)
OPT_bool(longest_match)
OPT_bool(log_errors)
OPT_bool(literal)
OPT_bool(never_nl)
OPT_bool(case_sensitive)
OPT_bool(perl_classes)
OPT_bool(word_boundary)
OPT_bool(one_line)

#undef OPT_BOOL


void cre2_opt_encoding(cre2_options *opt, enum Encoding enc) {
    switch (enc) {
        case EncodingUTF8:
            OPT(opt)->set_encoding(RE2::Options::EncodingUTF8);
            break;
        case EncodingLatin1:
            OPT(opt)->set_encoding(RE2::Options::EncodingLatin1);
            break;
    }
}

void cre2_opt_max_mem(cre2_options *opt, int m) {
    OPT(opt)->set_max_mem(m);
}
