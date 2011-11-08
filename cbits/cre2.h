#ifdef __cplusplus
extern "C" {
#endif


typedef void cre2_options;

enum Encoding {
    EncodingUTF8 = 1,
    EncodingLatin1
};

cre2_options *cre2_opt_new(void);
void cre2_opt_delete(cre2_options *opt);

void cre2_opt_posix_syntax(cre2_options *opt, int flag);
void cre2_opt_longest_match(cre2_options *opt, int flag);
void cre2_opt_log_errors(cre2_options *opt, int flag);
void cre2_opt_literal(cre2_options *opt, int flag);
void cre2_opt_never_nl(cre2_options *opt, int flag);
void cre2_opt_case_sensitive(cre2_options *opt, int flag);
void cre2_opt_perl_classes(cre2_options *opt, int flag);
void cre2_opt_word_boundary(cre2_options *opt, int flag);
void cre2_opt_one_line(cre2_options *opt, int flag);
void cre2_opt_encoding(cre2_options *opt, enum Encoding enc);
void cre2_opt_max_mem(cre2_options *opt, int m);


typedef void cre2;

cre2 *cre2_new(const char *pattern, const cre2_options *opt);
void cre2_delete(cre2 *re);

int cre2_error_code(const cre2 *re);
char *cre2_error_string(const cre2 *re);  // must free() string
char *cre2_error_arg(const cre2 *re);     // must free() string
int cre2_num_capturing_groups(const cre2 *re);
int cre2_program_size(const cre2 *re);


enum Anchor {
    UNANCHORED,
    ANCHOR_START,
    ANCHOR_BOTH
};

struct string_piece {
    const char *data;
    int length;
};

int cre2_match(
      const cre2 *re
    , const char *text
    , int startpos
    , int endpos
    , enum Anchor anchor
    , struct string_piece *match
    , int nmatch);


#ifdef __cplusplus
} // extern "C"
#endif
