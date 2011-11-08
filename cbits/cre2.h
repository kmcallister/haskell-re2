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


#ifdef __cplusplus
} // extern "C"
#endif
