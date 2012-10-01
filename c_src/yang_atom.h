#ifndef _yang_atom_h
#define _yang_atom_h

typedef char *yang_atom_t;

#define yang_atom_to_str(a) ((char*)(a))

extern yang_atom_t yang_make_atom_len(const char *str, int len);
extern yang_atom_t yang_make_atom(const char *str);
extern int yang_is_atom_len(const char *str, int len);
extern int yang_is_atom(const char *str);
extern void yang_print_atom_table(void);

#endif
