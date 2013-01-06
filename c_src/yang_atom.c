#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "yang_atom.h"

#define NATOMS 29989

struct bucket {
    yang_atom_t atom;
    int len;
    struct bucket *next;
};

static struct bucket *atom_table[NATOMS];

static unsigned long
hash(const char *str, int len)
{
    unsigned long hash = 5381;
    int i;

    for (i = 0; i < len; i++) {
        hash = ((hash << 5) + hash) + str[i];
    }
    return hash;
}

yang_atom_t
yang_make_atom_len(const char *str, int len)
{
    unsigned long h;
    int idx;
    struct bucket *p;

    h = hash(str, len);
    idx = h % NATOMS;
    for (p = atom_table[idx]; p != NULL; p = p->next) {
        if (p->len == len &&
            strncmp(str, p->atom, len) == 0) {
            return p->atom;
        }
    }
    p = (struct bucket *)malloc(sizeof(struct bucket));
    p->atom = (yang_atom_t)malloc(sizeof(char) * (len + 1));
    strncpy(p->atom, str, len);
    p->atom[len] = '\0';
    p->len = len;
    p->next = atom_table[idx];
    atom_table[idx] = p;
    return p->atom;
}

yang_atom_t
yang_make_atom(const char *str)
{
    return yang_make_atom_len(str, strlen(str));
}

int
yang_is_atom_len(const char *str, int len)
{
    unsigned long h;
    int idx;
    struct bucket *p;

    h = hash(str, len);
    idx = h % NATOMS;
    for (p = atom_table[idx]; p != NULL; p = p->next) {
        if (p->atom == (yang_atom_t)str) {
            return 1;
        }
    }
    return 0;
}

int
yang_is_atom(const char *str)
{
    return yang_is_atom_len(str, strlen(str));
}

void
yang_print_atom_table(void)
{
    int i;
    struct bucket *p;

    for (i = 0; i < NATOMS; i++) {
        for (p = atom_table[i]; p != NULL; p = p->next) {
            printf("atom: %s\n", p->atom);
        }
    }
}
