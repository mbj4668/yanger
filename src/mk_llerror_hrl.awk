BEGIN {
    printf("%% NOTE: this file has been generated from yang_error.h.\n");
    printf("%% Do not edit.\n\n");
}

/^#define YANG_FIRST_WARNING/ {
    printf("-define(YANG_FIRST_WARNING, %s).\n", $3)
}
