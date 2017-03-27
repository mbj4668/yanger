BEGIN {
    printf("%% NOTE: this file has been generated from yang_error.h.\n");
    printf("%% Do not edit.\n");
    printf("-module(yang_llerror).\n");
    printf("-export([code2err/1, codes/0]).\n\n");
}

/^#define YANG_ERR_/ {
    errors[$2] = $3;
}
/^#define YANG_WARN_/ {
    warnings[$2] = $3;
}

END {
    for (err in errors) {
        printf("code2err(%s) -> '%s';\n", errors[err], err);
    }
    for (err in warnings) {
        printf("code2err(%s) -> '%s';\n", warnings[err], err);
    }
    printf("code2err(_) -> undefined.\n\n");

    printf("codes() ->\n");
    printf("    [\n");
    for (err in errors) {
        printf("     {'%s', error, \"~s\"},\n", err);
    }
    for (err in warnings) {
        printf("     {'%s', warning, \"~s\"},\n", err);
    }
    printf("    {dummy, error, \"~s\"}].\n");
}
