#include <SWI-Prolog.h>
#include <md5.h>
#include <stdio.h>
#include <string.h>

// md5(data + nonce) = prefix + suffix
foreign_t
pl_data_prefix_suffix(term_t data_t, term_t prefix_t, term_t suffix_t, control_t handle) {
    size_t *nonce;

    switch (PL_foreign_control(handle)) {
        case PL_FIRST_CALL:
            nonce = malloc(sizeof(*nonce));
            *nonce = 0;
            break;
        case PL_REDO:
            nonce = PL_foreign_context_address(handle);
            break;
        case PL_PRUNED:
            nonce = PL_foreign_context_address(handle);
            free(nonce);
            PL_succeed;
    }

    char *copy;

    PL_get_list_chars(data_t, &copy, 0);
    char* data = malloc(strlen(copy) + 1);
    strcpy(data, copy);

    PL_get_list_chars(prefix_t, &copy, 0);
    char *prefix = malloc(strlen(copy) + 1);
    strcpy(prefix, copy);

    MD5_CTX ctx; uint8_t digest[16]; char md5string[33];
    char *str = malloc(strlen(data) + 20 + 1);

    do {
        sprintf(str, "%s%ld", data, *nonce);

        MD5Init(&ctx);
        MD5Update(&ctx, str, strlen(str));
        MD5Final(digest, &ctx);

        for(int i = 0; i < 16; ++i)
            sprintf(&md5string[i*2], "%02x", digest[i]);

        (*nonce)++;
    } while (strncmp(prefix, md5string, strlen(prefix)) != 0);

    size_t n = strlen(prefix);
    char suffix[33 - n];
    for (size_t i = n; i < 33; ++i) {
        suffix[i - n] = md5string[i];
    }
    PL_unify_list_chars(suffix_t, suffix);

    free(str); free(data); free(prefix);

    PL_retry_address(nonce);
}

install_t install_hash_helper() {
    PL_register_foreign("data_prefix_suffix", 3, pl_data_prefix_suffix, 0b00100);
}
