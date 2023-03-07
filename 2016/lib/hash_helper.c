#include <SWI-Prolog.h>
#include <md5.h>
#include <stdio.h>
#include <string.h>

// check that first n characters of digest are 0s
int prefix_match(int n, char *digest) {
    for (int i = 0; i < n; ++i) {
        if (digest[i] != 0)
            return 0;
    }

    return 1;
}

// md5(data + nonce) = (n 0s) + suffix
// foreign_t
// pl_data_n_suffix(term_t data_t, term_t n_t, term_t suffix_t, control_t handle) {
//     int *nonce;
// 
//     switch (PL_foreign_control(handle)) {
//         case PL_FIRST_CALL:
//             nonce = malloc(sizeof(*nonce));
//             *nonce = 0;
//             break;
//         case PL_REDO:
//             nonce = PL_foreign_context_address(handle);
//             break;
//         case PL_PRUNED:
//             nonce = PL_foreign_context_address(handle);
//             free(nonce);
//             PL_succeed;
//     }
// 
//     char *data; PL_get_list_chars(data_t, &data, 0);
// 
//     int n; PL_get_integer(n_t, &n);
// 
//     unsigned char digest[16];
//     struct MD5Context ctx;
// 
//     do {
//         char *str = malloc(strlen(data) + 10 + 1);
//         sprintf(str, "%s%d", data, *nonce);
// 
//         MD5Init(&ctx);
//         MD5Update(&ctx, str, strlen(str));
//         MD5Final(digest, &ctx);
// 
//         (*nonce)++;
//     } while (!prefix_match(n, digest));
// 
//     char suffix[16 - n];
//     for (int i = n; i < 16; ++i) {
//         suffix[i - n] = digest[i];
//     }
//     PL_unify_list_chars(suffix_t, suffix);
// 
//     PL_retry_address(nonce);
// }

foreign_t pl_data_n_suffix(term_t data_t, term_t n_t, term_t suffix_t) {
    int nonce = 0;

    char *data; PL_get_list_chars(data_t, &data, 0);

    int n; PL_get_integer(n_t, &n);

    MD5_CTX ctx; unsigned char digest[16];
    char *str = malloc(strlen(data) + 10 + 1);

    do {
        sprintf(str, "%s%d", data, nonce);

        MD5Init(&ctx);
        MD5Update(&ctx, str, strlen(str));
        MD5Final(digest, &ctx);

        nonce++;
    } while (!prefix_match(n, digest));

    char suffix[16 - n];
    for (int i = n; i < 16; ++i) {
        suffix[i - n] = digest[i];
    }
    PL_unify_list_chars(suffix_t, suffix);

    PL_succeed;
}

install_t install_hash_helper() {
    // PL_register_foreign("data_n_suffix", 3, pl_data_n_suffix, 0b00100);
    PL_register_foreign("data_n_suffix", 3, pl_data_n_suffix, 0);
}
