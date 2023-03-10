#include <SWI-Prolog.h>
#include <md5.h>
#include <stdio.h>
#include <string.h>

void md5(MD5_CTX *ctx, size_t stretch, const char *str, uint8_t *digest, char *md5str) {
    MD5Init(ctx);
    MD5Update(ctx, str, strlen(str));
    MD5Final(digest, ctx);

    for (size_t i = 0; i < 16; ++i)
        sprintf(&md5str[i * 2], "%02x", digest[i]);

    for (size_t i = 0; i < stretch; ++i) {
        MD5Init(ctx);
        MD5Update(ctx, md5str, strlen(md5str));
        MD5Final(digest, ctx);

        for (size_t i = 0; i < 16; ++i)
            sprintf(&md5str[i * 2], "%02x", digest[i]);
    }
}

// md5(data + nonce) = prefix + suffix
foreign_t pl_data_prefix_suffix(term_t data_t, term_t prefix_t, term_t suffix_t, control_t handle) {
    size_t *nonce;

    switch (PL_foreign_control(handle)) {
        case PL_FIRST_CALL:
            nonce = PL_malloc(sizeof(*nonce));
            *nonce = 0;
            break;
        case PL_REDO:
            nonce = PL_foreign_context_address(handle);
            break;
        case PL_PRUNED:
            nonce = PL_foreign_context_address(handle);
            PL_free(nonce);
            PL_succeed;
    }

    char *copy;
    PL_get_list_chars(data_t, &copy, 0);
    char *data = strdup(copy);
    PL_get_list_chars(prefix_t, &copy, 0);
    char *prefix = strdup(copy);

    MD5_CTX ctx; uint8_t digest[16]; char md5str[33];
    char *str = PL_malloc(strlen(data) + 11);

    do {
        sprintf(str, "%s%ld", data, *nonce);
        md5(&ctx, 0, str, digest, md5str);
        (*nonce)++;
    } while (strncmp(prefix, md5str, strlen(prefix)) != 0);

    size_t n = strlen(prefix);
    char suffix[33 - n];
    for (size_t i = n; i < 33; ++i) {
        suffix[i - n] = md5str[i];
    }
    PL_unify_list_chars(suffix_t, suffix);

    PL_free(data);
    PL_free(prefix);
    PL_free(str);

    PL_retry_address(nonce);
}

typedef struct {
    size_t nonce;
    char hash[33];
    size_t valid_nonces[16];
} STATE;

size_t hex_to_idx(char hex) {
    switch (hex) {
        case '0': return 0;
        case '1': return 1;
        case '2': return 2;
        case '3': return 3;
        case '4': return 4;
        case '5': return 5;
        case '6': return 6;
        case '7': return 7;
        case '8': return 8;
        case '9': return 9;
        case 'a': return 10;
        case 'b': return 11;
        case 'c': return 12;
        case 'd': return 13;
        case 'e': return 14;
        case 'f': return 15;
    }
}

void five_in_row(size_t *valid_nonces, char *hash, size_t nonce) {
    for (size_t i = 4; i < 33; ++i) {
        if (hash[i - 4] == hash[i - 3] && hash[i - 3] == hash[i - 2] &&
            hash[i - 2] == hash[i - 1] && hash[i - 1] == hash[i]) {
            size_t idx = hex_to_idx(hash[i]);
            valid_nonces[idx] = nonce;
        }
    }
}

int valid_key(STATE *st) {
    char *hash = st->hash;

    for (size_t i = 2; i < 33; ++i) {
        if (hash[i - 2] == hash[i - 1] && hash[i - 1] == hash[i]) {
            size_t idx = hex_to_idx(hash[i]);
            return st->valid_nonces[idx] > st->nonce;
        }
    }

    return 0;
}

// md5(salt + nonce) = ...AAA...
// one of the next 1000 md5s has ...AAAAA...
foreign_t pl_salt_stretch_key(term_t salt_t, term_t stretch_t, term_t key_t, control_t handle) {
    STATE *st;

    char *copy;
    PL_get_list_chars(salt_t, &copy, 0);
    char *salt = strdup(copy);

    size_t stretch;
    PL_get_uint64(stretch_t, &stretch);

    MD5_CTX ctx; uint8_t digest[16]; char md5str[33];
    char *str = PL_malloc(strlen(salt) + 11);

    switch (PL_foreign_control(handle)) {
        case PL_FIRST_CALL:
            st = PL_malloc(sizeof(*st));

            for (size_t i = 1; i < 1001; ++i) {
                sprintf(str, "%s%ld", salt, i);
                md5(&ctx, stretch, str, digest, md5str);
                five_in_row(st->valid_nonces, md5str, i);
            }

            st->nonce = 0;
            sprintf(str, "%s%ld", salt, st->nonce);
            md5(&ctx, stretch, str, digest, st->hash);

            break;
        case PL_REDO:
            st = PL_foreign_context_address(handle);

            sprintf(str, "%s%ld", salt, st->nonce + 1001);
            md5(&ctx, stretch, str, digest, md5str);
            five_in_row(st->valid_nonces, md5str, st->nonce + 1001);
            st->nonce++;
            sprintf(str, "%s%ld", salt, st->nonce);
            md5(&ctx, stretch, str, digest, st->hash);

            break;
        case PL_PRUNED:
            st = PL_foreign_context_address(handle);
            PL_free(st);
            PL_succeed;
    }

    while (!valid_key(st)) {
        sprintf(str, "%s%ld", salt, st->nonce + 1001);
        md5(&ctx, stretch, str, digest, md5str);
        five_in_row(st->valid_nonces, md5str, st->nonce + 1001);
        st->nonce++;
        sprintf(str, "%s%ld", salt, st->nonce);
        md5(&ctx, stretch, str, digest, st->hash);
    }

    PL_unify_uint64(key_t, st->nonce);

    PL_free(salt);
    PL_free(str);

    PL_retry_address(st);
}

install_t install_hash_helper() {
    PL_register_foreign("data_prefix_suffix", 3, pl_data_prefix_suffix, 0b00100);
    PL_register_foreign("salt_stretch_key", 3, pl_salt_stretch_key, 0b00100);
}
