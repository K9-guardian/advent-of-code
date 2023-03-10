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
    char buf[1001][33];
    size_t idx;
    size_t nonce;
} STATE;

int valid_key(STATE *st) {
    char *hash = st->buf[st->idx];
    char repeat;

    // ...AAA...
    for (size_t i = 2; i < 33; ++i) {
        if (hash[i - 2] == hash[i - 1] && hash[i - 1] == hash[i]) {
            repeat = hash[i];
            break;
        }
    }

    if (repeat == 0) return 0;

    // iterate over buf
    size_t shift = 1;
    for (size_t i = (st->idx + 1) % 1001; i != st->idx; i = (i + 1) % 1001) {
        hash = st->buf[i];

        // ...AAAAA...
        for (size_t j = 4; j < 33; ++j) {
            if (hash[j] == repeat &&
                hash[j - 4] == hash[j - 3] && hash[j - 3] == hash[j - 2] &&
                hash[j - 2] == hash[j - 1] && hash[j - 1] == hash[j]) {
                return 1;
            }
        }

        shift++;
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

            for (size_t i = 0; i < 1001; ++i) {
                sprintf(str, "%s%ld", salt, i);
                md5(&ctx, stretch, str, digest, md5str);
                strcpy(st->buf[i], md5str);
            }
            st->idx = 0;
            st->nonce = 0;

            break;
        case PL_REDO:
            st = PL_foreign_context_address(handle);

            sprintf(str, "%s%ld", salt, st->nonce + 1001);
            md5(&ctx, stretch, str, digest, md5str);
            strcpy(st->buf[st->idx], md5str);
            st->idx = (st->idx + 1) % 1001;
            st->nonce++;

            break;
        case PL_PRUNED:
            st = PL_foreign_context_address(handle);
            PL_free(st);
            PL_succeed;
    }

    while (!valid_key(st)) {
        sprintf(str, "%s%ld", salt, st->nonce + 1001);
        md5(&ctx, stretch, str, digest, md5str);
        strcpy(st->buf[st->idx], md5str);

        st->idx = (st->idx + 1) % 1001;
        st->nonce++;
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
