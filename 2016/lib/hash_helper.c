#include <SWI-Prolog.h>
#include <string.h>

// md5(data + nonce) = prefix + _
static foreign_t
pl_prefix_data_nonce(term_t prefix, term_t data, term_t nonce) {
    char **s;
    PL_get_list_chars(input, s, 0);
    PL_succeed;
}

install_t install_hash_helper() {
    PL_register_foreign("prefix_data_nonce", 3, pl_prefix_data_nonce, 0);
}
