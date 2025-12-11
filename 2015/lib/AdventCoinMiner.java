import java.security.DigestException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class AdventCoinMiner {
    private final String salt;
    private final int leadingZeros;

    public AdventCoinMiner(String salt, int leadingZeros) {
        this.salt = salt;
        this.leadingZeros = leadingZeros;
    }

    public long findNonce() throws DigestException, NoSuchAlgorithmException {
        MessageDigest md5 = MessageDigest.getInstance("MD5");
        StringBuilder sb = new StringBuilder(salt);
        byte[] input = new byte[salt.length() + 11];
        byte[] buf = new byte[16];

        int leadingZeroBits = leadingZeros * 4;
        int fullZeroBytes = leadingZeroBits / 8;
        boolean remainingBit = (leadingZeroBits % 8) > 0;

        for (long nonce = 1;; nonce++) {
            sb.setLength(salt.length());
            sb.append(nonce);
            for (int i = 0; i < sb.length(); i++) {
                input[i] = (byte) sb.charAt(i);
            }
            md5.update(input, 0, sb.length());
            md5.digest(buf, 0, 16);

            boolean found = true;
            for (int i = 0; i < fullZeroBytes; i++) {
                if (buf[i] != 0) {
                    found = false;
                    break;
                }
            }
            if (!found) { continue; }

            if (remainingBit) {
                if ((buf[fullZeroBytes] & 0xF0) == 0) {
                    return nonce;
                } else {
                    continue;
                }
            } else {
                return nonce;
            }
        }
    }
}
