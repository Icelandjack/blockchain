module Crypto.SHA 

%include C "openssl/sha.h"
%include C "sha256.h"
%link    C "sha256.o"
%lib     C "ssl"
%lib     C "crypto"

public export
sha256 : String -> String
sha256 str = unsafePerformIO (foreign FFI_C "inc" (String -> IO String) str)
