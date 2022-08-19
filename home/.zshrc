LLVM_PATH="/opt/homebrew/opt/llvm"
LLVM_VERSION="14.0.6"
SDKROOT=$(xcrun --sdk macosx --show-sdk-path)
LIBXML_PATH="/opt/homebrew/opt/libxml2"
LIBXSLT_PATH="/opt/homebrew/opt/libxslt"

export PATH="/opt/homebrew/opt/python/libexec/bin:$LLVM_PATH/bin:/opt/homebrew/opt/texinfo/bin:/usr/local/smlnj/bin:$HOME/.jenv/bin:$PATH"

export LDFLAGS="-L$LLVM_PATH/lib -L$LIBXML_PATH/lib -L$LIBXSLT_PATH/lib -L$SDKROOT/usr/lib -Wl,-rpath,$LLVM_PATH/lib"
export CPPFLAGS="-I$LLVM_PATH/include -I$LIBXML_PATH/include -I$LIBXSLT_PATH/include -isysroot $SDKROOT"
export LIBRARY_PATH="$LIBRARY_PATH:/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"
export LD_LIBRARY_PATH="$LLVM_PATH/lib/:$LD_LIBRARY_PATH"
export CPATH="$LLVM_PATH/lib/clang/$LLVM_VERSION/include/"
export CC="$LLVM_PATH/bin/clang"
export CXX="$LLVM_PATH/bin/clang++"
export PKG_CONFIG_PATH="$LIBXML_PATH/lib/pkgconfig:$LIBXSLT_PATH/lib/pkgconfig"

eval "$(jenv init -)"
