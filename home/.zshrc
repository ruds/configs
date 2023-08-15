LLVM_PATH="/opt/homebrew/opt/llvm"
LLVM_VERSION="14.0.6"
SDKROOT=$(xcrun --sdk macosx --show-sdk-path)
LIBXML_PATH="/opt/homebrew/opt/libxml2"
LIBXSLT_PATH="/opt/homebrew/opt/libxslt"
LIBICU_PATH="$HOME/icu4c-iosx/product"
export ICU_ROOT=$LIBICU_PATH

export PATH="/opt/homebrew/opt/tcl-tk/bin:/opt/homebrew/opt/python/libexec/bin:$LLVM_PATH/bin:/opt/homebrew/opt/texinfo/bin:/usr/local/smlnj/bin:$HOME/.jenv/bin:$LIBICU_PATH/bin:$LIBICU_PATH/sbin:$PATH"

export LDFLAGS="-L$LLVM_PATH/lib -L/opt/homebrew/opt/tcl-tk/lib -L$LIBXML_PATH/lib -L$LIBXSLT_PATH/lib -L$LIBICU_PATH/lib -L$SDKROOT/usr/lib -F/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/System/Library/Frameworks/ -Wl,-rpath,$LLVM_PATH/lib"
export CPPFLAGS="-I$LLVM_PATH/include -I/opt/homebrew/opt/tcl-tk/include -I$LIBXML_PATH/include -I$LIBXSLT_PATH/include -I$LIBICU_PATH/include -isysroot $SDKROOT"
export LIBRARY_PATH="$LIBRARY_PATH:/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"
export LD_LIBRARY_PATH="$LLVM_PATH/lib/:$LD_LIBRARY_PATH"
export CPATH="$LLVM_PATH/lib/clang/$LLVM_VERSION/include/"
export CC="$LLVM_PATH/bin/clang"
export CXX="$LLVM_PATH/bin/clang++"
export PKG_CONFIG_PATH="$LIBXML_PATH/lib/pkgconfig:$LIBXSLT_PATH/lib/pkgconfig:$LIBICU_PATH/pkgconfig:/opt/homebrew/opt/tcl-tk/lib/pkgconfig"

eval "$(jenv init -)"
