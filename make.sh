#!/bin/bash

set -e

echo "Compiling..."

mkdir build 2>/dev/null || true
elm make --optimize src/Main.elm --output build/main.js

# concat JS together
# cat assets/*.js build/main-elm.js > build/main.js

use_java=true

java_exe=/Users/john/bin/zulu19.32.15-ca-fx-jre19.0.2-macosx_aarch64/bin/java
closure_jar=/Users/john/bin/closure-compiler-v20180402.jar

echo "Minifying..."

if $use_java
then
  "$java_exe" -jar "$closure_jar" \
  --js build/main.js \
  --js_output_file build/main-min.js \
  --create_source_map build/main.map \
  --jscomp_off uselessCode
  # cp build/main.js build/main-min.js
else
  curl --output build/main-min.js \
    --data output_info=errors \
    --data output_info=compiled_code \
    --data-urlencode 'js_code@build/main.js' \
    https://closure-compiler.appspot.com/compile
fi

echo "Compressing..."

gzip -9 <build/main-min.js >build/main.js.gz

echo "Done!"


