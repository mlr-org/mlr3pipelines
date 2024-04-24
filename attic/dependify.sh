#!/bin/bash
if [ -z "$2" ] ; then
  echo "Usage: $0 <library to look for> <things to require>" >&2
  exit 1
fi
PATPART="$1"
REQUIRE="$2"
# Loop through each .R file in the current directory
for file in *.R; do
  # Check if file contains the pattern "^#'.*rpart"
  if grep -q "^#'.*${PATPART}" "$file"; then
    # Use awk to edit the file in-place
    awk '
      # Set a flag when the @examples line is found
      /^#'"'"' @examples$/ { 
        print;
        print "#'"'"' \\dontshow{ if (requireNamespace(\"'"${REQUIRE}"'\")) \\{ }";
        found = 1; 
        next; 
      }

      # After the @examples line, insert the closing dontshow before the first non-#" or #" @ line
      found && (!/^#'"'"'/ || /^#'"'"' @/) {
        print "#'"'"' \\dontshow{ \\} }";
        found = 0;
      }

      # Print every line of the file
      { print; }
    ' "$file" > tmp_file && mv tmp_file "$file"
  fi
done
