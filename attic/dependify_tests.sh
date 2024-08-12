#!/bin/bash

if [ -z "$2" ] ; then
  echo "Usage: $0 <pattern to look for> <package on which to skip>" >&2
  exit 1
fi
PATPART="$1"
REQUIRE="$2"

# Loop through all .R files in the current directory
for file in *.R; do
    # Use awk to edit the file in-place
    awk '
        # Enter a test_that block
        /^test_that\(/ { 
            buffer = $0 # Start buffering the block
            capture = 1  # Set flag to capture lines
            insert_needed = 0  # Reset insertion flag
            next
        }

        # Detect the end of a block
        /^})$/ && capture {
            if (insert_needed) {
                # Insert the line at the beginning of the block
                sub(/\{/, "{\n  skip_if_not_installed(\"'"${REQUIRE}"'\")", buffer)
            }
            print buffer  # Print the modified buffer
            print "})"
            buffer = ""  # Clear buffer
            capture = 0  # Stop capturing
            next
        }

        # While capturing the block
        capture {
            buffer = buffer "\n" $0  # Add line to buffer
            # If line contains "PipeOpLrnRP", mark for insertion
            if (/'"${PATPART}"'/)
                insert_needed = 1
        }

        # Print all lines outside of blocks normally
        !capture {
            print
        }
    ' "$file" > tmpfile && mv tmpfile "$file"  # Output redirection and file replacement
done

