#!/bin/bash
# filepath: /Users/joopringelberg/Code/perspectives-monorepo/packages/mycontexts/fix-react-imports.sh

# Define the directory to search
DIR="./src"

# Find all TypeScript/TSX files
find $DIR -name "*.ts" -o -name "*.tsx" | while read file; do
  echo "Processing $file..."
  
  # Create a temporary file
  tmp_file=$(mktemp)
  
  # Extract all React named imports
  react_imports=$(grep -o "import { [^}]* } from 'react';" "$file" | sed -E "s/import \{ ([^}]*) \} from 'react';/\1/g")
  
  if [ -n "$react_imports" ]; then
    # Replace all React named import statements with nothing (we'll add the destructuring later)
    sed -E "/import \{ [^}]* \} from 'react';/d" "$file" > "$tmp_file"
    
    # Check if there's a namespace import
    if grep -q "import \* as React from 'react'" "$tmp_file"; then
      # If namespace import exists, add destructuring after it
      sed -i '' -E "/import \* as React from 'react'/a\\
const { $react_imports } = React;" "$tmp_file"
    else
      # If no namespace import, add it and then the destructuring
      sed -i '' -E "1i\\
import * as React from 'react'\\
const { $react_imports } = React;\\
" "$tmp_file"
    fi
    
    # Replace the original file
    mv "$tmp_file" "$file"
  else
    rm "$tmp_file"
  fi
done

echo "All React imports have been updated!"