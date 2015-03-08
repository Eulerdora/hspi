# hspi
hspi is a command-line tool to read all the .js and .css files contents used in a .html file, and insert those contents into a single html file. 
It also tries to get the files over network if necessary.

## Usage
```sh
hspi [project path] [-options]
Options:
  --no-script:
     Do not merge JavaScript file 
      (do not insert contents of .js file into a script tag)
  --no-css:
      Do not merge CSS file 
      (do not insert contents of .css file into a style tag)
  --add=[tag1,attr1/tag2,attr2/...]:
      Add optional pair of tag and attribute, which will be included into a file.
  -a [tag1,attr/tag2,attr2/...]:
      The same as "--add"
  --input=[input file]:
      set input html file
        -i [input file]:
      The same as "--input"
  --output=[output file]:
      set output html file path
  -o [output file]:
      The same as "--output"
```

### Version
0.1.0.0

### Installation

cabal and ghc are required for installation.

```sh
$ git clone [git-repo-url] hspi
$ cd hspi
$ cabal install
```
