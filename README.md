# cfz

Select strings with hands, move with fingers.. TODO.

## Installing

````
make install-deps

make
````

This should result in a binary at `bin/cfz`. Put this to your path.

## Usage

Run `echo -e "foo\nbar\nbaz" | cfz`. Move up with `M-k` and down with
`M-j`. Hitting enter exits the program and prints the selected line.

Take a look at the `examples` directory for ideas.