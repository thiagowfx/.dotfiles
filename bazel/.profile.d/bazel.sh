#!/bin/sh
#
# Bazel Colorizer
#
# Intercepts output of `bazel` to apply colors to log messages and test results.
# Replace errors with a more colorful version so they are easier to spot.
#
# Source this file (or place in ~/.bashrc or ~/.zshrc) and use bazel as usual:
#   bazel run -- "path/to/binary" --logtostderr
#   bazel build "//path/to:build"
#
# To revert this behavior:
#   Only for the next execution: `command bazel`.
#   For the entire shell session: `unset -f bazel`.

__bazel_color() {
  unbuffer "$@" 2>&1 | sed -u '
#!/bin/sed -f

# Paint sponge links with cyan
/http:\/\/sponge2\/[0-9a-z-]\+/ {
  s#http://sponge2/[0-9a-z-]\+#\x1b[00;36m\0\x1b[0m#g ;
}

# Matches a BUILD error
# ERROR: [filename]:row:col:
:top
/.*ERROR:.*\/google\/src.*BUILD:/ {
  i
  i\\x1b[1;31m────>>> BUILD Error <<<────

  # Strip out existing colors
  s#\x1b\[[^m]*m##g;

  # Print the line numbers and message in color.
  s#BUILD:\([0-9]\+:[0-9]\+: \)\(.*\)#BUILD:\x1b[01;33m\1\x1b[0;31m\2\x1b[0m#g

  # Print the ERROR: in color
  s#ERROR:#\x1b[31;01m>>>\x1b[0m#g

  # Print the filename in color
  s#/\S\+/BUILD#\x1b[33m\0\x1b[0m#g

  # Python tracebacks, highlight this
  /\s*Traceback/ {
:pythonloop
    n;

    /^\S/ {
      # No-indent means end of trace back.
      btop
    }

    s#.*#\x1b[1;31m>>> \x1b[0;31m\0\x1b[0m#
    bpythonloop
  }

  btop
}

# Matches a GCL / Golang / Proto error
# [filename].[extension]:[linenr]:[colnr]:
/[a-zA-Z_0-9/]\+\.\(gcl\|go\|proto\):[0-9]\+:[0-9]\+: / {
  # Print the first line in reasonable colors
  s#\(^.\+\.\([[:lower:]]\+\)\)\(:[0-9]\+:[0-9]\+\): \(.*\)#\n\x1b[1;31m────>>> \2 error <<<────\n\n\x1b[31;01m>>> \x1b[00;33m\1\x1b[01;33m\3 \x1b[0;31m\4\x1b[00m#g ;
}

# Matches a Soy error.
# [filename].[extension]:[linenr]:
/[a-zA-Z_0-9/]\+\.soy:[0-9]\+: / {
  i
  i\\x1b[1;31m────>>> soy error <<<────

  # Print the first line in reasonable colors
  s#\(^.\+\.soy\)\(:[0-9]\+\): \(.*\)# \x1b[31;01m>>> \x1b[00;33m\1\x1b[01;33m\2 \x1b[0;31m\3\x1b[00m#g ;
}

# Matches a Java error.
# [filename].[extension]:[linenr]:
/[a-zA-Z_0-9/]\+\.java:[0-9]\+: / {
  i
  i\\x1b[1;31m────>>> java error <<<────

  # Print the first line in reasonable colors
  s#\(^.\+\.java\)\(:[0-9]\+\): \(.*\)#\x1b[31;01m>>> \x1b[00;33m\1\x1b[01;33m\2 \x1b[0;31m\3\x1b[00m#g ;

:javaloop;
    # Color the rest of the error in this loop. Take the next line.
    n;

    /^\S/ {
      # If the next line does not start with a space, we are done.
      btop;
    }

    # If the next line does start with a space, modify the pattern buffer.
    s#.*#\x1b[31;01m>>>\x1b[0;31m \0\x1b[0m#g;

    s#reason: \(.*\)#\x1b[33mreason: \x1b[31m\1\x1b[0m#g;
    s#found: \(.*\)#\x1b[33mfound: \x1b[31m\1\x1b[0m#g;
    s#required: \(.*\)#\x1b[33mrequired: \x1b[31m\1\x1b[0m#g;

    s#symbol: \(.*\)#\x1b[33msymbol: \x1b[31m\1\x1b[0m#g;
    s#location: \(.*\)#\x1b[33mlocation: \x1b[31m\1\x1b[0m#g;

  bjavaloop;
}

s/\(Target \)\(.*\)\( failed to build\)/\1\x1b[1;31m\2\x1b[31m\3\x1b[0m/g
'
}

bazel() {
  if [ -t 1 ]; then
    __bazel_color bazel "$@"
  else
    command bazel "$@"
  fi
}
