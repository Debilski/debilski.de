---
title: Using a legacy environment with fish shell
date: 2014-07-30
---

# Reading bash environment vars from fish shell

I’ve recently experimented with using the [nix package manager](https://nixos.org/nix/) on OS X. The basic principle of it is that each package is installed in a folder whose name is prefixed with a unique hash code, generated from all the dependencies of that package. This means that one can build package A with dependency B and it goes to a certain directory; and when at some point when B gets a newer version upstream, one can build A again and it would go into another directory.

Symlinks and environment variables then generate a working set from those packages which can be different for each user and even for the system itself. User wants a different Python/Ruby/Haskell from what the system uses? No problem. Need a different set of packages in a subfolder for development? No problem at all. (\*)

There is a caveat, though: The environment script currently is bash only (and automatically updated) and I like to use [fish](http://fishshell.com/). If only there were some trick to extract environment variables from a bash script.

Well, at least there’s a dirty, dirty hack for that:

``` {.sh}
function bash_env --description 'Executes a bash script and sets the named variables in fish'
  # bash_env script.bash VAR1 VAR2 VARN
  #
  set -l script $argv[1]
  set -l variables
  for arg in $argv[2..-1]
    set variables $variables "echo \"set -gx $arg \${$arg}\""\n
  end
  set res (bash (cat $script (echo $variables | psub) | psub))
  echo "Importing" (count $res) "variable(s)"
  for val in $res
      echo "$val"
      eval "$val"
  end
end
```

For each specified variable name this function generates a line

```sh
echo "set -gx VAR1 $VAR1"
```


and appends this to the bash script before it is executed. The `echo` output is then collected and `eval`’d inside fish.

I am not sure whether the `| psub) | psub))` part is stylistically the right thing to do. It definitely looks a bit strange. `cmd1 (cmd2 | psub)` is supposed to be (more or less?) equivalent to the bashism `cmd1 <(cmd2)`, which means that the output of `cmd2` will be written into a virtual file whose virtual name will be given as an argument to `cmd1`. It is called *process substitution*, by the way.

Alternatively, one could read the file into a variable and execute `bash -c $VAR` but fish’s array based variables make this task a little unintuitive.

(\*) Well, of course, there is a problem. Some packages do not work as well under OS X as they might under Linux, which means I am currently only using it for a few global Haskell packages à la Pandoc.

