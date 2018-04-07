# An experimental Lisp interpreter in Ruby

This is an experimental Lisp interpreter I wrote 10 years ago (2008) in Ruby.
It had been presented under the MIT License at <http://www.oki-osk.jp/esc/llsp/>
until last spring (2017), which has been shut down now.

It has the same features as
[l2lisp-in-python](https://github.com/nukata/l2lisp-in-python) has.

## How to use

It runs in any Ruby from 1.8 to 2.5.

```
$ ruby L2Lisp.rb
> "hello, world"
"hello, world"
> (+ 5 6)
11
> (exit 0)
$
```

You can give it a file name of your Lisp script.

```
$ ruby L2Lisp.rb fibs.l
5702887
$
```

If you put a "`-`" after the file name, it will 
begin an interactive session after running the file.

```
$ ruby L2Lisp.rb fibs.l -
5702887
> (take 20 fibs)
(1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765)
> (exit 0)
$ 
```

## License

It is under the MIT License.
See the [L2Lisp.rb](L2Lisp.rb#L98-L119) file or 
evaluate `(copyright)` in the Lisp.
