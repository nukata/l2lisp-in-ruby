#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require 'stringio'

module L2Lisp
  SUMMARY = <<'qqq'
Little Lazy Lisp 7.3 in Ruby                              H20.7/1 (鈴)

Ruby 1.8.2 以降 (JRuby を含む) による小さな Lisp インタープリタ

SYNOPSIS: L2Lisp.rb [file ...] [-]

無引数で起動すると対話セッションに入る。引数としてファイル名を与えると
それらを Lisp スクリプトとして順に実行する。引数としてハイフンを与える
と，スクリプトの実行後，対話セッションに入る。
readline がロード可能ならば，対話セッションで行編集機能を利用できる。
セッションでは本文書を (help)，著作権表示を (copyright) の Lisp 関数
呼出しでそれぞれ表示できる。

次のようにライブラリとしても利用できる:
  require "L2Lisp"
  LL = L2Lisp
  i = LL::Interp.new                 # インタープリタを構築
  i.symbol[:str] = proc {|a| a.to_s} # 組込み関数 str を追加
  e = LL.list(:str, 123)             # (str 123) を構築
  p e                                # (str 123) を表示
  r = i.eval(e)                      # (str 123) を評価 => "123"
  r = i.run("(+ 1 2 3)")             # (+ 1 2 3) を評価 => 6

各 Lisp 値は次のように Ruby のオブジェクトで表現される:
 数, 文字列 => 数, 文字列
 t          => true
 nil        => nil
 シンボル   => Symbol インスタンス (大域変数値は Interp#symbol に格納)
 cons セル  => Cell インスタンス

Lisp リストは Ruby の Enumerable として扱える。
Ruby との相互作用のために次の４関数がある:
 (ruby-eval str)
   文字列 str を Ruby 式としてトップレベルの環境で評価する。
 (ruby-send rcv sel param…)
   任意の rcv を Ruby オブジェクトとして扱い，文字列またはシンボル sel
   の名前のメソッドを，任意個数の引数 param… で呼び出す。
 (ruby-send-apply rcv sel fun param…)
   ruby-send と同様だが，関数またはマクロ fun をブロックとして渡す。
 (ruby-self)
   Ruby オブジェクトとしてのインタープリタ自身を返す。

特徴:
* 基本的には Emacs Lisp のサブセットだが，静的スコープをとる。
* 常に末尾呼出しの最適化を行う。
* *version* は版数とプラットフォームの２要素のリストを値とする。
* 関数は数や文字列と同じく自己評価的な一級の値であり固有の名を持たない。
  スペシャルフォームは一級の値ではなく，特定のシンボルで表される。
* 関数 apply (大域変数 apply の初期値である関数。以下同様) は２引数に限る。
* 除算と減算 / と - は１個以上の引数をとる。
* 除算と剰余 / と % は負数に対し Ruby の演算方法に従う。
* (eval e) は e を大域的な環境で評価する。
* (eql x y) の結果は Ruby の x.eql?(y) に従う。
* (delay x) は Scheme と同じく x の約束を作る。~x と略記できる。
  組込み関数と条件式は約束に対し implicit forcing を行う。
* (read) は EOF に対して *eof* の大域変数値を返す。
* 評価時例外 EvalError は (catch *error* …) で捕捉できる。
* (lambda …) を評価すると仮引数が "コンパイル" された関数が返される。
  このとき，入れ子で含まれている (lambda …) も再帰的にコンパイルされる。
* (macro …) は大域的な環境でだけ評価でき，"マクロ式" という関数が返される。
  このマクロ式を適用した時，引数は評価されず，適用結果が再び評価される。
* マクロ式の中の自由なシンボルは捕捉されないが，マクロ引数は捕捉され得る。
* (macro …) 内の $ で始まるシンボルは dummy symbol と解釈される。
  dummy symbol は自己評価的であり，そのマクロ式の中でだけ eq が成り立つ。
* (lambda …) の評価時，最大 MAX_MACRO_EXPS 重だけ再帰的にマクロ展開する
  (非大域的に束縛されたマクロを除く)。残りは適用時に処理される。
* 印字する時，高々 MAX_EXPANSIONS 重だけ再帰的に印字済みリストを印字する。
* (dump) は Interp#symbol のキーと環境のリストを返す。
* read 時の字句解析で文字列トークンは Ruby の文字列として評価される。
  文字列トークン内の \n などのエスケープ列はこのとき解釈される。
* 準引用のバッククォート，カンマ，カンマアットは read 時に解決される。
  例: '`((,a b) ,c ,@d) => (cons (list a 'b) (cons c d))

スペシャルフォーム:
  quote progn cond setq lambda macro catch unwind-protect delay
組込み関数:
  car cdr cons atom stringp numberp eq eql list
  prin1 princ terpri read + * / - % <
  load eval apply force rplaca rplacd throw mapcar mapc length
  ruby-eval ruby-send ruby-send-apply ruby-self
  dump help prelude copyright (および concat 実装用の _add _concat)
組込み変数:
  *error* *version* *eof*

Lisp 自身による標準の定義 (defun, if を含む) は (prelude) で表示できる。

URL: http://www.okisoft.co.jp/esc/llsp/
qqq

  COPYRIGHT = '
Copyright (c) 2007, 2008 Oki Software Co., Ltd.

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without 
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND  
NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
'

  LL = L2Lisp                   # 便宜上の短縮名

  MAX_EXPANSIONS = 5            # 再帰的に印字する深さ
  MAX_MACRO_EXPS = 20           # 静的にマクロ展開する深さ
  MAX_EXC_TRACES = 10           # 例外発生時の評価トレースの記録段数

  NONE = :'#<none>'
  S_EOF = :'#<eof>'
  S_ERROR = :'*error*'
  S_REST = :'&rest'


  class SyntaxError < RuntimeError
  end


  class EvalError < RuntimeError
    attr :trace

    def initialize(message, exp=NONE)
      super((exp == NONE) ? message : message + ': ' + LL.str(exp))
      @trace = []
    end

    def to_s
      s = "*** %s\n" % super
      @trace.each_with_index {|t, index| s += "%3d: %s\n" % [index, t]}
      return s
    end
  end


  class Thrown < EvalError
    attr :tag
    attr :value

    def initialize(tag, value)
      super('no catcher for (%s %s)' % [LL.str(tag), LL.str(value)])
      @tag = tag
      @value = value
    end
  end


  class VariableExpected < EvalError
    def initialize(exp)
      super('variable expected', exp)
    end
  end


  class ProperListExpected < EvalError
    def initialize(exp)
      super('proper list expected', exp)
    end
  end


  # cons セル
  class Cell
    include Enumerable
    attr_accessor :car, :cdr

    def initialize(car, cdr)
      @car = car
      @cdr = cdr
    end

    def inspect
      return LL.str(self)
    end

    def each                    # Lisp の mapc に相当
      j = self
      begin
        yield j.car
        j = j.cdr
        j = j.deliver if Promise === j
      end while Cell === j
      j.nil? or raise ProperListExpected, j
    end

    def length
      return inject(0) {|x, y| x + 1}
    end

    def _repr(print_quote, reclevel, printed)
      if printed.has_key?(self)
        reclevel -= 1
        return ['...'] if reclevel == 0
      else
        printed[self] = true
      end
      case kdr = ((Promise === @cdr) ? @cdr.value : @cdr)
      when nil
        s = LL.str(@car, print_quote, reclevel, printed)
        return [s]
      when Cell
        s = LL.str(@car, print_quote, reclevel, printed)
        t = kdr._repr(print_quote, reclevel, printed)
        return t.unshift(s)
      else
        s = LL.str(@car, print_quote, reclevel, printed)
        t = LL.str(kdr, print_quote, reclevel, printed)
        return [s, '.', t]
      end
    end
  end # Cell


  # 引数の文字列表現を得る
  def str(x, print_quote=true, reclevel=MAX_EXPANSIONS, printed={})
    case x
    when true
      return 't'
    when Cell
      if x.car == :quote and Cell === x.cdr and x.cdr.cdr.nil?
        return "'" + str(x.cdr.car, print_quote, reclevel, printed)
      else
        return '(' + x._repr(print_quote, reclevel, printed).join(' ') + ')'
      end
    when Symbol
      return x.to_s
    when String, Exception
      return (print_quote) ? x.inspect : x
    else
      return x.inspect
    end
  end
  module_function :str

  def list(*args)
    z = y = Cell.new(nil, nil)
    args.each {|e| y = y.cdr = Cell.new(e, nil)}
    return z.cdr
  end
  module_function :list

  def mapcar(x)
    return nil if x.nil?
    z = y = Cell.new(nil, nil)
    case x
    when String
      x.each_byte {|e| y = y.cdr = Cell.new((yield e), nil)}
    when Enumerable
      x.each {|e| y = y.cdr = Cell.new((yield e), nil)}
    else
      raise EvalError.new('Enumerable expected', x)
    end
    return z.cdr
  end
  module_function :mapcar


  module QQ                     # 準引用 (Quasi-Quotation)
    class Unquote
      attr :x

      def initialize(x)
        @x = x
      end

      def inspect
        ',' + LL.str(@x)
      end
    end


    class UnquoteSplicing
      attr :x

      def initialize(x)
        @x = x
      end

      def inspect
        ',@' + LL.str(@x)
      end
    end


    module_function

    def expand_qq(x)        # 準引用式 `x の x を等価な S 式に展開する
      case x
      when Cell
        t = _expand1(x)
        if Cell === t and Cell === t.car and t.cdr.nil?
          case t.car.car when :list, :cons
            return t.car
          end
        end
        Cell.new(:append, t)
      when Unquote
        x.x
      else
        _quote(x)
      end
    end

    def _quote(x)
      case x when Symbol, Arg, Cell
        LL.list(:quote, x)
      else
        x
      end
    end

    def _expand1(x)
      case x
      when Cell
        h = _expand2(x.car)
        t = _expand1(x.cdr)
        if Cell === t
          if t.car.nil? and t.cdr.nil?
            return LL.list(h)
          elsif Cell === h and h.car == :list
            hh = ((Cell === t.car and t.car.car == :list) ?
                  _concat(h, t.car.cdr) : _conscons(h.cdr, t.car))
            return Cell.new(hh, t.cdr)
          end
        end
        return Cell.new(h, t)
      when Unquote
        LL.list(x.x)
      else
        LL.list(_quote(x))
      end
    end

    def _concat(x, y)
      (x.nil?) ? y : Cell.new(x.car, _concat(x.cdr, y))
    end

    def _conscons(x, y)
      (x.nil?) ? y : LL.list(:cons, x.car, _conscons(x.cdr, y))
    end

    def _expand2(x)
      case x
      when Unquote
        LL.list(:list, x.x)
      when UnquoteSplicing
        x.x
      else
        LL.list(:list, expand_qq(x))
      end
    end
  end # QQ


  class DefinedFunction         # ラムダ式等の便宜的な基底クラス
    attr :arity
    attr :body
    attr :env                   # 環境 (CLOSURE 以外は常に nil)

    def initialize(arity, body)
      @arity = arity
      @body = body
    end
  end # DefinedFunction


  class MACRO < DefinedFunction
    def inspect
      LL.str(Cell.new(:'#<macro>', Cell.new(@arity, @body)))
    end
  end # MACRO


  class LAMBDA < DefinedFunction
    def inspect
      LL.str(Cell.new(:'#<lambda>', Cell.new(@arity, @body)))
    end
  end # LAMBDA


  class CLOSURE < DefinedFunction
    def initialize(env, arity, body)
      super(arity, body)
      @env = env
    end

    def inspect
      LL.str(Cell.new(:'#<closure>', Cell.new(Cell.new(@arity, @env), @body)))
    end
  end # CLOSURE


  class Arg                 # コンパイル後のラムダ式やマクロ式の仮引数
    attr :level
    attr :offset
    attr :symbol

    def initialize(level, offset, symbol)
      @level = level
      @offset = offset
      @symbol = symbol
    end

    def inspect
      '#%p:%p%p' % [@level, @offset, @symbol]
    end

    def set_value(x, link)
      @level.times {link = link.cdr}
      link.car[@offset] = x
    end

    def get_value(link)
      @level.times {link = link.cdr}
      return link.car[@offset]
    end
  end # Arg


  class Dummy                  # コンパイル後のマクロ式の dummy symbol
    attr :symbol

    def initialize(symbol)
      @symbol = symbol
    end

    def inspect
      '%p:%x' % [@symbol, object_id]
    end
  end # Dummy


  class Promise                 # 約束: (delay exp) の評価結果
    def initialize(exp, link, interp)
      @exp = exp
      @link = link              # 環境 (約束をかなえたら NONE にする)
      @interp = interp
    end

    def inspect
      (@link == NONE) ? @exp.inspect : '#<promise:%x>' % object_id
    end

    def value
      (@link == NONE) ? @exp : self
    end

    def deliver                 # 約束をかなえる
      unless @link == NONE
        old_env = @interp.environ
        @interp.environ = @link
        begin
          x = @interp.eval(@exp, true)
          x = x.deliver if Promise === x
        ensure
          @interp.environ = old_env
        end
        unless @link == NONE    # eval の中でかなえられていなければ…
          @exp = x
          @link = NONE
        end
      end
      return @exp
    end
  end # Promise


  # 式を読む
  class Reader
    def initialize(rf)
      @rf = rf                  # 入力ファイル: IO
      @buf = []                 # 入力行から得たトークンの並び
      @line = nil               # 入力行: String または nil
    end

    def read
      begin
        _read_token
        return _parse_expression
      rescue SyntaxError => ex
        @buf.clear    # その行の残りのトークンを捨てて次回の回復を図る
        raise EvalError, 'SyntaxError: %s -- %d: %p' % [ex, @rf.lineno, @line]
      ensure
        @rf.reset if InteractiveInput === @rf
      end
    end

    RPAREN = :')'
    DOT = :'.'

    def _parse_expression
      case @token
      when DOT, RPAREN
        raise SyntaxError, 'unexpected: %s' % @token
      when :'('
        _read_token
        return _parse_list_body
      when :'\''
        _read_token
        return Cell.new(:quote, Cell.new(_parse_expression, nil))
      when :'~'
        _read_token
        return Cell.new(:delay, Cell.new(_parse_expression, nil))
      when :'`'
        _read_token
        return QQ.expand_qq(_parse_expression)
      when :','
        _read_token
        return QQ::Unquote.new(_parse_expression)
      when :',@'
        _read_token
        return QQ::UnquoteSplicing.new(_parse_expression)
      else
        return @token
      end
    end

    def _parse_list_body
      case @token
      when S_EOF
        raise SyntaxError, 'unexpected EOF'
      when RPAREN
        return nil
      else
        e1 = _parse_expression
        _read_token
        if @token == DOT
          _read_token
          (@token != S_EOF) or raise SyntaxError, 'unexpected EOF'
          e2 = _parse_expression
          _read_token
          (@token == RPAREN) or raise SyntaxError, '")" expected: %s' % @token
        else
          e2 = _parse_list_body
        end
        return Cell.new(e1, e2)
      end
    end

    def _read_token
      while @buf.empty?
        @line = @rf.gets
        if @line.nil?
          @token = S_EOF
          @rf.close
          return
        end
        @buf = @line.chomp.scan(TOKEN_PAT).flatten.compact
      end
      case t = @buf.shift
      when '(', ')', '.', '\'', '~', '`', ',', ',@'
        @token = t.to_sym
      when 'nil'
        @token = nil
      when 't'
        @token = true
      when /\A\".*\"\z/
        @token = eval(t, TOPLEVEL_BINDING)
      else
        if t[0] == ?-
          if t.length >= 2 and ?0 <= t[1] and t[1] <= ?9
            begin
              @token = - _to_int('0d' + t[1..-1])
            rescue ArgumentError
            else
              return
            end
          end
        else
          radix = '0d'
          offset = 0
          if t[0] == ?# and t.length >= 3
            case t[1]
            when ?b, ?B
              radix = '0b'
              offset = 2
            when ?o, ?O
              radix = '0o'
              offset = 2
            when ?x, ?X
              radix = '0x'
              offset = 2
            end
          end
          begin
            @token = _to_int(radix + t[offset..-1])
          rescue ArgumentError
          else
            return
          end
        end
        begin
          @token = _to_float(t)
        rescue ArgumentError
          if /\A([A-Za-z0-9]|_|&|\$|\*|\/|%|\+|-|<|>|=|!|\?)+\z/ === t
            @token = t.to_sym
          else
            raise SyntaxError, 'bad token: %p' % t
          end
        end
      end
    end

    def _to_int(s)              # Emacs Lisp にならい _ を数に含めない
        s.include?(?_) and raise ArgumentError
        return Integer(s)
    end

    def _to_float(s)
        s.include?(?_) and raise ArgumentError
        return Float(s)
    end

    TOKEN_PAT = /\s+|;.*$|(".*?"|,@?|[^()'`~ ]+|.)/
  end # Reader


  # 式を解釈する
  class Interp
    attr_accessor :environ      # 環境
    attr :symbol                # シンボルからその大域値への写像
    attr :lazy                  # 引数を force しない組込み関数の集合 (Set
                                # ではなく Hash で代用)
    alias ruby_eval eval

    def initialize
      @reader = Reader.new(InteractiveInput.new('', ''))
      @environ = nil
      @symbol = {}
      @lazy = {}
      def @symbol.inspect
        LL.str(LL.list(*keys))
      end
      @symbol[:'*version*'] = LL.list(7.3, 'Ruby')
      @symbol[:'*eof*'] = S_EOF
      @symbol[S_ERROR] = S_ERROR
      @symbol[:car] = proc {|x| (x.nil?) ? nil : x.car}
      @symbol[:cdr] = proc {|x| (x.nil?) ? nil : x.cdr}
      @lazy[@symbol[:cons] = proc {|x, y| Cell.new(x, y)}] = true
      @symbol[:atom] = proc {|x| !(Cell === x) || nil}
      @symbol[:stringp] = proc {|x| (String === x) || nil}
      @symbol[:numberp] = proc {|x| (Numeric === x) || nil}
      @symbol[:eq] = proc {|x, y| x.equal?(y) || nil}
      @symbol[:eql] = proc {|x, y| x.eql?(y) || nil}
      @lazy[@symbol[:list] = proc {|*x| LL.list(*x)}] = true
      @symbol[:prin1] = proc {|x| print(LL.str(x, true)); x}
      @symbol[:princ] = proc {|x| print(LL.str(x, false)); x}
      @symbol[:terpri] = proc {print("\n"); true}
      @symbol[:read] = proc {@reader.read}
      @symbol[:'+'] = proc {|*x| x.inject(0) {|a, b| a + b}}
      @symbol[:'*'] = proc {|*x| x.inject(1) {|a, b| a * b}}
      @symbol[:'/'] = proc {|x, *y| y.inject(x) {|a, b| a / b}}
      @symbol[:'%'] = proc {|x, y| x % y}
      @symbol[:'-'] = proc {|x, *y|
        (y.length == 0) ? -x : y.inject(x) {|a, b| a - b}
      }
      @symbol[:'<'] = proc {|x, y| (x < y) || nil}
      @symbol[:load] = proc {|file_name| run(File.new(file_name))}
      @symbol[:eval] = proc {|x|
        old_env = @environ
        @environ = nil          # 大域的な環境にする
        begin
          eval(x, true)         # 末尾呼出しと同じく環境復元は不要
        ensure
          @environ = old_env
        end
      }
      @symbol[:apply] = proc {|fun, arg| apply(fun, arg.to_a)}
      @symbol[:force] = proc {|x| x}
      @symbol[:rplaca] = proc {|x, y| x.car = y}
      @symbol[:rplacd] = proc {|x, y| x.cdr = y}
      @symbol[:throw] = proc {|x, y| raise Thrown.new(x, y)}
      @symbol[:mapcar] = proc {|fun, x| LL.mapcar(x) {|e| apply(fun, [e])}}
      @symbol[:mapc] = proc {|fun, x|
        case x
        when String
          x.each_byte {|e| apply(fun, [e])}
        when nil
        else
          x.each {|e| apply(fun, [e])}
        end
        x
      }
      @symbol[:length] = proc {|x| (x.nil?) ? 0 : x.length}
      @symbol[:_add] = proc {|x, y| x + y} # (concat ...) 実装用
      @symbol[:_concat] = proc {|x|        # 同上
        case x
        when String
          x
        when nil
          ""
        else
          x.to_a.pack("C*")
        end
      }
      @symbol[:'ruby-eval'] = proc {|expression|
        ruby_eval(expression, TOPLEVEL_BINDING)
      }
      @symbol[:'ruby-send'] = proc {|receiver, selector, *parameters|
        receiver.__send__(selector, *parameters)
      }
      @symbol[:'ruby-send-apply'] = proc {|rcv, sel, fun, *params|
        rcv.__send__(sel, *params) {|arg| apply(fun, arg)}
      }
      @symbol[:'ruby-self'] = proc {self}
      @symbol[:dump] = proc {LL.list(LL.list(*@symbol.keys), @environ)}
      @symbol[:help] = proc {puts SUMMARY}
      @symbol[:prelude] = proc {puts PRELUDE}
      @symbol[:copyright] = proc {puts COPYRIGHT}
      run(PRELUDE)
    end

    def eval(x, can_lose_current_env=false)
      begin
        loop {
          case x
          when Symbol
            return eval_symbol(x)
          when Arg
            return x.get_value(@environ)
          when Cell
            case x.car
            when :quote
              return x.cdr.car
            when :progn
              x = eval_progn_body(x.cdr)
            when :cond
              x, cont = eval_cond_body(x.cdr)
              return x unless cont
            when :setq
              return eval_setq_body(x.cdr)
            when :lambda
              return CLOSURE.new(@environ, *compile(x.cdr))
            when :macro
              @environ.nil? or raise EvalError.new('nested macro', x)
              y = replace_dummy_variables(x.cdr, {})
              return MACRO.new(*compile(y))
            when :catch
              return eval_catch_body(x.cdr)
            when :'unwind-protect'
              return eval_unwind_protect_body(x.cdr)
            when :delay
              kdr = x.cdr
              (Cell === kdr and kdr.cdr.nil?) or raise EvalError, 'bad delay'
              return Promise.new(kdr.car, @environ, self)
            else
              fn = x.car
              case fn      # 高速化のため，eval を簡単にここに展開する
              when Symbol
                fn = eval_symbol(fn)
              when Arg
                fn = fn.get_value(@environ)
              when Cell
                fn = eval(fn)
              when LAMBDA
                fn = CLOSURE.new(@environ, fn.arity, fn.body)
              end
              fn = fn.deliver if Promise === fn
              case fn
              when CLOSURE
                args = get_args(x.cdr, false)
                x, cont = apply_function(fn, args, can_lose_current_env)
                return x unless cont
              when MACRO
                x, cont = apply_function(fn, x.cdr.to_a)
              when Proc
                args = get_args(x.cdr, (not @lazy.has_key? fn))
                begin
                  return fn.call(*args)
                rescue EvalError
                  raise
                rescue => e
                  raise EvalError, "%s: %s -- %p %p" % [e.class, e, fn, args]
                end
              else
                raise EvalError.new("not applicable", fn)
              end
            end
          when LAMBDA
            return CLOSURE.new(@environ, x.arity, x.body)
          else 
            return x
          end
        }
      rescue EvalError => ex
        ex.trace << LL.str(x) if ex.trace.length < MAX_EXC_TRACES
        raise
      end
    end

    def apply(fn, args)         # 関数適用のための便宜関数
      case fn
      when CLOSURE, MACRO
        return apply_function(fn, args)[0]
      when Proc
        unless @lazy.has_key? fn
          args = args.collect {|e| (Promise === e) ? e.deliver : e}
        end
        return fn.call(*args)
      else
        raise EvalError.new('not applicable', fn)
      end
    end

    def eval_symbol(name)
      begin
        return @symbol.fetch(name)
      rescue IndexError
        raise EvalError.new('void variable', name)
      end
    end

    def eval_progn_body(body)
      if Cell === body
        while Cell === (d = body.cdr)
          eval(body.car)
          body = d
        end
        d.nil? or raise ProperListExpected, d
        return body.car         # 末尾呼出し ⇒ 戻った先で評価する
      else
        body.nil? or raise ProperListExpected, body
        return nil
      end
    end

    def eval_cond_body(body)
      while Cell === body       # 高速化のためベタな while を使う
        case clause = body.car
        when Cell
          result = eval(clause.car)
          result = result.deliver if Promise === result
          if result != nil      # テスト結果が真ならば
            clause = clause.cdr
            return result, false unless Cell === clause
            while Cell === (d = clause.cdr)
              eval(clause.car)
              clause = d
            end
            d.nil? or raise ProperListExpected, d
            return clause.car, true # 末尾呼出し ⇒ 戻った先で評価する
          end
        when nil
        else
          raise EvalError.new('cond test expected', clause)
        end
        body = body.cdr
      end
      body.nil? or raise ProperListExpected, body
      return nil, false         # すべて失敗ならば nil
    end

    def eval_setq_body(body)    # (LVAL RVAL LVAL RVAL...)
      result = nil
      while Cell === body
        lval = body.car
        body = body.cdr
        (Cell === body) or raise EvalError, 'right value expected'
        result = eval(body.car)
        case lval
        when Symbol
          @symbol[lval] = result
        when Arg
          lval.set_value(result, @environ)
        else
          raise VariableExpected, lval
        end
        body = body.cdr
      end
      body.nil? or raise ProperListExpected, body
      return result
    end

    # リストを評価して (フラグが真なら force して) Array にする
    def get_args(list, flag)
      args = []
      while Cell === list
        x = eval(list.car)
        if flag and Promise === x
          x = x.deliver 
        end
        args << x
        list = list.cdr
      end
      list.nil? or raise ProperListExpected, list
      return args
    end

    # fn は CLOSURE か MACRO のインスタンス
    def apply_function(fn, args, can_lose_original_env=false)
      body = fn.body
      (Cell === body) or raise EvalError, 'body expected'
      arity = fn.arity
      link = fn.env
      if arity < 0              # &rest 付きならば
        arity = -arity -1       # &rest より前の引数の個数を得て
        if arity <= args.length # rest 引数を１個のリストに構成する
          rest = args.slice!(arity .. -1)
          args << LL.list(*rest)
          arity += 1
        end
      end
      (args.length == arity) or raise EvalError, 'arity not matched'
      old_env = @environ              # 元の環境を退避する
      @environ = Cell.new(args, link) # 新環境に変更する
      begin
        while Cell === (d = body.cdr)
          eval(body.car)
          body = d
        end
        if can_lose_original_env      # ⇒ (典型的には) 末尾呼出し
          old_env = @environ          # 新環境のまま
          return body.car, true       # 戻った先で評価する
        else
          return eval(body.car, true), false
        end
      ensure
        @environ = old_env
      end
    end

    # $ ではじまるシンボルを Dummy に置き換える
    def replace_dummy_variables(j, names)
      case j
      when Symbol
        if j.to_s[0] == ?$
          k = names[j]
          if k.nil?
            names[j] = k = Dummy.new(j)
          end
          return k
        else
          return j
        end
      when Cell
        return LL.mapcar(j) {|x| replace_dummy_variables(x, names)}
      else
        return j
      end
    end

    def compile(j)
      (Cell === j) or raise EvalError, 'arglist and body expected'
      rest, table = make_arg_table2(j.car)
      arity = table.length
      arity = -arity if rest
      (Cell === j.cdr) or raise EvalError, 'body expected'
      body = scan2(j.cdr, table)
      body = expand_macros2(body, MAX_MACRO_EXPS)
      body = compile_inners2(body)
      return arity, body
    end

    def compile_inners2(j)
      if Cell === j
        case j.car
        when :quote
          return j
        when :lambda
          return LAMBDA.new(*compile(j.cdr))
        when :macro
          raise EvalError.new('nested macro', j)
        else
          return LL.mapcar(j) {|x| compile_inners2(x)}
        end
      else
        return j
      end
    end

    def make_arg_table2(i)      # 仮引数並び -> rest?, table
      offset = 0
      rest = false
      table = {}
      while Cell === i
        j = i.car
        (not rest) or raise EvalError.new('2nd rest', j)
        if j == S_REST          # &rest rest_arg
          i = i.cdr
          (Cell === i) or raise VariableExpected, i
          j = i.car
          (j != S_REST) or raise VariableExpected, j
          rest = true
        end
        case j
        when Symbol
          sym = j
        when Arg
          sym = j = j.symbol
        when Dummy
          sym = j.symbol
        else
          raise VariableExpected, j
        end
        table[j] = Arg.new(0, offset, sym)
        offset += 1
        i = i.cdr
      end
      i.nil? or raise ProperListExpected, i
      return rest, table
    end

    def scan2(j, table)
      case j
      when Symbol, Dummy
        k = table[j]
        return (k.nil?) ? j : k
      when Arg
        k = table[j.symbol]
        return (k.nil?) ? Arg.new(j.level + 1, j.offset, j.symbol) : k
      when Cell
        return (j.car == :quote) ? j : LL.mapcar(j) {|x| scan2(x, table)}
      else
        return j
      end
    end

    def expand_macros2(j, count)
      if count > 0 and Cell === j
        case k = j.car
        when :quote, :lambda, :macro
          return j
        else
          k = @symbol.fetch(k, k) if Symbol === k
          if MACRO === k
            z, c = apply_function(k, j.cdr.to_a)
            return expand_macros2(z, count - 1)
          else
            return LL.mapcar(j) {|x| expand_macros2(x, count)}
          end
        end
      else
        return j
      end
    end

    def eval_catch_body(j)      # j = (tag body...)
      (Cell === j) or raise EvalError.new('tag and body expected', j)
      tag = eval(j.car)
      begin
        result = nil
        case k = j.cdr
        when Cell
          k.each {|x| result = eval(x)}
        when nil
        else
          raise ProperListExpected, k
        end
        return result
      rescue Thrown => th
        return th.value if tag == th.tag
        raise
      rescue EvalError => ex    # 一般の評価時例外の捕捉
        return ex if tag == S_ERROR
        raise
      end
    end

    def eval_unwind_protect_body(j) # j = (body cleanup...)
      (Cell === j) or raise EvalError.new('body (and cleanup) expected', j)
      begin
        return eval(j.car)
      ensure
        case k = j.cdr
        when Cell
          k.each {|x| eval(x)}
        when nil
        else
          raise ProperListExpected, k
        end
      end
    end

    # IO または String から式の並びを読んで評価する。
    # 無引数ならば対話的に入力/評価/出力を繰り返す。
    def run(rf=nil)
      if interactive = rf.nil?
        rf = InteractiveInput.new('> ', '  ')
      elsif String === rf
        rf = StringIO.new(rf)
      end
      rr = Reader.new(rf)
      result = nil
      loop {
        begin
          x = rr.read
          if x == S_EOF
            puts 'Goodbye' if interactive
            return result
          end
          result = eval(x)
          puts LL.str(result) if interactive
        rescue Interrupt => ex # (典型的には) Control-C が打鍵されたとき
          raise unless interactive
          puts "\n" + ex.inspect
        rescue EvalError => ex
          raise unless interactive
          print ex
        end
      }
    end
  end # Interp


  # プロンプト付き標準入力クラス
  # (可能ならば行編集機能付き。readline のロードの成否で場合分けを行う)

  # 場合分けの共通部分をになう基底クラス
  class InteractiveInputBase
    attr :lineno                # 現在の行番号

    def initialize(ps1, ps2)    # 引数は１次プロンプトと２次プロンプト
      @ps1 = ps1
      @ps2 = ps2
      @primary = true
      @lineno = 0
    end

    def reset                   # プロンプトを１次に戻す
      @primary = true
    end

    def close                   # 何もしない
    end
  end # InteractiveInputBase


  begin
    require 'readline'
  rescue LoadError
    # ロード失敗 → 行編集機能なしのクラス定義
    class InteractiveInput < InteractiveInputBase
      def gets        # 初回は１次，以降２次プロンプトで１行を入力する
        if @primary
          STDOUT.print @ps1
          @primary = false
        else
          STDOUT.print @ps2
        end
        STDOUT.flush
        @lineno += 1
        return STDIN.gets
      end
    end # InteractiveInput
  else
    # ロード成功 → 行編集機能ありのクラス定義
    class InteractiveInput < InteractiveInputBase
      def gets
        if @primary
          prompt = @ps1
          @primary = false
        else
          prompt = @ps2
        end
        @lineno += 1
        s = Readline.readline(prompt, true)
        return (s.nil?) ? nil : s + "\n"
      end
    end # InteractiveInput
  end


  # 初期化 Lisp スクリプト
  PRELUDE = %q{
(setq defmacro
      (macro (name args &rest body)
             `(progn (setq ,name (macro ,args ,@body))
                     ',name)))

(defmacro defun (name args &rest body)
  `(progn (setq ,name (lambda ,args ,@body))
          ',name))

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))
(defun caaar (x) (car (car (car x))))
(defun caadr (x) (car (car (cdr x))))
(defun cadar (x) (car (cdr (car x))))
(defun caddr (x) (car (cdr (cdr x))))
(defun cdaar (x) (cdr (car (car x))))
(defun cdadr (x) (cdr (car (cdr x))))
(defun cddar (x) (cdr (cdr (car x))))
(defun cdddr (x) (cdr (cdr (cdr x))))
(defun not (x) (eq x nil))
(defun consp (x) (not (atom x)))
(defun print (x) (prin1 x) (terpri) x)
(defun identity (x) x)

(setq
 = eql
 null not
 setcar rplaca
 setcdr rplacd)

(defun > (x y) (< y x))
(defun >= (x y) (not (< x y)))
(defun <= (x y) (not (< y x)))
(defun /= (x y) (not (= x y)))

(defun equal (x y)
  (cond ((atom x) (eql x y))
        ((atom y) nil)
        ((equal (car x) (car y)) (equal (cdr x) (cdr y)))))

(defun concat (&rest x)
  (cond ((null x) "")
        ((null (cdr x)) (_concat (car x)))
        (t (_add (_concat (car x))
                 (apply concat (cdr x))))))

(defmacro if (test then &rest else)
  `(cond (,test ,then)
         ,@(cond (else `((t ,@else))))))

(defmacro when (test &rest body)
  `(cond (,test ,@body)))

(defmacro let (args &rest body)
  ((lambda (vars vals)
     (defun vars (x)
       (cond (x (cons (if (atom (car x))
                          (car x)
                        (caar x))
                      (vars (cdr x))))))
     (defun vals (x)
       (cond (x (cons (if (atom (car x))
                          nil
                        (cadar x))
                      (vals (cdr x))))))
     `((lambda ,(vars args) ,@body) ,@(vals args)))
   nil nil))

(defun _append (x y)
  (if (null x)
      y
    (cons (car x) (_append (cdr x) y))))
(defmacro append (x &rest y)
  (if (null y)
      x
    `(_append ,x (append ,@y))))

(defmacro and (x &rest y)
  (if (null y)
      x
    `(cond (,x (and ,@y)))))

(defmacro or (x &rest y)
  (if (null y)
      x
    `(cond (,x)
           ((or ,@y)))))

(defun listp (x)
  (or (null x) (consp x)))    ; NB (listp (lambda (x) (+ x 1))) => nil

(defun memq (key x)
  (cond ((null x) nil)
        ((eq key (car x)) x)
        (t (memq key (cdr x)))))

(defun member (key x)
  (cond ((null x) nil)
        ((equal key (car x)) x)
        (t (member key (cdr x)))))

(defun assq (key alist)
  (cond (alist (let ((e (car alist)))
                 (if (and (consp e) (eq key (car e)))
                     e
                   (assq key (cdr alist)))))))

(defun assoc (key alist)
  (cond (alist (let ((e (car alist)))
                 (if (and (consp e) (equal key (car e)))
                     e
                   (assoc key (cdr alist)))))))

(defun _nreverse (x prev)
  (let ((next (cdr x)))
    (setcdr x prev)
    (if (null next)
        x
      (_nreverse next x))))
(defun nreverse (list)            ; (nreverse '(a b c d)) => (d c b a)
  (cond (list (_nreverse list nil))))

(defun last (list)
  (if (atom (cdr list))
      list
    (last (cdr list))))

(defun nconc (&rest lists)
  (if (null (cdr lists))
      (car lists)
    (setcdr (last (car lists))
            (apply nconc (cdr lists)))
    (car lists)))

(defmacro push (newelt listname)
  `(setq ,listname (cons ,newelt ,listname)))

(defmacro pop (listname)
  `(let (($a (car ,listname)))
     (setq ,listname (cdr ,listname))
     $a))

(defmacro while (test &rest body)
  `(let ($loop)
     (setq $loop (lambda () (cond (,test ,@body ($loop)))))
     ($loop)))

(defun nth (n list)
  (while (< 0 n)
    (setq list (cdr list)
          n (- n 1)))
  (car list))

(defmacro dolist (spec &rest body) ; (dolist (name list [result]) body...)
  (let ((name (car spec)))
    `(let (,name
           ($list ,(cadr spec)))
       (while $list
         (setq ,name (car $list))
         ,@body
         (setq $list (cdr $list)))
       ,@(if (cddr spec)
             `((setq ,name nil)
               ,(caddr spec))))))

(defmacro dotimes (spec &rest body) ; (dotimes (name count [result]) body...)
  (let ((name (car spec)))
    `(let ((,name 0)
           ($count ,(cadr spec)))
       (while (< ,name $count)
         ,@body
         (setq ,name (+ ,name 1)))
       ,@(if (cddr spec)
             `(,(caddr spec))))))

(defun reduce (f x)
  (if (null x)
      (f)
    (let ((r (car x)))
      (setq x (cdr x))
      (while x
        (setq r (f r (car x))
              x (cdr x)))
      r)))

(defun some (f x)
  (cond ((null x) nil)
        ((f (car x)))
        (t (some f (cdr x)))))

(defun take (n x)                       ; Haskell
  (if (or (= 0 n) (null x))
      nil
    (cons (car x) (take (- n 1) (cdr x)))))

(defun drop (n x)                       ; Haskell
  (if (or (= 0 n) (null x))
      x
    (drop (- n 1) (cdr x))))

(defun _zip (x)
  (if (some null x)
      nil
    (let ((cars (mapcar car x))
          (cdrs (mapcar cdr x)))
      (cons cars ~(_zip cdrs)))))
(defun zip (&rest x) (_zip x))          ; Python 3.0 & Haskell

(defun range (m n)                      ; Python 3.0
  (cond ((< m n) (cons m ~(range (+ m 1) n)))))

(defun map (f x)                        ; Haskell
  (cond (x (cons ~(f (car x)) ~(map f (cdr x))))))

(defun mapf (f x)                       ; map force
  (cond (x (cons (f (car x)) ~(map f (cdr x))))))

(defun scanl (f q x)                    ; Haskell
  (cons q ~(cond (x (scanl f (f q (car x)) (cdr x))))))

(defun filter (f x)                     ; Haskell & Python 3.0
  (cond ((null x) nil)
        ((f (car x)) (cons (car x) ~(filter f (cdr x))))
        (t (filter f (cdr x)))))

(setq STDOUT (ruby-eval "STDOUT"))
(defun printf (str &rest args)
  (apply ruby-send `(,STDOUT printf ,str ,@args)))

(defun exit (n)
  (ruby-send (ruby-self) 'exit n))
}
end # L2Lisp


if __FILE__ == $0               # 主ルーチン
  interp = L2Lisp::Interp.new
  if ARGV.empty?
    interp.run()
  else
    ARGV.each {|file_name|
      if file_name == '-'
        interp.run()
      else
        interp.run(File.new(file_name))
      end
    }
  end
end
