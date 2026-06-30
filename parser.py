from typing import Callable, Any, Tuple, Optional, List
import re

# Rustの either::Either を再現するための簡易クラス
class Left:
    def __init__(self, value: Any):
        self.value = value
    def __repr__(self): return f"Left({self.value})"
    def __eq__(self, other): return isinstance(other, Left) and self.value == other.value

class Right:
    def __init__(self, value: Any):
        self.value = value
    def __repr__(self): return f"Right({self.value})"
    def __eq__(self, other): return isinstance(other, Right) and self.value == other.value


class Parser:
    def __init__(self, f: Callable[[str], Optional[Tuple[Any, str]]]):
        self.f = f

    def run(self, src: str) -> Optional[Tuple[Any, str]]:
        return self.f(src)

    # Instance of Functor
    def map(self, f: Callable[[Any], Any]) -> 'Parser':
        return Parser(lambda s: (res := self.run(s)) and (f(res[0]), res[1]))

    # Instance of Monad
    def and_then(self, f: Callable[[Any], 'Parser']) -> 'Parser':
        return Parser(lambda s: (res := self.run(s)) and f(res[0]).run(res[1]))

    # Instance of Applicative
    @staticmethod
    def pure(x: Any) -> 'Parser':
        return Parser(lambda s: (x, s))

    def ap(self, pf: 'Parser') -> 'Parser':
        def parse(s: str):
            res_f = pf.run(s)
            if not res_f: return None
            func, s1 = res_f
            res_a = self.run(s1)
            if not res_a: return None
            a, s2 = res_a
            return func(a), s2
        return Parser(parse)

    # Instance of Alternative
    @staticmethod
    def empty() -> 'Parser':
        return Parser(lambda _: None)

    def or_else(self, other: 'Parser') -> 'Parser':
        return Parser(lambda s: self.run(s) or other.run(s))

    def or_either(self, other: 'Parser') -> 'Parser':
        def parse(s: str):
            res = self.run(s)
            if res: return Left(res[0]), res[1]
            res_other = other.run(s)
            if res_other: return Right(res_other[0]), res_other[1]
            return None
        return Parser(parse)

    def _many_helper(self, s: str) -> Tuple[List[Any], str]:
        xs = []
        while True:
            res = self.run(s)
            if not res: break
            x, rest = res
            if len(rest) == len(s):  # 無限ループ防止ガード
                break
            xs.append(x)
            s = rest
        return xs, s

    def many(self) -> 'Parser':
        return Parser(lambda s: self._many_helper(s))

    def many1(self) -> 'Parser':
        def parse(s: str):
            xs, rest = self._many_helper(s)
            return (xs, rest) if xs else None
        return Parser(parse)

    def optional(self) -> 'Parser':
        return self.map(lambda x: x).or_else(Parser(lambda s: (None, s)))

    def _sep_by_helper(self, sep: 'Parser', s: str) -> Optional[Tuple[List[Any], str]]:
        xs = []
        res = self.run(s)
        if not res:
            return [], s
        x, rest = res
        xs.append(x)
        s = rest

        while True:
            res_sep = sep.run(s)
            if not res_sep: break
            _, rest_sep = res_sep
            s = rest_sep
            res_val = self.run(s)
            if not res_val:
                return None
            x, rest_val = res_val
            xs.append(x)
            s = rest_val
        return xs, s

    def sep_by(self, sep: 'Parser') -> 'Parser':
        return Parser(lambda s: self._sep_by_helper(sep, s))

    def sep_by1(self, sep: 'Parser') -> 'Parser':
        def parse(s: str):
            res = self._sep_by_helper(sep, s)
            return res if (res is not None and len(res[0]) > 0) else None
        return Parser(parse)

    def chainl1(self, op: 'Parser') -> 'Parser':
        def parse(s: str):
            res = self.run(s)
            if not res: return None
            acc, s = res

            while True:
                res_op = op.run(s)
                if not res_op: break
                f, rest = res_op
                s = rest
                res_val = self.run(s)
                if not res_val: return None
                a, rest = res_val
                acc = f(acc, a)
                s = rest
            return acc, s
        return Parser(parse)

    def _chainr1_helper(self, op: 'Parser', s: str) -> Optional[Tuple[Any, str]]:
        res = self.run(s)
        if not res: return None
        a, s = res

        res_op = op.run(s)
        if res_op:
            f, rest = res_op
            s = rest
            next_res = self._chainr1_helper(op, s)
            if next_res:
                acc, s = next_res
                return f(a, acc), s
            return None
        else:
            return a, s

    def chainr1(self, op: 'Parser') -> 'Parser':
        return Parser(lambda s: self._chainr1_helper(op, s))

    def lexeme(self) -> 'Parser':
        return (self & whitespace()).map(lambda pair: pair[0])

    # 演算子オーバーロード
    def __or__(self, rhs: 'Parser') -> 'Parser':
        return self.or_either(rhs)

    def __and__(self, rhs: 'Parser') -> 'Parser':
        def parse(s: str):
            res1 = self.run(s)
            if not res1: return None
            a, rest1 = res1
            res2 = rhs.run(rest1)
            if not res2: return None
            b, rest2 = res2
            return (a, b), rest2
        return Parser(parse)

    def __add__(self, rhs: 'Parser') -> 'Parser':
        return self.or_else(rhs)

    def __mul__(self, rhs: 'Parser') -> 'Parser':
        # Rustの複数トレイト実装を動的型チェックで一本化
        def parse(s: str):
            res = (self & rhs).run(s)
            if not res: return None
            (a, b), rest = res
            if isinstance(a, list):
                return a + [b], rest
            return [a, b], rest
        return Parser(parse)

    def __rshift__(self, rhs: 'Parser') -> 'Parser':
        return (self & rhs).map(lambda pair: pair[1])

    def __lshift__(self, rhs: 'Parser') -> 'Parser':
        return (self & rhs).map(lambda pair: pair[0])


# ユーティリティ関数（基本パーサ群）

def lazy(f: Callable[[], Parser]) -> Parser:
    return Parser(lambda s: f().run(s))

def satisfy(f: Callable[[str], bool]) -> Parser:
    def parse(s: str):
        if s and f(s[0]):
            return s[0], s[1:]
        return None
    return Parser(parse)

def charp(c: str) -> Parser:
    return satisfy(lambda x: x == c)

def digit() -> Parser:
    return satisfy(lambda c: c.isdigit())

def alpha() -> Parser:
    return satisfy(lambda c: c.isalpha())

def alphanum() -> Parser:
    return satisfy(lambda c: c.isalnum())

def number() -> Parser:
    return digit().many1().map(lambda xs: int("".join(xs)))

def string(expected: str) -> Parser:
    def parse(s: str):
        if s.startswith(expected):
            return expected, s[len(expected):]
        return None
    return Parser(parse)

def any_char() -> Parser:
    return satisfy(lambda _: True)

def one_of(chars: List[str]) -> Parser:
    return satisfy(lambda c: c in chars)

def whitespace() -> Parser:
    # バックスラッシュエスケープの修正を適用済み
    return one_of([' ', '\t', '\r', '\n']).many().map(lambda _: None)

def epsilon() -> Parser:
    return Parser(lambda s: (None, s))

def regex(pattern: str) -> Parser:
    # re.compile して re.match (必ず先頭からマッチ) を用いることで安全に
    compiled = re.compile(pattern)
    def parse(s: str):
        match = compiled.match(s)
        if match:
            return match.group(0), s[match.end():]
        return None
    return Parser(parse)

def identifier() -> Parser:
    return regex(r"[a-zA-Z_][a-zA-Z0-9_]*")

# Test
# 数式パーサの定義 (Rust版の実装を移植)
def expr() -> Parser:
    def op_func(op):
        return (lambda a, b: a + b) if isinstance(op, Left) else (lambda a, b: a - b)
    return term().chainl1(
        (charp('+') | charp('-')).lexeme().map(op_func)
    )

def term() -> Parser:
    def op_func(op):
        return (lambda a, b: a * b) if isinstance(op, Left) else (lambda a, b: a // b)
    return primitive().chainl1(
        (charp('*') | charp('/')).lexeme().map(op_func)
    )

def primitive() -> Parser:
    return number().lexeme() + (charp('(').lexeme() >> lazy(expr) << charp(')').lexeme())


if __name__ == "__main__":
    # 1. 基本機能テスト
    assert charp('a').map(lambda c: c.upper()).run("abc") == ('A', "bc")
    assert Parser.pure(42).run("input") == (42, "input")
    assert (charp('a') + charp('b')).run("bcd") == ('b', "cd")

    # 2. 演算子によるリストの畳み込み (*)
    assert (charp('a') * charp('b') * charp('c')).run("abcd") == (['a', 'b', 'c'], 'd')

    # 3. 繰り返しと区切り文字
    assert number().sep_by(alpha()).run("1a2b3_") == ([1, 2, 3], "_")

    # 4. レクセムと空白の自動スキップ
    assert number().lexeme().run("334   rest") == (334, "rest")

    # 5. 正規表現・識別子
    assert identifier().run("x_1=0") == ("x_1", "=0")
    assert identifier().run("0_1=0") is None

    # 6. 数式評価パーサの実戦テスト
    calc = expr()
    assert calc.run("3 + 4 * 2") == (11, "")
    assert calc.run("( 3 + 4 ) * 2") == (14, "")
    assert calc.run("6 / 3 - 1") == (1, "")
    assert calc.run("1 - 3 + 1") == (-1, "")

    print("すべてのテストに合格しました！ Perfect!")
