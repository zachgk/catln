"""A Python file testing the Catln Python parser."""


def add(x: int, y: int) -> int:
    result: int = x + y
    return result


def square(x: float) -> float:
    return x * x


class Point:
    pass


PI: float = 3.14159
MAX_SIZE: int = 100

# Phase 1: numeric literals now produce CInt/CFloat, not CStr
DOUBLE_PI: float = PI * 2.0
BIG: int = MAX_SIZE + MAX_SIZE

# Phase 1: list, dict, set, tuple type annotations map to Catln collection types
def scale(values: list, factor: int) -> list:
    pass


# Phase 2: abs, pow, round, floor, ceil, integer //
def abs_int(x: int) -> int:
    return abs(x)


def abs_float(x: float) -> float:
    return abs(x)


def power(base: int, exp: int) -> int:
    return pow(base, exp)


def power_f(base: float, exp: float) -> float:
    return pow(base, exp)


def rounded(x: float) -> int:
    return round(x)


def floored(x: float) -> int:
    return floor(x)


def ceiled(x: float) -> int:
    return ceil(x)


def int_div(a: int, b: int) -> int:
    return a // b


# Phase 3: type conversions
def to_int_from_float(x: float) -> int:
    return int(x)


def to_float_from_int(x: int) -> float:
    return float(x)


def to_str_from_int(x: int) -> str:
    return str(x)


def to_str_from_float(x: float) -> str:
    return str(x)


def to_bool_from_int(x: int) -> bool:
    return bool(x)


def to_bool_from_float(x: float) -> bool:
    return bool(x)


# Phase 4: string operations
def char_from_code(n: int) -> str:
    return chr(n)


def code_from_char(c: str) -> int:
    return ord(c)


def to_hex(n: int) -> str:
    return hex(n)


def to_oct(n: int) -> str:
    return oct(n)


def to_bin(n: int) -> str:
    return bin(n)


def concat_strings(a: str, b: str) -> str:
    return a + b


def string_length(s: str) -> int:
    return len(s)


# Phase 5: numeric min/max
def int_max(a: int, b: int) -> int:
    return max(a, b)


def float_max(a: float, b: float) -> float:
    return max(a, b)


def int_min(a: int, b: int) -> int:
    return min(a, b)


def float_min(a: float, b: float) -> float:
    return min(a, b)


# Collection operations
def sum_list(nums: list) -> int:
    return sum(nums)


def all_true(flags: list) -> bool:
    return all(flags)


def any_true(flags: list) -> bool:
    return any(flags)


def make_range(n: int) -> list:
    return range(n)


def reverse_list(lst: list) -> list:
    return reversed(lst)


def sort_list(lst: list) -> list:
    return sorted(lst)


# Phase 6: IO / print and input
def say_hello(msg: str) -> None:
    print(msg)


def print_twice(msg: str) -> None:
    print(msg)
    print(msg)


def echo() -> None:
    print("")


def main() -> None:
    pass
