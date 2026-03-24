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


def main() -> None:
    pass
