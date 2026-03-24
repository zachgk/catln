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


def main() -> None:
    pass
