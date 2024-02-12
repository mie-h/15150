from __future__ import annotations

from dataclasses import dataclass


@dataclass(slots=True, frozen=True)
class Nil: ...


@dataclass(slots=True, frozen=True)
class Cons:
    value: int
    next: List


List = Nil | Cons


def sublist(i: int, k: int, l: List) -> List:
    if k == 0:
        return []
    if i == 0 and isinstance(l, Cons):
        return Cons(l.value, sublist(i, k - 1, l.next))
    if isinstance(l, Cons):
        return sublist(i - 1, k, l.next)
    raise AssertionError()


def sublist(i: int, k: int, l: List) -> List:
    if i < 0 or k < 0 or i + k > len(l):
        raise AssertionError()

    while i:
        l = l.next
        i -= 1

    sl = []
    while k:
        sl.append(l.value)
        l = l.next
        k -= 1

    result = Nil
    for i in sl:
        result = Cons(i, result)
    return result
