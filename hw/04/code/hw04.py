from __future__ import annotations

from dataclasses import dataclass


@dataclass
class List:
    val: int
    next: List | None


@dataclass
class Tree:
    val: int
    left: Tree | None
    right: Tree | None


def inorder(node: Tree, A1: List) -> List:
    if not node:
        return
    A2 = inorder(node.left, A1)
    A3 = List(node.val, A2)
    return inorder(node.right, A3)
