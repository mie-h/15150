# val (["aa", "bb", "cc"], [1,2,3]) = unzip([("aa", 1), ("bb", 2), ("cc", 3)])

def unzip(lst: list[tuple[str, int]]) -> tuple[list[str], list[int]]:
    lst1 = []
    lst2 = []
    for s, n in lst:
        lst1.append(s)
        lst2.append(n)
    return lst1, lst2

def unzip_r(lst: list[tuple[str, int]]) -> tuple[list[str], list[int]]:
    if not lst:
        return [], []
    x, y = lst[0]
    lst1, lst2 = unzip_r(lst[1:])
    lst1.insert(0, x)
    lst2.insert(0, y)
    return lst1, lst2


# let 
#   val (lst1, lst2) = runWith (x, ys)
# in 
#   if x = y then (x::lst1, lst2)
#   else
#     ([], lst)
# end
def runWith(x: int, l: list[int]) -> tuple[list[int], list[int]]:
    if not l:
        return [], []
    
    if x == l[0]:
        lst1, lst2 = runWith(x, l[1:])
        lst1.append(x)
        return lst1, lst2
    else:
        return [], l