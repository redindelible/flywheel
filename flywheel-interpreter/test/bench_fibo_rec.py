import timeit

def fibo_rec(n):
    if n < 2:
        return n
    else:
        return fibo_rec(n-1) + fibo_rec(n-2)

print(f"{timeit.timeit(lambda: fibo_rec(28), number=5)} sec")
