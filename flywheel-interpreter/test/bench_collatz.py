import timeit

def collatz(n):
    i = 0
    while n != 1:
        if n % 2 == 0:
            n /= 2
        else:
            n = 3*n + 1
        i += 1
    return i

print(f"{timeit.timeit(lambda: collatz(6171), number=10000)} sec")
