def fibonacci(n):
    if n <= 1:
        return n
    a, b = 0, 1
    for _ in range(n - 1):
        a, b = b, a + b
    return b

# Test the function
test_numbers = [0, 1, 5, 7, 10]
for n in test_numbers:
    result = fibonacci(n)
    print(f"fibonacci({n}) = {result}")
