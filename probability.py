def factorial(x):
    """ 
    Factorial function 
    x * (x - 1) * (x - 2) * ... * 1
    https://en.wikipedia.org/wiki/Factorial 
    """
    if x == 1:
        return x
    elif x < 1:
        return 1
    else:
        return x * factorial(x - 1)


def choose(n, k):
    """
    Combination of n elements into k groups
    https://en.wikipedia.org/wiki/Combination
    """
    return factorial(n) / (factorial(k) * factorial(n-k))


def dice_pmf(x):
    """ 
    Dice probability mass function
    """
    if x >= 1 or x <= 6:
        return 1/6
    else:
        return 0


def cmf(pdf_fn, lower_bound, upper_bound):
    """ 
    Cumulative mass function 
    """
    values = list(range(lower_bound, upper_bound + 1))
    p = sum(list(map(pdf_fn, values)))
    return p


def bernoulli(x, p):
    """
    Bernoulli pmf 
    x: event
    p: probability of success
    Probability of event
    """
    if x == 1:
        return p
    elif x == 0:
        return 1 - p


def binomial(x, n, p):
    """
    Binomial pmf 
    x: number of successful events
    n: number of events
    p: probability of success
    https://en.wikipedia.org/wiki/Binomial_distribution
    """
    return choose(n, x) * bernoulli(1, p)**x * bernoulli(0, p)**(n - x)

dados = 6
p1 = sum(binomial(x, dados, 1/6) for x in range(1, dados + 1))

dados = 12
p2 = sum(binomial(x, dados, 1/6) for x in range(2, dados + 1))

dados = 18
p3 = sum(binomial(x, dados, 1/6) for x in range(3, dados + 1))

print(p1, p2, p3)
