# The pension calculation explained

How the retirement phase of `investment-scenarios-pensions.r` works, and how the
desired inheritance (bequest) is made a choice argument while keeping the
life-expectancy withdrawal rule.

All values are in real terms (2024 dollars); `r` is the **real** interest rate
earned during retirement (e.g. 1% for the "safe" scenario).

## 1. Setup

The work period delivers wealth `W` at age 65 (this is `wealth_65` in the code,
different for each 40-year historical window). Retirement covers ages 65–100
(`n = 36` years). Remaining life expectancy declines linearly from 25 years at
age 65 to 8 years at age 100 (both endpoints are function arguments):

```
L = seq(from = 25, to = 8, length.out = 36)
```

## 2. The withdrawal rule (recursion)

Each year the remaining wealth compounds at `r`, and the pension is the current
wealth spread over remaining life expectancy, scaled by a **withdrawal factor**
`λ`:

```
pension[i] = λ × wealth[i] / L[i]
wealth[i]  = wealth[i-1] × (1 + r) − pension[i-1]      wealth[1] = W
```

Timing conventions:

- The pension at age 65 is zero — the first withdrawal is at age 66, so the
  first transition earns interest without any withdrawal.
- The age-100 pension **is** paid out before the inheritance is measured:
  `inheritance = wealth[36] − pension[36]`.

`λ = 1` is the original rule. With the default schedule (ending at `L = 8`),
the rule never fully depletes wealth: a residual is always left over. With
`life_exp_100 = 1` the rule withdraws everything at age 100 and the residual
is exactly zero.

## 3. Closed form for the inheritance

Substituting the pension into the wealth recursion:

```
wealth[i] = wealth[i-1] × (1 + r − λ / L[i-1])
```

so the wealth at age 100, and the inheritance after the age-100 pension, are

```
wealth_at_100(λ) = W × (1 + r) × Π_{i=2}^{35} (1 + r − λ / L[i])

inheritance(λ)   = wealth_at_100(λ) × (1 − λ / L[36])
```

(the standalone `(1 + r)` factor is the age-65→66 transition, in which no
pension is withdrawn yet; the final factor has no `r` because the age-100
pension is paid out of the already-compounded wealth).

## 4. Making the bequest a choice

To leave exactly `B` at age 100, solve

```
inheritance(λ) = B
```

for `λ`. On the interval where all factors stay positive, the right-hand side
is continuous and **strictly decreasing** in `λ`, so a unique solution exists
and is found numerically with `uniroot()` (base R; milliseconds per window).

Interpretation of `λ`:

| case | λ | meaning |
|---|---|---|
| bequest below the natural residual | λ > 1 | spend more aggressively than the rule |
| bequest = natural residual | λ = 1 | exactly the original rule |
| bequest above the natural residual | λ < 1 | spend conservatively to fund the bequest |
| bequest = maximum (never consume) | λ = 0 | pension is 0 |

**Two routes to "die with zero":**

- `life_exp_100 = 1` (recommended): the schedule itself withdraws everything
  at age 100, so `λ = 1` already gives inheritance exactly zero and the
  pension path stays **smooth**.
- `bequest = 0` (λ-solved): works with the default schedule too, but needs
  `λ ≈ 8`, which front-loads the path extremely (see section 7).

## 5. Feasibility

The bequest must satisfy

```
0 ≤ B ≤ W × (1 + r)^35
```

(the maximum = never consume anything and let `W` compound for 35 years). The
code stops with an informative error outside this range — relevant because some
historical windows are poor (e.g. the 1942 starter has only ~$305k at 65), so a
large *absolute* bequest can be infeasible for them.

## 6. Dollars or percent

The bequest can be specified either way:

- `bequest = 100000` — absolute, in 2024 dollars;
- `bequest_share = 0.25` — as a share of that window's `wealth_65`
  (internally: `bequest = bequest_share × wealth_65`).

Shares keep scenarios comparable across rich and poor windows and are always
feasible for values below 100%. (Shares *above* 100% are also feasible, up to
`(1+r)^35`, since wealth compounds during retirement.)

## 7. Caveat: large λ distorts the *shape* of the pension path

`λ` scales the withdrawal rate uniformly in every year, so a large `λ` does not
just raise pensions — it **front-loads** them. A λ-solved `bequest = 0`
(`λ ≈ 8`) withdraws about a third of the nest egg at age 66, after which wealth
(and hence the pension) collapses quickly; the effect gets more extreme the
higher the interest rate. As a result the *average* pension over ages 66–100
is not monotone in the target along the λ-solved route: it peaks near the
natural residual share and can be *lower* at a 0% bequest than at a 10% one,
even though total consumption in present-value terms is highest at 0%.

The life-expectancy schedule still governs the shape; `λ` only scales it. For
aggressive spend-down targets, prefer the smooth route (`life_exp_100 = 1`),
which needs no large `λ` and keeps the path well-behaved.

## 8. Worked example

1984 window, safe scenario (`r = 1%`), `W = $1,335,355`; maximum possible
bequest `$1,891,667`; natural residual share ≈ 12.1%:

| bequest target | route | λ | avg pension (66–100) | inheritance at 100 |
|---|---|---|---|---|
| none (natural rule) | — | 1.00 | $40,999 | $161,394 |
| 0 ("die with zero") | smooth, `life_exp_100 = 1` | 1.00 | $44,892 | $0 |
| 0 ("die with zero") | λ-solved, `bequest = 0` | 8.00 | $39,702 | $0 |
| 10% | λ-solved | 1.07 | $41,477 | $133,535 |
| 25% | λ-solved | 0.71 | $37,585 | $333,839 |
| 50% | λ-solved | 0.43 | $30,082 | $667,677 |

Note the shape effect from section 7 in action: the smooth route to zero beats
the λ-solved route *on average pension too* ($44.9k vs $39.7k), because the
λ-solved route's extreme early withdrawals are offset by a near-zero tail.
Targets above the natural residual (25%, 50%) need `λ < 1` and behave
smoothly; targets hit exactly (to solver precision, ~1e-10).
