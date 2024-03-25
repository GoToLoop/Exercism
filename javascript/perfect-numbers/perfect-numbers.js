// @ts-check

/** @throws {Error} */
export function classify(n=1) {
  if (n<=0) throw Error('Classification is only possible for natural numbers.');

  for (var lim = ~~Math.sqrt(n |= 0), sum = -n, div = 0, i = 1; i <= lim; ++i)
    if (n % i == 0) sum += i + (div = n / i) * +(div != i);

  return sum == n ? 'perfect' : sum > n && 'abundant' || 'deficient'; }
