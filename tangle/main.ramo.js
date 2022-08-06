import  { run, Rel, conde, exist, eq, conso, nilo } from 'https://esm.sh/ramo';

const appendo = Rel((xs, ys, zs) => conde(
  [nilo(xs), eq(ys, zs)],
  exist((a, d, res) => [
    conso(a, d, xs),
    conso(a, res, zs),
    appendo(d, ys, res)
  ])
));

const roleo = Rel((role, el, er, biclique, eo) => [
    exist((x) => [
        eq(x, [role, el, er, biclique, eo]),
        conde(
            eq(x, ['a', 1, 0, 'l', 0]),
            eq(x, ['b', 0, 1, 'r', 0]),
            eq(x, ['u', 1, 0, 'o', 1]),
            eq(x, ['v', 0, 1, 'o', 1]),
            eq(x, ['w', 1, 1, 'o', 1]),
            eq(x, ['o', 0, 0, 'o', 0]))])])
            
console.log(run()(q => exist((a, b, c, d, e) => [eq(q, [a, b, c, d, e]), roleo(a, b, c, d, e)])))



