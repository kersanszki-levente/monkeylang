let fib = fn(n) { if (n <= 1) { return n } else { return fib(n-1) + fib(n-2) } };
let result = fib(1);
result == 1;

let result = fib(7);
result == 13;
