-- filepath: c:\Users\elcha\paradigmaUAP2025\funcional\ejercicios\ejercicio1\Guia1Soluciones.elm

module Guia1Soluciones exposing (..)

-- Ejercicio 1: Potencia
power : Int -> Int -> Int
power a b =
    if b == 0 then
        1
    else if b < 0 then
        0 -- No se manejan potencias negativas de enteros
    else
        a * power a (b - 1)

-- Ejercicio 2: Factorial
factorial : Int -> Int
factorial n =
    if n <= 1 then
        1
    else
        n * factorial (n - 1)

-- Ejercicio 3: Fibonacci
fibonacciExponential : Int -> Int
fibonacciExponential n =
    if n == 0 then
        0
    else if n == 1 then
        1
    else
        fibonacciExponential (n - 1) + fibonacciExponential (n - 2)

fibonacciLinear : Int -> Int
fibonacciLinear n =
    let
        fibAux a b count =
            if count == 0 then
                a
            else
                fibAux b (a + b) (count - 1)
    in
    fibAux 0 1 n

-- Ejercicio 4: Triángulo de Pascal
pascalTriangle : Int -> Int -> Int
pascalTriangle x y =
    if x == 0 || x == y then
        1
    else
        pascalTriangle (x - 1) (y - 1) + pascalTriangle x (y - 1)

-- Ejercicio 5: Máximo Común Divisor (MCD)
gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        abs a
    else
        gcd b (a % b)

-- Ejercicio 6: Contar Dígitos
countDigits : Int -> Int
countDigits n =
    let
        absN = abs n
    in
    if absN < 10 then
        1
    else
        1 + countDigits (absN // 10)

-- Ejercicio 7: Suma de Dígitos
sumDigits : Int -> Int
sumDigits n =
    let
        absN = abs n
    in
    if absN < 10 then
        absN
    else
        (absN % 10) + sumDigits (absN // 10)

-- Ejercicio 8: Verificar Palíndromo
reverseNumber : Int -> Int
reverseNumber n =
    let
        absN = abs n
        revAux num acc =
            if num == 0 then
                acc
            else
                revAux (num // 10) (acc * 10 + (num % 10))
    in
    revAux absN 0

isPalindrome : Int -> Bool
isPalindrome n =
    let
        absN = abs n
    in
    absN == reverseNumber absN

-- Ejercicio 9: Paréntesis Balanceados
isBalanced : String -> Bool
isBalanced str =
    let
        aux chars count =
            case chars of
                [] ->
                    count == 0
                c :: cs ->
                    if count < 0 then
                        False
                    else if c == '(' then
                        aux cs (count + 1)
                    else if c == ')' then
                        aux cs (count - 1)
                    else
                        aux cs count
    in
    aux (String.toList str) 0