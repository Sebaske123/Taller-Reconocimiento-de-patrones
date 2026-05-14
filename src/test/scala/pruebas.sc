import Newton._

// ─── Expresiones base ────────────────────────────────────────────────────────
val expr1 = Suma(Atomo('x'), Numero(2))
val expr2 = Prod(Atomo('x'), Atomo('x'))
val expr3 = Suma(expr1, Expo(expr2, Numero(5)))
val expr4 = Logaritmo(Atomo('x'))
val expr5 = Prod(Div(expr1, expr2), Resta(expr3, expr4))
val expr6 = Expo(Atomo('x'), Numero(3))

// ─── 1.1 mostrar ─────────────────────────────────────────────────────────────
mostrar(expr1)  // (x + 2.0)
mostrar(expr2)  // (x * x)
mostrar(expr3)  // ((x + 2.0) + ((x * x) ^ 5.0))
mostrar(expr4)  // (lg(x))
mostrar(expr5)  // (((x + 2.0) / (x * x)) * (((x + 2.0) + ((x * x) ^ 5.0)) - (lg(x))))
mostrar(expr6)  // (x ^ 3.0)

// casos adicionales mostrar
mostrar(Numero(0.0))                              // 0.0
mostrar(Div(Numero(1.0), Atomo('y')))             // (1.0 / y)
mostrar(Resta(Atomo('a'), Numero(5.0)))           // (a - 5.0)
mostrar(Logaritmo(Suma(Atomo('x'), Numero(1.0)))) // (lg((x + 1.0)))
mostrar(Prod(Numero(2.0), Expo(Atomo('x'), Numero(2.0)))) // (2.0 * (x ^ 2.0))

// ─── 1.2 derivar ─────────────────────────────────────────────────────────────
mostrar(derivar(expr6, Atomo('x')))
// ((x ^ 3.0) * (((1.0 * 3.0) / x) + (0.0 * (lg(x)))))

mostrar(derivar(expr2, Atomo('x')))
// ((1.0 * x) + (x * 1.0))

mostrar(derivar(expr2, Atomo('y')))
// ((0.0 * x) + (x * 0.0))

mostrar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x')))
// (0.0 + ((0.0 * x) + (3.0 * 1.0)))

// casos adicionales derivar
mostrar(derivar(Numero(7.0), Atomo('x')))          // 0.0
mostrar(derivar(Atomo('x'), Atomo('x')))           // 1.0
mostrar(derivar(Logaritmo(Atomo('x')), Atomo('x')))// (1.0 / x)
mostrar(derivar(Div(Atomo('x'), Numero(2.0)), Atomo('x'))) // ((1.0 * 2.0) - (x * 0.0)) / (2.0 * 2.0)
mostrar(derivar(Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0)), Atomo('x')))
// ((1.0 * x) + (x * 1.0)) - 0.0

// ─── 1.3 evaluar ─────────────────────────────────────────────────────────────
mostrar(Numero(5.0))
evaluar(Numero(5.0), Atomo('x'), 1.0)        // 5.0

mostrar(Atomo('x'))
evaluar(Atomo('x'), Atomo('x'), 5.0)         // 5.0

mostrar(Suma(expr1, expr2))
evaluar(Suma(expr1, expr2), Atomo('x'), 5.0) // 32.0

mostrar(Prod(expr1, expr2))
evaluar(Prod(expr1, expr2), Atomo('x'), 5.0) // 175.0

mostrar(Resta(expr1, expr2))
evaluar(Resta(expr1, expr2), Atomo('x'), 5.0) // -18.0

mostrar(Div(expr1, expr2))
evaluar(Div(expr1, expr2), Atomo('x'), 5.0)  // 0.28

mostrar(Expo(expr1, expr2))
evaluar(Expo(expr1, expr2), Atomo('x'), 5.0) // 1.341068619663965E21

mostrar(Logaritmo(expr1))
evaluar(Logaritmo(expr1), Atomo('x'), 5.0)   // 1.9459101490553132

// casos adicionales evaluar
evaluar(Numero(42.0), Atomo('x'), 0.0)                   // 42.0
evaluar(Prod(Numero(3.0), Atomo('x')), Atomo('x'), 4.0)  // 12.0
evaluar(Expo(Atomo('x'), Numero(2.0)), Atomo('x'), 3.0)  // 9.0
evaluar(Logaritmo(Atomo('x')), Atomo('x'), Math.E)       // 1.0
evaluar(Div(Atomo('x'), Numero(4.0)), Atomo('x'), 8.0)   // 2.0

// ─── 1.4 limpiar ─────────────────────────────────────────────────────────────
limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x')))
// Numero(3.0)

mostrar(limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x'))))
// 3.0

// casos adicionales limpiar
mostrar(limpiar(Suma(Numero(0.0), Atomo('x'))))              // x
mostrar(limpiar(Prod(Numero(1.0), Atomo('x'))))              // x
mostrar(limpiar(Prod(Numero(0.0), Expo(Atomo('x'), Numero(3.0))))) // 0.0
mostrar(limpiar(Expo(Atomo('x'), Numero(0.0))))              // 1.0
mostrar(limpiar(Resta(Atomo('x'), Numero(0.0))))             // x

// ─── 1.5 raizNewton ──────────────────────────────────────────────────────────
def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
  evaluar(f, a, d) < 0.001
}

val e1 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(2.0))
val e2 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))
val e3 = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0)), Prod(Numero(3.0), Atomo('x')))

raizNewton(e1, Atomo('x'), 2.0, buenaAprox) // 1.4142156862745099
raizNewton(e2, Atomo('x'), 2.0, buenaAprox) // 2.0
raizNewton(e3, Atomo('x'), 2.0, buenaAprox) // 1.0000029768726761

// casos adicionales raizNewton
val e4 = Resta(Atomo('x'), Numero(5.0))                          // x - 5 = 0  → raiz = 5
val e5 = Resta(Expo(Atomo('x'), Numero(3.0)), Numero(8.0))       // x^3 - 8 = 0 → raiz = 2
raizNewton(e4, Atomo('x'), 1.0, buenaAprox)
raizNewton(e5, Atomo('x'), 1.0, buenaAprox)
