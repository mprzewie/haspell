https://github.com/Koozco/codeFormat.git

- Kod sprawia ogólnie dobre wrażenie
- nazwy zmiennych i funkcji raczej ekspresywne (wyjątek: splitWithForb)
- w niektórych nazwach pojawiają się literówki, np. formated zamiast formatted, co jest mylące i uciążliwe przy korzystaniu z kodu
- w niektórych miejscach nieco myląca konwencja: (\param_name -> f1 . f2 $ param_name) other_param	=> można by zmienić na krótsze i szybsze do sparsowania w głowie: (f1 . f2) other_param
- wiekszość funkcji dobrze udokumentowana
- miejscami złamana konwencja wywołań funkcji w Haskellu, tzn. "fun(x)" zamiast "fun x"

- brak testów
- odkrywanie koła na nowo - splitByWhiteChars to powtórka z istniejącej metody words
