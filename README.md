# Implementace ECDSA v jazyce Haskell

Implementace programu je plně funkční a má otestované všechny příklady podle zadání.



Testované případy jsou následující:

```
./flp22-fun -i test/i_example.in
./flp22-fun -k test/i_example.in
./flp22-fun -s test/s_example.in
./flp22-fun -v test/v_example_true.in
./flp22-fun -v test/v_example_false.in
```

Všechny uvedené příklady samozřejmě fungují i pro čtení ze stdin

```
./flp22-fun -i < test/i_example.in
```

Příklady `-i` a `-v` mají přesný výstup podle specifikace.
Příklady `-k` a `-s` mají nedeterministický výstup, proto byly testovány pro podpis a následné ověření souboru.
Nejprve byl vygenerován privátní klíč a napsán textový soubor.
Ze souboru byl vypočítán hash příkazem `sha256sum`.
Ten byl použit společně s vygenerovaným soukromým klíčem pro podepsání souboru.


Program nebyl testován s jinou křivkou než SECP256k1 ze zadání, měl by ale fungovat.

## Omezení

Tato implementace využívá generátor náhodných čísel `StdGen` z knihovny Random.
Ten je nevhodný pro jakoukoli kryptografii, protože nená dostatečnou entropii.
Knihovna Random byla použita z důvodu, že v povolených knihovnách nebyla žádná vhodnější.

## Rozšíření

Oproti specifikaci má tato implementace pokročilejší parsing vstupu.
Ten je implementován pomocí knihovny parsec.
Díky tomu není vstup citlivý na pořadí atributů objektů, které jsou ve složených závorkách.
Parser také nebere ohled na whitespace, kde není nutně pořeba.

