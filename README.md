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

Všechny uvedené příklady samozřejmě fungují i pro čtení ze stdin.

```
./flp22-fun -i < test/i_example.in
```

Příklady `-i` a `-v` mají přesný výstup podle specifikace.
Příklady `-k` a `-s` mají nedeterministický výstup, proto byly testovány pro podpis a následné ověření souboru.

Nejprve byl vygenerován privátní klíč a napsán textový soubor.
Ze souboru byl vypočítán hash příkazem `sha256sum`.
Ten byl použit společně s vygenerovaným soukromým klíčem pro podepsání souboru.
Poté byl vytvořen soubor s křivkou, podpisem, veřejným klíčem a hashem.
Tento soubor byl poté úspěšně ověřen spuštěním programu s argumentem `-v`. Po změně náhodně vybrané číslice podpisu byl výstup `False`, což je očekávané chování.

## Omezení

Program byl vyvíjen pro křivku `SECP256k1` ze zadání.
Dále byla testována křivka `SECP192r1`.
Podepisování a ověřování bylo funkční, ale veřejný klíč je pravděpodobně ve špatném formátu.
Ze vstupu Curve nelze spolehlivě zjistit, jakou bitovou šířku mají parametry.
Proto má tento parametr pevnou velikost 256 bitů, což by mohlo znemožnit použití s většími čísly.
Jiné křivky nebyly testovány.

Pro zpracování hashovací funkce je použit přístup z
[Wikipedie](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm), kde je využito prvních n bitů z hashe a zbytek je zahozen.
V této implementaci je použito vždy 256 bitů z již zmíněných důvodů.

Tato implementace využívá generátor náhodných čísel `StdGen` z knihovny Random.
Ten je nevhodný pro jakoukoli kryptografii, protože nená dostatečnou entropii.
Knihovna Random byla použita z důvodu, že v povolených knihovnách nebyla žádná vhodnější.

## Rozšíření

Oproti specifikaci má tato implementace pokročilejší parsing vstupu.
Ten je implementován pomocí knihovny parsec.
Díky tomu není vstup citlivý na pořadí atributů objektů, které jsou ve složených závorkách.
Parser také nebere ohled na whitespace, kde není nutně pořeba.

Všechny parametry typu Integer mohou být zadány v decimálním nebo hexadecimálním formátu (hexadecimální s předponou `0x`).
Výstup těchto parametrů je ve formátu, který je specifikován zadáním.

