[![Build Status](https://travis-ci.org/zyla/rybu.svg?branch=master)](https://travis-ci.org/zyla/rybu)

**rybu** to prosty język kompilowany do modeli programu Dedan.

[Dokumentacja](https://github.com/zyla/rybu/wiki/Opis-sk%C5%82adni-Rybu)

Istnieje możliwość użycia programu z poziomu przeglądarki WWW, bez konieczności
instalacji: http://zyla.neutrino.re/rybu/.

## Kompilacja i uruchamianie lokalnie

Program napisany jest w języku [Haskell](https://www.haskell.org).
Przy użyciu narzędzia [stack](http://docs.haskellstack.org/en/stable/README/#how-to-install)
kompilacja jest prosta:

    stack build

Program przyjmuje plik wejściowy poprzez standardowe wejście i wypisuje wynikowy
model na standardowy wyjście. Chcąc skompilować przykład `kolos.txt` należy
wpisać:

    stack exec rybu < examples/kolos.txt > kolos.dedan.txt

Po czym plik `kolos.dedan.txt` należy załadować do programu Dedan.

## Kontakt

Pytania można zadać na kanale [#rybu na freenode](http://webchat.freenode.net/?channels=rybu)
