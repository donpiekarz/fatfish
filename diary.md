This file will be in polish.

Chciałbym tutaj opisywać swoją przygodę przy przygotowywaniu fatfisha.

2014-12-09
===========================
Biblioteka crypto w erlangu nie potrafi dodawać paddingu do wiadomości szyfrowanych blokowo. I nie powie tego w prost, musiałem to wyczytać kodów w C samego erlanga.

W Każdym razie, taki kod:

crypto:block_encrypt(des_cbc, <<"12345678">>, <<"12345678">>, <<"czysty tekst\n16\n">>).

dekoduje się tak opensslem:

openssl enc -d -in fatfish_des.bin -des-cbc -K 3132333435363738 -iv 3132333435363738 -nopad


2014-12-08
===========================
Ostatni tydzień spędziłem na rozgryzaniu co wpisać w pole issuer i serial w RecipientInfo. Przy okazji tego zwiedziłem implementacje openssla. 
Jak to udało mi się wyczytać i dopasować poszedłem dalej i spróbowałem zdekodować klucz sesyjny z przykładowego zaszyfrowanego przez thunderbirda maila. Czy wyszło to nie umiem teraz tego stwierdzić, bo nie umiem zdekodować treści wiadomości zaszyfrowanej przy użyciu des3_ede_cbc (ede w środku jest skrótem od encrypt decrypt encrypt). I to ostanie spowodowało, że z rozpaczą znowu zaczołem czytać kod openssla. Po tym jak wypłakałem litr łez czytając ten kod, wpadłem na pomysł czytania kodu thunderbirda. No i muszę powiedzieć, że jest o niebo lepiej niż w opensslu. Sam kod jest bardziej dla mnie zrozumiały. Są nawet komentarze, które wydają się być aktualne (czy są one przydatne to już różnie). No i nie muszę ręcznie wyszukiwać referencji (@github kiedy to zrobicie??). 
Przy okazji kopania w thunderbirdzie okazuje się, że oni maja swoją implementacje do kryptografii - nss. Trudno mi jeszcze wypowiedzieć się w sprawie jakości.

Problemy:
- co to sa za parametry do algorytmu przed szyfrogramem i po nim? (być może jest to IV, na pewno się to różni miedzy wiadomościami).
- walka z dekodowaniem wiadomości z użyciem desa i erlanga.

Plany:
- erlang <=> openssl zgodność na poziomie szyfrogramów,
- rozkodować smimie w opensslu i zakodować jeszcze raz, zobaczyc co z tego wyjdzie,
- zdekodowanie w erlangu przykładowej wiadomości z thunderbirda.


prolog - przed 2014-12-08
===========================
Najwięcej czytania i walki miałem z asn1. Jak udało mi się wygenerować implementacje w erlangu do CMS z rfc to okazało się, że jest to zbędne. Wtedy znalazłem gotowe bindingi pcks. Później systematycznie odtwarzam co te pola wewnatrz znaczą i jakie dane tam pownienem wpisać.
Najwiekszym szokiem było odkrycie, że mogłem sie zbindować do openssla jak robią to wszyscy inni (i erlang też).
Jest też tutaj ciekawe to, że duża cześć projektów bazuje na opensslu. Openssl jest jedynym 'wlasciwym' rozwiazaniem, o czym już się świat przekonał wiosną b.r. przy okazji [Heartbleed](https://en.wikipedia.org/wiki/Heartbleed).
