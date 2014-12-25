This file will be in polish.

Chciałbym tutaj opisywać swoją przygodę przy przygotowywaniu fatfisha.

2014-12-25
===========================
Przez ostatnie dwa tygodnie walczyłem z deszyfrowaniem wiadomości smime zapisanej przez Thunderbirda. Czytałem implementacje OpenSSLa, NSSa (biblioteka z której korzysta Mozilla) oraz różne książki do kryptografii (polecam: Cryptography and Network Security - Principles and Practice, 6th edition). W końcu zdecydowałem się pobrać, skompilować i zdebugować samego OpenSSLa. Następnie krok po kroku analizowałem co z czego jest dekodowane. 


```
# generujemy tekst jawny, lepiej żeby długość tego tekstu była wielokrotnością 8
echo "to jest test 42" > 3msg.dec.txt
```

```
# szyfrujemy tekst jawny des3 i zapisujemy w formacie DER (żeby było szybciej), używamy do tego certyfikatu odbiorcy
openssl smime -encrypt -des3 -in 3msg.dec.txt -out 3msg.enc.der -outform der ../certs/koparka.czerwona/cert.pem
```

```
# sprawdzamy czy coś z tego wyszło, narzędzie dumpasn1 (paczka w debianie nazywa sie asn1dump)
dumpasn1 3msg.enc.der
```
sprawdzamy czy coś z tego wyszło, narzędzie openssl asn1parse
```
openssl asn1parse -in 3msg.enc.der -inform der
```

wyciągamy zaszyfrowany klucz sesyjny do osobnego pliku, 228 to offset z wydruków poprzednich komend. 
```
openssl asn1parse -in 3msg.enc.der -inform der -strparse 228 -noout -out 3msg.enc.key
```

sprawdzamy czy zawartość jest taka jakiej oczekiwaliśmy (256 bajtów)
```
xxd 3msg.enc.key
```

dekodujemy klucz sesyjny przy użyciu klucza prywatnego odbiorcy. padding jest domyślny
```
openssl rsautl -decrypt -in 3msg.enc.key -out 3msg.dec.key -inkey ../certs/koparka.czerwona/priv.pem
```

sprawdzamy czy sie zgadza. 3des, czyli 3 * 8 = 24 bajtów 
```
xxd -p 3msg.dec.key
```

wyciagamy wektor inicjujący, 513 to offset z pierwszego wydruku z pola parameters, zaraz za identyfikatorem algorytmu szyfrującego
```
openssl asn1parse -in 3msg.enc.der -inform der -strparse 513 -noout -out 3msg.iv
```
sprawdzamy, czy dostaliśmy to co chcemy: 8 bajtów
```
xxd 3msg.iv
```

wyciągamy zaszyfrowana wiadomość, 523 to offset
```
openssl asn1parse -in 3msg.enc.der -inform der -strparse 523 -noout -out 3msg.enc.msg
```

sprawdzamy co wyszło, powinny być 24 bajty
```
xxd 3msg.enc.msg
```

usuniecie 2 bajtow na początku pliku, po tej edycji musimy dostać 24 bajty. zwrócicie uwage czy vim nie dodał znaku końca linii
```
vim -b 3msg.enc.msg
```

ostateczne deszyfrowanie, klucz i wektor muszą być podane w formie szestnastkowej
```
openssl des3 -d -in 3msg.enc.msg -K `xxd -p 3msg.dec.key` -iv `xxd -p 3msg.iv`
```

Tyle.

Warto tutaj jeszcze wspomnieć, że to nie jest dokładny tutorial. Wystarczy, że użyjemy innego: algorytmu, klucza (rodzaj, długość), tekstu jawnego i nasze offsety nie będą się zgadzać, itp. Co może powodować różne problemy. Pominołem też opis struktury asn1, bo jest na tyle 'intuicyjna', że nie ma na to tutaj miejsca :)


Rozwiązania poprzednich problemów:
- te parametry do algorytmu to jest IV (wektor incjujący)

Plany:
- zdekodowanie testowej wiadomości w erlangu
- pisanie kodu


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
