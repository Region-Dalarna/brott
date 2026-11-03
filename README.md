# brott

Detta repository innehåller en Shinyapplikation (`brott`) för Samhällsanalys, Region Dalarna.

## Avloppsmätningar (RISE) - kommuner med flera mätplatser

`rise.avloppsmatningar` fick en ny kolumn `plats` när en kommun började mäta på två platser
samtidigt. `forbered_avloppsmatningar()` i `R/data_avlopp.R` räknar ut antalet unika platser
per kommun (över hela datasetet, inte bara vald period/ämne) och bygger kolumnen
`kommun_etikett`: `"Kommun, Plats"` för kommuner med fler än en plats, annars bara
kommunnamnet. Stapeldiagram, heatmap, linjediagram och fokuskommun-väljaren använder
`kommun_etikett` i stället för `kommun` rakt av.

Kartan är undantaget - den har en polygon per kommun (kopplad via `kommunkod`) och kan inte
färgsätta flera platser separat. `avlopp_senaste_per_kommun_karta()` väljer därför den plats
som mättes senast (högst `sasongsnyckel`) för polygonens färg; om platserna råkar ha mätningar
från samma säsong används istället den högsta av dem. Tooltipen är opåverkad av detta - den
listar ändå varje plats för sig (med värde, period och ev. flaggor) när kommunen har mer än en.

## Avloppsmätningar - tidpunktsväljare i stapeldiagrammet

Stapeldiagrammet visar som standard varje kommuns senaste mätning (`avlopp_senaste_per_kommun()`),
vilket kan innebära olika säsonger för olika kommuner. Staplarna färgläggs därför efter
tidpunkt (`period`) så att det syns tydligt när de inte är direkt jämförbara. Man kan även välja
en specifik tidpunkt i `avlopp_tidpunkt`-väljaren (`avlopp_vid_tidpunkt()` i `R/data_avlopp.R`),
vilket visar samtliga kommuners värde för just den säsongen - då blir alla staplar samma färg,
eftersom de då faktiskt är jämförbara. Standardvalet styrs av sentinel-värdet
`AVLOPP_TIDPUNKT_SENASTE` i `global.R`, av samma anledning som `AVLOPP_FOKUS_ALLA` (se nedan).

## Avloppsmätningar - "Jämför alla kommuner" och sentinel-värden i selectize

`AVLOPP_FOKUS_ALLA`/`AVLOPP_TIDPUNKT_SENASTE` i `global.R` MÅSTE vara icke-tomma strängar.
selectize.js (som Shinys `selectInput` använder) tolkar ett val med tomt värde (`""`) som en
platshållartext i stället för ett riktigt, klickbart listval - det alternativet går då aldrig
att välja i efterhand om det försöker använda `""`. Strängarna är bara till för att aldrig
krocka med ett riktigt `kommun_etikett`/period-värde.

## Struktur

- All appkod ligger i katalogen `app/`
  - `ui.R`, `server.R`, `global.R`
  - `www/` för favicon och övriga statiska filer
  - `R/` för hjälpfunktioner

- Deployment sker via GitHub Actions (`.github/workflows/deploy.yml`)
  till Shiny-servern (appmapp `/srv/shiny-server/brott`).

## Rättigheter och användare på Shiny-servern

För att deploy och drift ska fungera följer vi en enkel princip:
**en användare skriver, en grupp läser.**

- **Ägare** av `/srv/shiny-server/brott` och allt under: `shiny-deploy`
  (det är användaren som GitHub Actions-runnern kör som och som
  rensar/kopierar filer vid varje deploy).
- **Grupp**: `shiny` (gruppen som `shiny-server`-tjänsten kör som och
  behöver kunna läsa filerna för att servera appen).
- **Läge på katalogen**: `2775` (`drwxrwsr-x`). Setgid-biten (`s` på
  gruppen) gör att nya filer som skapas i katalogen automatiskt ärver
  gruppen `shiny`.
- **Läge på filer**: `664` (`-rw-rw-r--`).
- **`shiny-deploy` måste vara medlem i gruppen `shiny`** — det är vad som
  gör att deploy-skriptet kan köra `chgrp -R shiny` utan `sudo`. Lägg till
  med `sudo usermod -aG shiny shiny-deploy` och starta om GitHub
  Actions-runnern så att gruppmedlemskapet slår igenom.

### Vanliga fallgropar

- **Redigera aldrig filer i `/srv/shiny-server/brott` som `root` eller
  någon annan användare än `shiny-deploy`** — då blir filen ägd av
  fel användare och nästa deploy får `Åtkomst nekas` när den försöker
  ta bort filen. Logga in som `shiny-deploy` (`sudo -u shiny-deploy ...`)
  om du måste ändra något manuellt.
- **`rm <fil>` kräver skrivrätt på *katalogen*, inte filen.** När du
  felsöker rättighetsfel: börja alltid med `ls -ld` på katalogen
  (och dess föräldrar), inte på filen.
- **Föräldrakatalogerna måste också vara åtkomliga.** `/srv/shiny-server`
  har `drwxr-s---  shiny-deploy  shinyapps`, så bara ägaren och medlemmar
  i gruppen `shinyapps` kommer åt underkatalogerna.
- **Om `shiny-deploy` inte är medlem i gruppen `shiny`** failar deployen
  med `Operation not permitted` när `chgrp` körs. Lägg till medlemskapet
  enligt ovan och starta om runnern.

### Konsistens vid deploy

Deploy-skriptet `/usr/local/bin/shiny_deploy.sh` avslutas med
`chgrp -R shiny` plus `chmod 2775`/`664` på app-katalogen vid varje deploy.
Grupp och läge återställs alltså automatiskt vid varje körning, även om en
fil tillfälligt fått fel ägare eller fel läge. Inget extra steg behövs i
`.github/workflows/deploy.yml`.

### Återställa rättigheter manuellt

Normalt behövs inte detta — `shiny_deploy.sh` sköter grupp och läge vid varje
deploy. Men om en fil ägs av fel *användare* (t.ex. `root` efter en
felaktig manuell redigering) måste det rättas med `sudo`, eftersom `chown`
av användare kräver root:

```bash
sudo chown -R shiny-deploy:shiny /srv/shiny-server/brott
sudo find /srv/shiny-server/brott -type d -exec chmod 2775 {} \;
sudo find /srv/shiny-server/brott -type f -exec chmod 664 {} \;
```

Efter det här tar nästa deploy hand om resten via `shiny_deploy.sh`.
