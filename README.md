# brott

Detta repository innehåller en Shinyapplikation (`brott`) för Samhällsanalys, Region Dalarna.

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

### Självläkning vid deploy

Workflowen kör `chown -R shiny-deploy:shiny` på app-katalogen efter
varje deploy, så även om en fil tillfälligt fått fel ägare återställs
det automatiskt vid nästa körning.

### Återställa rättigheter manuellt

Om något ändå går snett:

```bash
sudo chown -R shiny-deploy:shiny /srv/shiny-server/brott
sudo find /srv/shiny-server/brott -type d -exec chmod 2775 {} \;
sudo find /srv/shiny-server/brott -type f -exec chmod 664 {} \;
```
