# Suhdannetilanne

Suomen suhdannetilanne kuukausi- ja neljännesvuosi-indikaattoreista. Henkilökohtainen kokeilu.

- `series.json`: sarjat, ryhmät ja tekstit
- `pipeline.R`: hakee datan (Tilastokeskus, Eurostat, EKP, Suomen Pankki, FRED), laskee trendipoikkeamat ja kirjoittaa `clock.json` ja `suhdannetilanne.html`
- `clock_template.html`: sivupohja, johon data upotetaan

Paikallisesti: `LANG=C.UTF-8 Rscript pipeline.R`, avaa `suhdannetilanne.html`.

Julkaisu: GitHub Actions ajaa putken arkisin ja julkaisee `public/`-kansion Cloudflare Workersiin (`wrangler.jsonc`). `STRICT=1` merkitsee ajon epäonnistuneeksi, jos sarja on vanhentunut tai tuli välimuistista; sivu julkaistaan silti.

Data: lähteiden omat lisenssit, esim. Tilastokeskus CC BY 4.0.
