// Savutesti ennen julkaisua: avaa kaikki nakymat kahdella leveydella ja klikkaa Reaalitalouden rivit.
// Epaonnistuu, jos konsoliin tulee virhe, sivu vierii sivusuunnassa tai keskeinen sisalto puuttuu.
const { chromium } = require("playwright");
const path = require("path");
const file = "file://" + path.resolve(process.argv[2] || "public/index.html");
(async () => {
  const b = await chromium.launch(); const fails = [];
  for (const w of [1440, 360]) {
    const p = await b.newPage({ viewport: { width: w, height: 900 } });
    p.on("pageerror", e => fails.push(`${w}px: JS-virhe: ${e.message}`));
    p.on("console", m => { if (m.type() === "error" && !/fonts\.g/.test(m.text())) fails.push(`${w}px: konsoli: ${m.text()}`); });
    for (const h of ["#suhdanne", "#reaalitalous", "#rahoitus"]) {
      await p.goto(file + h); await p.waitForTimeout(800);
      const r = await p.evaluate(() => ({ sw: document.documentElement.scrollWidth, iw: innerWidth,
        pts: document.querySelectorAll("#clock g.pt").length, rows: document.querySelectorAll("#grTable tr.pick").length,
        charts: document.querySelectorAll("#charts .fcard, #grCharts .fcard").length }));
      if (r.sw > r.iw + 1) fails.push(`${w}px ${h}: vaakavieritys ${r.sw} > ${r.iw}`);
      if (h === "#suhdanne" && r.pts < 5) fails.push(`${w}px ${h}: kartalla vain ${r.pts} pistetta`);
      if (h === "#reaalitalous" && r.rows < 5) fails.push(`${w}px ${h}: taulukossa vain ${r.rows} rivia`);
      if (h === "#rahoitus" && r.charts < 5) fails.push(`${w}px ${h}: vain ${r.charts} kuviota`);
      if (h === "#reaalitalous") {
        const ids = await p.$$eval("#grTable tr.pick", e => e.map(x => x.dataset.id));
        for (const id of ids) {
          await p.click(`#grTable tr[data-id="${id}"] th`); await p.waitForTimeout(120);
          const n = await p.evaluate(() => document.querySelectorAll("#grCharts svg").length);
          if (!n) fails.push(`${w}px rivi ${id}: ei kuviota`);
          const sw = await p.evaluate(() => document.documentElement.scrollWidth);
          if (sw > w + 1) fails.push(`${w}px rivi ${id}: vaakavieritys ${sw}`);
        }
      }
    }
    await p.close();
  }
  await b.close();
  if (fails.length) { console.error("SAVUTESTI EPAONNISTUI:\n" + [...new Set(fails)].join("\n")); process.exit(1); }
  console.log("Savutesti ok");
})();
