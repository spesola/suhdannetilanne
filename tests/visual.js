// Visuaalinen regressiotesti: sivupohja kiinteallä aineistolla (tests/visual/fixture.json.gz) ja kiinteällä kellonajalla,
// kuvakaappaukset kaikista näkymistä kahdella leveydellä, vertailu tallennettuihin pohjakuviin (tests/visual/*.png).
//   node tests/visual.js [sivupohja]            vertaa; poikkeamat ja erokuvat hakemistoon tests/visual/out
//   node tests/visual.js [sivupohja] --update   tallentaa nykyiset kuvat uusiksi pohjakuviksi (tarkoituksellisen muutoksen jälkeen)
// Poistumiskoodi 1, jos jokin näkymä poikkeaa yli RAJA-osuuden pikseleistä tai kuvan koko muuttuu.
// Ajetaan ennen julkaisua samassa ympäristössä, jossa pohjakuvat on tehty (selainversio ja fontit vaikuttavat pikseleihin).
const { chromium } = require("playwright");
const fs = require("fs"), path = require("path"), zlib = require("zlib"), os = require("os"), http = require("http");
const ROOT = path.resolve(__dirname, ".."), DIR = path.join(__dirname, "visual"), OUT = path.join(DIR, "out");
const args = process.argv.slice(2), UPDATE = args.includes("--update");
const TEMPLATE = path.resolve(args.find(a => !a.startsWith("--")) || path.join(ROOT, "clock_template.html"));
const RAJA = 0.002;   // sallittu osuus poikkeavia pikseleitä (fonttien reunanpehmennys)
const VIEWS = ["suhdanne", "reaalitalous", "rahoitus", "julkinen-talous"], WIDTHS = [1440, 390];

// sivu tilapäishakemistoon fonttien ja kuvakkeiden kanssa
const tmp = fs.mkdtempSync(path.join(os.tmpdir(), "visual-"));
const data = zlib.gunzipSync(fs.readFileSync(path.join(DIR, "fixture.json.gz"))).toString("utf8").replace(/\n$/, "").replace(/<\//g, "<\\/");
const t = fs.readFileSync(TEMPLATE, "utf8"); if (t.split("__DATA__").length !== 2) throw new Error("sivupohjassa pitää olla tasan yksi __DATA__");
fs.writeFileSync(path.join(tmp, "index.html"), t.replace("__DATA__", () => data));
fs.mkdirSync(path.join(tmp, "fonts")); for (const f of fs.readdirSync(path.join(ROOT, "fonts"))) fs.copyFileSync(path.join(ROOT, "fonts", f), path.join(tmp, "fonts", f));
for (const f of ["favicon.svg", "favicon-96.png", "favicon.ico", "apple-touch-icon.png"]) if (fs.existsSync(path.join(ROOT, f))) fs.copyFileSync(path.join(ROOT, f), path.join(tmp, f));
const TYPES = { ".html": "text/html; charset=utf-8", ".svg": "image/svg+xml", ".woff2": "font/woff2", ".png": "image/png" };
const srv = http.createServer((q, r) => { const f = path.join(tmp, decodeURIComponent(q.url.split("?")[0]) === "/" ? "index.html" : decodeURIComponent(q.url.split("?")[0]));
  if (!f.startsWith(tmp) || !fs.existsSync(f)) { r.writeHead(404); r.end(); return; }
  r.writeHead(200, { "Content-Type": TYPES[path.extname(f)] || "application/octet-stream" }); fs.createReadStream(f).pipe(r); });

(async () => {
  await new Promise(ok => srv.listen(0, "127.0.0.1", ok)); const base = `http://127.0.0.1:${srv.address().port}/`;
  fs.mkdirSync(OUT, { recursive: true });
  const b = await chromium.launch(); let bad = 0; const rows = [];
  for (const w of WIDTHS) for (const v of VIEWS) {
    const ctx = await b.newContext({ viewport: { width: w, height: 900 }, deviceScaleFactor: 1, reducedMotion: "reduce" });
    const p = await ctx.newPage(); await p.clock.setFixedTime(new Date("2026-11-18T10:00:00+02:00"));
    await p.goto(base + "#" + v); await p.waitForTimeout(1200);
    await p.evaluate(() => document.fonts.ready);
    const name = `${v}-${w}.png`, cur = await p.screenshot({ fullPage: true, animations: "disabled" });
    if (UPDATE) { fs.writeFileSync(path.join(DIR, name), cur); rows.push(`${name}: pohjakuva päivitetty`); await ctx.close(); continue; }
    const refF = path.join(DIR, name);
    if (!fs.existsSync(refF)) { rows.push(`${name}: ei pohjakuvaa (aja --update)`); bad++; await ctx.close(); continue; }
    // vertailu selaimessa: ero kanavittain yli 24 lasketaan poikkeavaksi pikseliksi; erokuvaan punaisella
    const res = await p.evaluate(async ([a, c]) => {
      const load = s => new Promise(ok => { const i = new Image(); i.onload = () => ok(i); i.src = "data:image/png;base64," + s; });
      const [A, C] = await Promise.all([load(a), load(c)]);
      if (A.width !== C.width || A.height !== C.height) return { size: `${A.width}x${A.height} -> ${C.width}x${C.height}` };
      const cv = document.createElement("canvas"); cv.width = A.width; cv.height = A.height; const g = cv.getContext("2d");
      g.drawImage(A, 0, 0); const da = g.getImageData(0, 0, A.width, A.height);
      g.drawImage(C, 0, 0); const dc = g.getImageData(0, 0, A.width, A.height);
      let n = 0; const out = g.createImageData(A.width, A.height);
      for (let k = 0; k < da.data.length; k += 4) {
        const d = Math.max(Math.abs(da.data[k] - dc.data[k]), Math.abs(da.data[k + 1] - dc.data[k + 1]), Math.abs(da.data[k + 2] - dc.data[k + 2]));
        if (d > 24) { n++; out.data[k] = 230; out.data[k + 1] = 30; out.data[k + 2] = 30; out.data[k + 3] = 255; }
        else { const y = (dc.data[k] + dc.data[k + 1] + dc.data[k + 2]) / 3; out.data[k] = out.data[k + 1] = out.data[k + 2] = 200 + y * 0.2; out.data[k + 3] = 255; } }
      g.putImageData(out, 0, 0); return { share: n / (A.width * A.height), diff: cv.toDataURL("image/png").split(",")[1] };
    }, [fs.readFileSync(refF).toString("base64"), cur.toString("base64")]);
    if (res.size) { rows.push(`${name}: koko muuttui ${res.size}`); bad++; fs.writeFileSync(path.join(OUT, name), cur); }
    else { const pct = (100 * res.share).toFixed(2) + " %";
      if (res.share > RAJA) { bad++; rows.push(`${name}: POIKKEAA ${pct}`); fs.writeFileSync(path.join(OUT, name), cur); fs.writeFileSync(path.join(OUT, "ero-" + name), Buffer.from(res.diff, "base64")); }
      else rows.push(`${name}: ok ${pct}`); }
    await ctx.close();
  }
  await b.close(); srv.close(); fs.rmSync(tmp, { recursive: true, force: true });
  console.log(rows.join("\n")); if (!UPDATE && bad) { console.log(`Visuaalinen testi: ${bad} poikkeamaa, erokuvat: ${OUT}`); process.exit(1); }
  console.log(UPDATE ? "Pohjakuvat päivitetty." : "Visuaalinen testi ok");
})().catch(e => { console.error(e); process.exit(2); });
