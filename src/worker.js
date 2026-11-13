// Sivu tarjoillaan staattisina tiedostoina (public/). Ajastus kaynnistaa GitHubin paivitysajon,
// koska GitHubin oma ajastus viivastyy ja kytkeytyy pois, jos repoon ei tule committeja 60 paivaan.
// Vaatii salaisuuden GH_TOKEN (fine-grained token, vain tama repo, Actions: Read and write).
const BACKUP = ["35 6 * * 1-5", "5 11 * * 1-5"];
export default {
  async fetch(request, env) {
    const url = new URL(request.url);
    // yksi osoite: https ja ilman www-etuliitetta
    if (url.hostname === "www.suhdannetilanne.fi" || url.protocol === "http:") {
      url.hostname = "suhdannetilanne.fi"; url.protocol = "https:";
      return Response.redirect(url.toString(), 301);
    }
    const res = await env.ASSETS.fetch(request);
    // tuntematon osoite etusivulle
    if (res.status === 404 && (request.method === "GET" || request.method === "HEAD")) return Response.redirect(url.origin + "/", 302);
    return res;
  },
  async scheduled(event, env, ctx) {
    if (!env.GH_TOKEN) { console.log("GH_TOKEN puuttuu, ajoa ei kaynnistetty"); return; }
    const api = "https://api.github.com/repos/spesola/suhdannetilanne/actions";
    const H = { Authorization: `Bearer ${env.GH_TOKEN}`, Accept: "application/vnd.github+json",
                "X-GitHub-Api-Version": "2022-11-28", "User-Agent": "suhdannetilanne-cron" };
    // varmistusajastus (puoli tuntia paaajastuksen jalkeen): kaynnistaa ajon vain, jos sita ei ole jo kaynnistetty
    if (BACKUP.includes(event.cron)) {
      const since = new Date(event.scheduledTime - 45 * 60e3).toISOString().slice(0, 19) + "Z";
      const r = await fetch(`${api}/workflows/update.yml/runs?per_page=1&created=%3E%3D${since}`, { headers: H });
      if (r.ok) { const j = await r.json(); if (j.total_count > 0) { console.log(`${event.cron}: ajo jo kaynnissa tai tehty`); return; } }
      else console.log(`${event.cron}: ajojen haku ${r.status}`);
    }
    let last = "";
    for (let k = 0; k < 3; k++) {                                       // uusi yritys, jos GitHub ei vastaa heti
      const r = await fetch(`${api}/workflows/update.yml/dispatches`, { method: "POST", headers: H, body: JSON.stringify({ ref: "main" }) });
      console.log(`dispatch ${event.cron} yritys ${k + 1}: ${r.status}`);
      if (r.status === 204) return;
      last = `${r.status}: ${await r.text()}`; await new Promise(ok => setTimeout(ok, 20000));
    }
    throw new Error(`GitHub vastasi ${last}`);
  }
};
