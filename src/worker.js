// Sivu tarjoillaan staattisina tiedostoina (public/). Ajastus kaynnistaa GitHubin paivitysajon,
// koska GitHubin oma ajastus viivastyy ja kytkeytyy pois, jos repoon ei tule committeja 60 paivaan.
// Vaatii salaisuuden GH_TOKEN (fine-grained token, vain tama repo, Actions: Read and write).
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
    const r = await fetch("https://api.github.com/repos/spesola/suhdannetilanne/actions/workflows/update.yml/dispatches", {
      method: "POST",
      headers: { Authorization: `Bearer ${env.GH_TOKEN}`, Accept: "application/vnd.github+json",
                 "X-GitHub-Api-Version": "2022-11-28", "User-Agent": "suhdannetilanne-cron" },
      body: JSON.stringify({ ref: "main" })
    });
    console.log(`dispatch ${event.cron}: ${r.status}`);
    if (r.status !== 204) throw new Error(`GitHub vastasi ${r.status}: ${await r.text()}`);
  }
};
