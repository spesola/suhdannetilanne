# Upottaa clock.jsonin sivupohjaan (sama kasittely kuin pipeline.R:ssa): python3 tools/kokoa_sivu.py [pohja] [data] [ulos]
import sys
pohja, data, ulos = (sys.argv[1:] + ["clock_template.html", "clock.json", "suhdannetilanne.html"][len(sys.argv) - 1:])[:3]
t = open(pohja, encoding="utf-8").read()
d = open(data, encoding="utf-8").read().rstrip("\n").replace("</", "<\\/")
assert t.count("__DATA__") == 1, "sivupohjassa pitaa olla tasan yksi __DATA__"
open(ulos, "w", encoding="utf-8").write(t.replace("__DATA__", d, 1))
print("Kirjoitettu", ulos)
