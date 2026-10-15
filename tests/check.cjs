/* Automated checks for the study guide.
   Run locally:  npm install && npx playwright install chromium && npm test
   1. The page loads without script errors, on desktop and phone widths, light and dark.
   2. Every practice question accepts its own correct answer (and only one MC option is marked correct).
   3. Numbers in worked examples and questions are recomputed independently.
   4. Every demo produces output, exam mode and search work.
   5. axe-core finds no serious or critical WCAG 2 A/AA problems. */
const path = require('path');
const fs = require('fs');
const { chromium } = require('playwright');

const FILE = 'file://' + path.resolve(__dirname, '..', 'index.html');
let failures = 0;
const ok = (cond, msg) => { if (cond) { console.log('  ok   ' + msg); } else { failures++; console.log('  FAIL ' + msg); } };
const near = (a, b, tol = 1e-3) => Math.abs(a - b) <= tol;

function lchoose(n, k) { let s = 0; for (let i = 1; i <= k; i++) s += Math.log(n - k + i) - Math.log(i); return s; }
function binomTail(n, k) { let s = 0; for (let j = k; j <= n; j++) s += Math.exp(lchoose(n, j) - n * Math.log(2)); return s; }

(async () => {
  const browser = await chromium.launch();
  const page = await browser.newPage({ viewport: { width: 1000, height: 900 } });
  const errors = [];
  page.on('pageerror', e => errors.push(e.message));
  page.on('console', m => { if (m.type() === 'error' && !/fonts\.g|ERR_|net::/.test(m.text())) errors.push(m.text()); });
  await page.route(/fonts\.(googleapis|gstatic)\.com/, r => r.abort());
  await page.goto(FILE);
  await page.waitForTimeout(300);

  console.log('1. Pages');
  const pages = await page.evaluate(() => [...document.querySelectorAll('[data-page]')].map(p => p.id));
  ok(pages.length === 11, 'eleven pages (start, basics, 7 lectures, cheat sheet, exam): ' + pages.join(', '));
  for (const id of pages) {
    await page.evaluate(id => { location.hash = '#' + id; }, id);
    await page.waitForTimeout(60);
    const vis = await page.evaluate(id => !document.getElementById(id).hidden, id);
    ok(vis, 'page ' + id + ' shows from its link');
  }

  console.log('2. Question bank');
  const qb = await page.evaluate(() => QB);
  const ids = new Set();
  for (const q of qb) {
    ok(!ids.has(q.id), 'unique id ' + q.id); ids.add(q.id);
    ok(!!q.explain || q.type === 'open', q.id + ' has an explanation');
    if (q.type === 'mc') {
      ok(q.options.filter(o => o.ok).length === 1, q.id + ' has exactly one correct option');
      ok(q.options.every(o => o.fb), q.id + ' gives feedback for every option');
    }
    if (q.type === 'num') ok((q.wrong || []).every(w => Math.abs(w.v - q.answer) > (q.tol || 0)), q.id + ' wrong-answer feedback never overlaps the right answer');
  }
  const N = { 'l1-a2': 0.08 + 0.10 * 0.15, 'l2-a1': 60 / 150, 'l2-a2': 4 / Math.sqrt(100), 'l3-a2': 0.025 * 2000,
    'l4-a1': (80 - 70) - 4, 'l4-a2': ((5 - 3) + (4 - 4) + (2 - 5)) / 3, 'l5-a1': 0.30 / (0.70 - 0.20),
    'l6-a1': 0.15 / (0.7 - 0.2), 'l7-a1': (75 - 50) - (55 - 40), 'l7-a2': (29 - 22) - (36 - 32) };
  for (const q of qb.filter(q => q.type === 'num')) ok(N[q.id] !== undefined && near(N[q.id], q.answer, q.tol || 1e-6), q.id + ' numeric answer recomputed: ' + (N[q.id] && N[q.id].toFixed(4)));

  console.log('3. Practice questions accept their correct answers');
  for (const q of qb) {
    const pg = q.topic === 'design' ? 'cheat' : q.topic;
    await page.evaluate(pg => { location.hash = '#' + pg; }, pg);
    const card = page.locator(`#${pg} .qcard[data-q="${q.id}"]`);
    if (q.type === 'mc') {
      const idx = await card.evaluate((c, q) => { const tmp = document.createElement('div'); const right = q.options.find(o => o.ok).t; tmp.innerHTML = right; const want = tmp.textContent.replace(/\s+/g, ' ').trim(); return [...c.querySelectorAll('.opt')].findIndex(b => b.lastElementChild.textContent.replace(/\s+/g, ' ').trim() === want); }, q);
      await card.locator('.opt').nth(idx).click();
    } else if (q.type === 'num') { await card.locator('input').fill(String(q.answer)); await card.locator('button', { hasText: 'Check' }).click(); }
    else { await card.locator('button', { hasText: 'Compare' }).click(); await card.locator('button', { hasText: 'key points' }).click(); }
    if (q.type !== 'open') ok(/Correct/.test(await card.locator('.verdict').first().innerText()), q.id + ' marked correct');
  }
  const prog = await page.locator('#progress').innerText();
  ok(/practised 7/.test(prog), 'all seven lectures count as practised: ' + prog.replace(/\s+/g, ' '));

  console.log('4. Worked examples in the text');
  const text = await page.evaluate(() => document.body.textContent.replace(/\s+/g, ' '));
  const has = s => text.includes(s);
  ok(near(0.355 / 0.741, 0.479) && has('0.355 / 0.741 = 0.479'), 'KIPP LATE 0.355/0.741 = 0.479');
  ok((14 - 62) - (614 - 565) === -97 && has('DiD = (−48) − (+49) = −97'), "Snow's DiD = −97");
  const po = await page.evaluate(() => { const t = [...document.querySelectorAll('#l4 table')].find(t => /Y\(1\) treated/.test(t.textContent)); return [...t.querySelectorAll('tr')].slice(1).map(r => [...r.cells].slice(1, 3).map(c => +c.textContent)); });
  const ate = po.reduce((s, r) => s + r[0] - r[1], 0) / po.length;
  ok(near(ate, -1.5) && has('−9 / 6 = −1.5'), 'potential-outcomes ATE = −1.5 (from the table)');
  let tot = 0, cnt = 0;
  for (let m = 0; m < 64; m++) { const T = [0, 1, 2, 3, 4, 5].filter(i => m >> i & 1); if (T.length !== 3) continue; const C = [0, 1, 2, 3, 4, 5].filter(i => !(m >> i & 1)); tot += T.reduce((s, i) => s + po[i][0], 0) / 3 - C.reduce((s, i) => s + po[i][1], 0) / 3; cnt++; }
  ok(cnt === 20 && near(tot / cnt, -1.5), 'average over all 20 random splits equals the ATE');
  ok(near((4 + 5 + 3) / 3 - (6 + 7 + 3) / 3, -1.333, 0.001) && near((8 + 3 + 1) / 3 - (6 + 7 + 4) / 3, -1.667, 0.001), 'the two example splits give −1.33 and −1.67');
  const A = [10, 16, 21, 24], B = [20, 22, 27, 29], Nn = [30, 32, 35, 36];
  const att = [(A[1] - A[0]) - ((B[1] - B[0]) + (Nn[1] - Nn[0])) / 2, (A[2] - A[0]) - (Nn[2] - Nn[0]), (A[3] - A[0]) - (Nn[3] - Nn[0]), (B[2] - B[1]) - (Nn[2] - Nn[1]), (B[3] - B[1]) - (Nn[3] - Nn[1])];
  ok(JSON.stringify(att) === '[4,6,8,2,3]', 'Callaway & Sant\'Anna cells A: 4, 6, 8 and B: 2, 3');
  ok((B[2] - B[1]) - (A[2] - A[1]) === 0, 'forbidden TWFE comparison gives 0');
  ok(near(0.191 * 60.4 + 0.284 * -98.4 + 0.525 * 6.5, -13.0, 0.05), 'Bacon decomposition sums to −13.0');
  ok(near(0.2 * 1 + 0.8 * 2, 1.8), 'conditional DiD example = 1.8');
  ok(near(97 / 500, 0.194) && near(Math.log10(Math.exp(lchoose(500, 97) - 500 * Math.log(2))), Math.log10(9.3e-46), 0.02), 'coin: MLE 0.194 and P(97 heads | p = 0.5) ≈ 9.3 × 10⁻⁴⁶');
  ok(near(2 * binomTail(20, 16), 0.0118, 0.0005), 'p-value explainer: 16 of 20 heads gives p ≈ 0.012');
  ok(near(1 - 1 / Math.E, 0.632), 'bootstrap maximum: 1 − 1/e ≈ 63 %');
  ok(near(5.179 + 11.601, 16.78, 0.001), 'Lab 4: ATT + selection bias = naive difference');
  ok(near(0.332 / 0.768, 0.432, 0.001), 'Lab 5: Wald = reduced form / first stage = 0.432');
  ok(near(-0.949 + -3.975, -4.924), 'Lab 7 distractor: total change in city A = −4.92');

  console.log('5. Demos, exam, search');
  const demos = { l1: '[data-demo=ols] [data-out]', l2: '[data-demo=mle] [data-out]', l3: '#b-origmean', l4: '[data-demo=sel] [data-out]', l5: '[data-demo=iv] [data-out]', l6: '[data-demo=rd] [data-out]', l7: '[data-demo=did] [data-out]' };
  for (const [pg, sel] of Object.entries(demos)) { await page.evaluate(pg => { location.hash = '#' + pg; }, pg); await page.waitForTimeout(150); ok((await page.locator(sel).innerText()).length > 10, pg + ' demo shows results'); }
  await page.evaluate(() => { location.hash = '#l7'; });
  await page.locator('[data-demo=did] [data-k=ex]').fill('0.5'); await page.waitForTimeout(100);
  ok(/bias 1\.50/.test(await page.locator('[data-demo=did] [data-out]').innerText()), 'DiD demo: extra trend 0.5 gives bias 1.50');
  await page.evaluate(() => { location.hash = '#exam'; });
  await page.selectOption('#ex-count', '10'); await page.locator('#ex-start').click();
  ok(!(await page.locator('.tab[data-t=cheat]').isVisible()), 'cheat sheet hidden during exam');
  for (let i = 0; i < 10; i++) {
    const card = page.locator('#ex-q .qcard');
    if (await card.locator('.opt').count()) await card.locator('.opt').first().click();
    else if (await card.locator('input').count()) { await card.locator('input').fill('0'); await card.locator('button', { hasText: 'Check' }).click(); }
    else { await card.locator('button', { hasText: 'Compare' }).click(); await card.locator('button', { hasText: 'Partly' }).click(); }
    await page.locator('#ex-next').click();
  }
  ok(/Results by topic/.test(await page.locator('#ex-results').innerText()), 'exam shows results by topic');
  await page.locator('#search-btn').click(); await page.locator('#s-input').fill('bandwidth');
  ok((await page.locator('.s-hit').count()) >= 3, 'search finds "bandwidth" in several places');
  await page.keyboard.press('Escape');
  ok(errors.length === 0, 'no script errors' + (errors.length ? ': ' + errors.join(' | ') : ''));

  console.log('6. Layout and accessibility');
  const axePath = require.resolve('axe-core/axe.min.js');
  for (const [w, scheme] of [[375, 'light'], [1000, 'light'], [1000, 'dark']]) {
    const p = await browser.newPage({ viewport: { width: w, height: 800 }, colorScheme: scheme });
    await p.route(/fonts\.(googleapis|gstatic)\.com/, r => r.abort());
    await p.goto(FILE); await p.addScriptTag({ path: axePath });
    for (const id of pages) {
      await p.evaluate(id => { location.hash = '#' + id; }, id); await p.waitForTimeout(80);
      const over = await p.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth);
      if (w === 375) ok(over <= 0, `no sideways scrolling on a phone: ${id}`);
      const v = await p.evaluate(async () => (await axe.run(document, { runOnly: ['wcag2a', 'wcag2aa'] })).violations.filter(x => x.impact === 'serious' || x.impact === 'critical').map(x => x.id + ' (' + x.nodes.length + ')'));
      ok(v.length === 0, `axe ${scheme} ${w}px ${id}` + (v.length ? ': ' + v.join(', ') : ''));
    }
    await p.close();
  }

  await browser.close();
  console.log(failures ? `\n${failures} check(s) failed` : '\nAll checks passed');
  process.exit(failures ? 1 : 0);
})();
