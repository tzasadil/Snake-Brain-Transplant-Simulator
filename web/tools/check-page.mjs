// Headless browser check of the page: loads it, optionally picks a brain, samples the stats while it trains,
// saves a screenshot, and reports console errors. Uses the system Chrome, so nothing is downloaded.
//
// From web/, after `pnpm build` and `node_modules/.bin/vite preview --port 4179`:
//   node tools/check-page.mjs http://localhost:4179/ worm 30 /tmp/page.png
// Arguments: URL, brain (fly, worm, net; default: the page default), seconds to sample (default 20), screenshot path.
import { chromium } from "playwright-core";

const [url, brain, seconds = "20", screenshot = "page.png"] = process.argv.slice(2);
const browser = await chromium.launch({ channel: "chrome" });
const page = await browser.newPage({ viewport: { width: 1200, height: 860 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
page.on("console", (m) => m.type() === "error" && errors.push(m.text()));

await page.goto(url);
if (brain) {
  await page.waitForFunction((b) => !document.querySelector(`#brain option[value="${b}"]`).disabled, brain);
  await page.selectOption("#brain", brain);
}

const ids = ["generation", "score", "outcome", "trained", "latest", "best"];
const stats = () => page.evaluate((ids) => ids.map((id) => document.getElementById(id).textContent).join(" "), ids);
console.log(`time  ${ids.join(" ")}`);
for (let t = 5; t <= Number(seconds); t += 5) {
  await page.waitForTimeout(5000);
  console.log(`${t}s   ${await stats()}`);
}

await page.screenshot({ path: screenshot });
console.log(`screenshot ${screenshot}`);
console.log("errors:", errors.length ? errors : "none");
await browser.close();
process.exit(errors.length ? 1 : 0);
