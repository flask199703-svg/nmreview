/* Optional developer smoke test. Run against the local app with synthetic data. */
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { chromium } = require(process.env.NMREVIEW_PLAYWRIGHT || 'playwright');

async function main() {
  const url = process.env.NMREVIEW_URL || 'http://127.0.0.1:4787';
  const output = path.resolve(process.env.NMREVIEW_QA || 'verification-local');
  fs.mkdirSync(output, { recursive: true });
  const browser = await chromium.launch({
    headless: true,
    executablePath: process.env.NMREVIEW_BROWSER || undefined
  });
  const errors = [];
  const externalRequests = [];
  const page = await browser.newPage({ viewport: { width: 1536, height: 1080 } });
  page.on('pageerror', error => errors.push(error.message));
  page.on('request', request => {
    if (/^https?:/.test(request.url()) && !request.url().startsWith(url)) externalRequests.push(request.url());
  });
  async function select(label, text) {
    const control = page.getByRole('combobox', { name: label, exact: true });
    await control.fill(text);
    const wrapper = page.locator('.selectize-control').filter({ has: control });
    await wrapper.locator('.selectize-dropdown').getByText(text, { exact: true }).click();
  }
  async function tab(label) {
    await page.getByRole('tab', { name: label, exact: true }).click();
  }
  async function noOutputErrors() {
    await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'));
    assert.equal(await page.locator('.shiny-output-error:not(.shiny-output-error-validation)').count(), 0);
    try {
      await page.waitForFunction(() => document.documentElement.scrollWidth <= innerWidth + 2);
    } catch (error) {
      console.error(await page.evaluate(() => ({ width: innerWidth, documentWidth: document.documentElement.scrollWidth,
        overflow: [...document.querySelectorAll('body *')].filter(element => element.getBoundingClientRect().right > innerWidth + 3).slice(0, 12).map(element => ({ id: element.id, className: element.className, right: element.getBoundingClientRect().right })) })));
      throw error;
    }
  }
  try {
    await page.goto(url);
    await page.getByRole('textbox', { name: 'NONMEM output folder', exact: true }).fill(path.resolve('inst/extdata/demo'));
    await page.getByRole('button', { name: 'Scan folder', exact: true }).click();
    await page.getByRole('cell', { name: 'run103:p1', exact: true }).waitFor();
    await page.getByRole('button', { name: 'Load synthetic demo' }).click();
    await page.waitForFunction(() => document.getElementById('source_path').value === '');
    await page.getByRole('row', { name: /run103:p1 Synthetic/ }).click();
    await page.getByRole('button', { name: 'Set as comparison', exact: true }).click();
    await page.getByRole('button', { name: 'Open comparison' }).click();
    await page.getByText('observation key sets differ', { exact: true }).waitFor();
    await select('Comparison model', 'run102:p1 - Synthetic oral PK review fixture 102');
    await page.getByText('Delta OFV (comparison - reference) -21.453', { exact: true }).waitFor();
    await select('Reference model', 'run102:p1 - Synthetic oral PK review fixture 102');
    await select('Comparison model', 'run101:p1 - Synthetic oral PK review fixture 101');
    await page.getByText('Delta OFV (comparison - reference) 21.453', { exact: true }).waitFor();
    await select('Reference model', 'run101:p1 - Synthetic oral PK review fixture 101');
    await select('Comparison model', 'run102:p1 - Synthetic oral PK review fixture 102');
    await page.getByRole('row', { name: /^20 20120 12/ }).click();
    await page.locator('#point_detail').getByText('sdtab101', { exact: false }).waitFor();
    await page.waitForFunction(() => document.querySelector('#individual_plot img')?.naturalWidth > 0);
    await noOutputErrors();
    await page.screenshot({ path: path.join(output, 'compare.png'), fullPage: true });
    for (const diagnostic of ['DV vs PRED', 'DV vs IPRED', 'CWRES vs prediction', 'CWRES vs time']) {
      await select('Diagnostic', diagnostic);
      await page.waitForFunction(() => document.querySelector('#reference_plot img')?.naturalWidth > 0);
      await noOutputErrors();
    }
    await tab('Parameters & checks');
    await page.locator('#reference_parameters tbody tr').first().waitFor();
    await noOutputErrors();
    await tab('Bootstrap');
    await page.getByText('8 replicates | 5 included', { exact: true }).waitFor();
    await page.waitForFunction(() => document.querySelector('#bootstrap_plot img')?.naturalWidth > 0);
    await noOutputErrors();
    await page.screenshot({ path: path.join(output, 'bootstrap.png'), fullPage: true });
    await tab('VPC / pcVPC');
    await page.getByText('run101 | Prediction corrected', { exact: true }).waitFor();
    await page.waitForFunction(() => { const image = document.querySelector('#vpc_reference_plot img'); return image?.naturalWidth > 0 && image.alt.startsWith('9 stored percentile'); });
    await noOutputErrors();
    await page.screenshot({ path: path.join(output, 'pcvpc.png'), fullPage: true });
    await select('Comparison VPC analysis', 'run103 vpc:run103:vpc');
    await page.getByText('run103 | Ordinary VPC', { exact: true }).waitFor();
    await page.getByText('Correction differs between the selected analyses.', { exact: false }).waitFor();
    await select('Comparison VPC analysis', 'run102 vpc:run102:pcVPC');
    await tab('Compact archive');
    await page.getByRole('textbox', { name: 'Or save to a local folder', exact: true }).fill(output);
    await page.getByRole('button', { name: 'Save and verify', exact: true }).click();
    await page.getByText('Saved and reopened successfully', { exact: true }).waitFor();
    const archive = (await page.locator('#archive_result code').innerText()).trim();
    assert.ok(fs.statSync(archive).size > 0);
    const downloadPromise = page.waitForEvent('download');
    await page.locator('#download_archive').click();
    const download = await downloadPromise;
    const downloadedArchive = path.join(output, 'downloaded.sqlite');
    await download.saveAs(downloadedArchive);
    assert.ok(fs.statSync(downloadedArchive).size > 0);

    const fresh = await browser.newPage({ viewport: { width: 1536, height: 1080 } });
    fresh.on('pageerror', error => errors.push(error.message));
    await fresh.goto(url);
    await fresh.locator('#archive_file').setInputFiles(archive);
    await fresh.getByRole('cell', { name: 'run101:p1', exact: true }).waitFor();
    await fresh.getByRole('button', { name: 'Open comparison' }).click();
    await fresh.getByText('Delta OFV (comparison - reference) -21.453', { exact: true }).waitFor();
    await fresh.getByRole('tab', { name: 'Bootstrap', exact: true }).click();
    await fresh.getByText('8 replicates | 5 included', { exact: true }).waitFor();
    await fresh.getByRole('tab', { name: 'VPC / pcVPC', exact: true }).click();
    await fresh.getByText('run101 | Prediction corrected', { exact: true }).waitFor();
    await fresh.close();

    await page.setViewportSize({ width: 768, height: 1024 });
    await tab('Compare');
    await noOutputErrors();
    await page.screenshot({ path: path.join(output, 'compare-tablet.png'), fullPage: true });
    assert.deepEqual(errors, []);
    assert.deepEqual(externalRequests, []);
    const result = { passed: true, archiveBytes: fs.statSync(archive).size, archive: path.basename(archive),
      scenarios: ['all-run selection', 'arbitrary reference/comparison', 'mismatched data gate',
        'linked observation', 'four diagnostics', 'parameter checks', 'bootstrap', 'pcVPC',
        'compact save', 'fresh browser archive reopen', 'tablet layout', 'local-only requests'],
      screenshots: ['compare.png', 'bootstrap.png', 'pcvpc.png', 'compare-tablet.png'] };
    fs.writeFileSync(path.join(output, 'browser-results.json'), JSON.stringify(result, null, 2));
    console.log(JSON.stringify(result));
  } finally {
    await browser.close();
  }
}
main().catch(error => { console.error(error); process.exitCode = 1; });
