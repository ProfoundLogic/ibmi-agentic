/* ==================================================================
 * Offline pre-flight for the two Putaway screens.
 *
 *   node gtwms/tools/preflight-put.js
 *
 * puthome's scan band is PINNED, so it is asserted by actually scrolling. A
 * band that has quietly stopped pinning looks identical in a screenshot.
 *
 * putdest gets three renders, because its states are genuinely different
 * shapes: the suggestion accepted, a scanned override, and the after-the-fact
 * receipt panel. The no-destination case is the one that must still let the
 * operator leave.
 * ================================================================== */

'use strict';

const { run } = require('./preflight-lib.js');

const HOME_DECLARED = ['action', 'scanval', 'langpref', 'opername', 'msg',
                       'msgkind', 'nrows', 'nunits', 'hitline', 'stgsfl'];

const DEST_DECLARED = ['action', 'scanval', 'langpref', 'opername', 'msg',
                       'msgkind', 'psku', 'pdesc', 'pdept', 'puom', 'pcase',
                       'pimg', 'psscc', 'pfrom', 'pavail', 'pto', 'ptotyp',
                       'ptozone', 'ptoroom', 'ptoqty', 'prank', 'ptook',
                       'pqty', 'pdone', 'pfrombef', 'pfromaft', 'ptobef',
                       'ptoaft', 'palts'];

const COMMON = (lang) => ({
  action: '', scanval: '', langpref: lang, msg: '', msgkind: '',
  opername: lang === 'FR' ? 'Marie Tremblay' : 'James Okonkwo'
});

/* Enough rows to make the page scroll, so the pinned band has something to be
 * pinned over. One row has NO suggestion, which is a real state -- an item with
 * no home and no empty face -- and it renders differently. */
const STAGED = (lang) => {
  const rows = [];
  for (let i = 1; i <= 8; i++) {
    rows.push({
      _rrn: i, sseq: i,
      ssku: 'GROC-0000' + (10 + i),
      sdesc: lang === 'FR'
        ? 'Riz blanc à grain long - 2 kg, sac refermable'
        : 'Long Grain White Rice - 2 kg resealable bag',
      sdept: 'GROC', suom: 'EA', simg: 0,
      sqty: 120 + i * 12, scase: 12,
      sloc: 'STAGE01',
      ssugg: i === 4 ? '' : 'A0' + (10 + i) + '41',
      ssugtyp: i === 4 ? '' : (i % 3 === 0 ? 'BULK' : 'PICK'),
      ssugrm: i === 4 ? 0 : 300 - i * 7,
      ssugrnk: i === 4 ? 0 : (i % 3 === 0 ? 2 : 1),
      ssscc: '306141410000000044'
    });
  }
  /* A description long enough to wrap, in the language where it is longest. */
  rows[1].sdesc = lang === 'FR'
    ? 'Ensemble de meubles de patio quatre pièces avec coussins résistants'
    : 'Four Piece Patio Furniture Set with Weather Resistant Cushions';
  return rows;
};

const ALTS = JSON.stringify([
  { l: 'A01141', t: 'PICK', z: 'A', r: 288, k: 1 },
  { l: 'D01032', t: 'BULK', z: 'D', r: 265, k: 2 },
  { l: 'D01044', t: 'BULK', z: 'D', r: 130, k: 2 },
  { l: 'A01202', t: 'PICK', z: 'A', r: 360, k: 3 }
]);

const ITEM = (lang) => ({
  psku: 'GROC-000011',
  pdesc: lang === 'FR' ? 'Pâtes de blé entier - 750 g' : 'Whole Wheat Pasta - 750 g',
  pdept: 'GROC', puom: 'EA', pcase: 12, pimg: 0,
  psscc: '306141410000000044',
  pfrom: 'STAGE01', pavail: 144
});

const SCREENS = [
  {
    name: 'puthome',
    tpl: ['gtpud', 'puthome.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtpud/puthome.css'],
    declared: HOME_DECLARED,
    controls: ['#gt-scan-start', '#gt-scan-manual', '.gt-btn-primary', '.gt-put-hit'],
    minTarget: ['#gt-scan-manual', '#gt-scan-start'],
    /* The whole point of the band: the queue is long and the operator scans
     * throughout, so the camera and the box must still be there and still be
     * hit-testable after scrolling. */
    sticky: { band: '.gt-put-top',
              keep: ['#gt-scan-start', '#gt-scan-manual'] },
    data: (lang) => Object.assign(COMMON(lang), {
      nrows: 8, nunits: 1240, hitline: 3, stgsfl: STAGED(lang)
    })
  },
  {
    name: 'puthome-clear',
    tpl: ['gtpud', 'puthome.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtpud/puthome.css'],
    declared: HOME_DECLARED,
    controls: ['#gt-scan-start', '#gt-scan-manual', '.gt-btn-primary'],
    data: (lang) => Object.assign(COMMON(lang), {
      nrows: 0, nunits: 0, hitline: 0, stgsfl: []
    })
  },
  {
    name: 'putdest-suggested',
    tpl: ['gtpdd', 'putdest.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtpdd/putdest.css'],
    declared: DEST_DECLARED,
    controls: ['#gt-pd-qty', '#gt-scan-manual', '#gt-scan-start',
               '.gt-btn-primary'],
    minTarget: ['#gt-pd-qty', '.gt-btn-primary'],
    data: (lang) => Object.assign(COMMON(lang), ITEM(lang), {
      pto: 'A01141', ptotyp: 'PICK', ptozone: 'A', ptoroom: 288, ptoqty: 12,
      prank: 1, ptook: 'N', pqty: 144, pdone: 'N',
      pfrombef: 0, pfromaft: 0, ptobef: 0, ptoaft: 0, palts: ALTS
    })
  },
  {
    /* A scanned override. The badge changes, and a third footer button appears
     * -- three buttons on a 360px row is the case that breaks footers. */
    name: 'putdest-override',
    tpl: ['gtpdd', 'putdest.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtpdd/putdest.css'],
    declared: DEST_DECLARED,
    controls: ['#gt-pd-qty', '.gt-btn-primary', '.gt-btn-ghost'],
    minTarget: ['#gt-pd-qty', '.gt-btn-primary'],
    data: (lang) => Object.assign(COMMON(lang), ITEM(lang), {
      msg: lang === 'FR' ? 'Destination D01032 retenue.'
                         : 'Destination set to D01032.',
      msgkind: 'GOOD',
      pto: 'D01032', ptotyp: 'BULK', ptozone: 'D', ptoroom: 265, ptoqty: 0,
      prank: 0, ptook: 'Y', pqty: 144, pdone: 'N',
      pfrombef: 0, pfromaft: 0, ptobef: 0, ptoaft: 0, palts: ALTS
    })
  },
  {
    /* No destination at all. The operator must still be able to leave, and the
     * state has to be named rather than showing an empty gap. */
    name: 'putdest-nodest',
    tpl: ['gtpdd', 'putdest.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtpdd/putdest.css'],
    declared: DEST_DECLARED,
    controls: ['.gt-btn-ghost', '.gt-btn-primary'],
    minTarget: ['.gt-btn-ghost'],
    data: (lang) => Object.assign(COMMON(lang), ITEM(lang), {
      pto: '', ptotyp: '', ptozone: '', ptoroom: 0, ptoqty: 0,
      prank: 0, ptook: 'N', pqty: 144, pdone: 'N',
      pfrombef: 0, pfromaft: 0, ptobef: 0, ptoaft: 0, palts: '[]'
    })
  },
  {
    /* After booking: the receipt panel is added, so it is a different render. */
    name: 'putdest-done',
    tpl: ['gtpdd', 'putdest.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtpdd/putdest.css'],
    declared: DEST_DECLARED,
    controls: ['.gt-btn-primary'],
    data: (lang) => Object.assign(COMMON(lang), ITEM(lang), {
      msg: lang === 'FR' ? '144 rangé en A01141.' : '144 put away in A01141.',
      msgkind: 'GOOD',
      pavail: 0,
      pto: 'A01141', ptotyp: 'PICK', ptozone: 'A', ptoroom: 144, ptoqty: 156,
      prank: 1, ptook: 'N', pqty: 144, pdone: 'Y',
      pfrombef: 144, pfromaft: 0, ptobef: 12, ptoaft: 156, palts: ALTS
    })
  }
];

run('Putaway pre-flight', SCREENS);
