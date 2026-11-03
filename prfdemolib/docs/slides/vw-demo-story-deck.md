# From Green Screen to Insight

## Variety Wholesalers: their sample, three generations of UI, and 22 business reports

- Built from the PRFDEMOLIB sample library Variety Wholesalers sent us
- One working session with CoderFlow, 3 November 2026
- Same RPG under the covers, from the 5250 screen to a branded web app

> Presenter note: the point of the deck is speed and fidelity. Their own code and their own data, made to run, modernised three ways and mined for insight, without rewriting the business logic.

---

## What Variety Wholesalers sent

- A partial restore of library PRFDEMOLIB: 28 source members, about 7,000 lines
- Three JDA merchandising programs: INV046 Inventory by Store (RPG III), INV502 Item Inquiry, TS0110 Ticket Item Inventory
- About 50 data files shipped as objects only, with no DDS source
- Real Roses data: 162 SKUs, 16 stores and DCs, about 10,000 rows
- Gaps: prompt and lookup programs missing, F-keys calling programs that weren't sent, one department deep (145 of 162 SKUs in Ladies Intimate Basics)

---

## The goal

- Get their programs running on our IBM i, unchanged in behaviour
- Make them demo-ready: data to key in, a guide for whoever drives the demo
- Show the modernisation path in three steps: green screen, plain web, branded web experience
- Turn their data into business insight: exception and insight reports in CoderFlow for Business

---

## The timeline: one session, ten steps

1. Imported the 28 source members into Git
2. Rebuilt the missing data files, built everything, loaded their data, added the programs to the menu
3. Wrote a new-hire guide for running the three programs
4. Added sample data to key in and seeded two Roses-style departments
5. Switched off calls to programs missing from the restore
6. Built the Variety Wholesalers web versions on agentic (EJS) display files
7. Fixed dropdown layering and width after a hands-on review
8. Added a plain "web version of the green screen" tier and re-ordered the menu
9. Diagnosed and fixed a screen that sat on "Loading..."
10. Profiled the data and wrote 22 CoderFlow for Business report prompts

> Presenter note: steps 7 and 9 came from me clicking through the demo and reporting what I saw. Both were root-caused and fixed in the same session.

---

## Step 1: Make their code run

- No source for the data files, so their DDS was rebuilt from the system's own file descriptions
- Proof: all 54 record formats match PRFDEMOLIB's format level identifiers exactly, so their data copies straight in
- 71 build targets: files, display files, message file, binding directory, module and 13 programs
- About 10,000 rows of their data loaded into the task library, with PRFDEMOLIB only ever read
- Menu options 4, 5 and 6 run INV046, INV502 and TS0110 as shipped
- One environment difference handled: the demo profile can't create a job description, so TS0110's batch submit sets the library directly (tagged in the source)

---

## Step 2: Make it demo-ready

- New-hire guide: 5250 basics, a walk-through of every screen and column, real screen captures, troubleshooting, a glossary
- Sample-data sheet: tested "try these first" recipes and a 186-SKU catalogue, generated from the live tables
- Seeded data modelled on Roses' own website: Seasonal Candy and Bedding departments, 24 SKUs, 5 vendors, pack-and-hold, and the inventory groups the sample left empty
- 34 calls to programs missing from the restore commented out (12 in INV046, 22 in INV502), so those keys simply redisplay instead of erroring
- Found and documented a bug in the original INV046 (store-type filter on group searches), with a workaround

---

## Step 3: One program, three experiences

| Menu | Experience | What it shows |
|---|---|---|
| 4, 5, 6 | Original 5250 green screens | Their programs, untouched |
| 7, 8, 9 | Plain web version of the green screen | Same fields, same keys, in a browser |
| 10, 11, 12 | Variety Wholesalers web experience | Branded, responsive, full of browser help |

- All three web apps run the same business logic as the green screen
- Walk one SKU through options 4, 7 and 10 to show the journey

---

## Same RPG under the covers

- Web programs are copies of the originals with a small, tagged set of changes
- Original lines replaced: 12 in INV502, 6 in TS0110, 4 in INV046
- The display file is swapped for an agentic (EJS) one through Profound UI Open Access
- Small routines translate browser buttons and keyboard keys into exactly what the program already tests
- INV046 was RPG III, so it was converted to ILE RPG first, then proven byte-identical to the original across 12 scripted screen steps
- The plain-web programs are generated from the branded ones; only the display-file name differs

---

## The plain web tier (options 7-9)

- A field-for-field web rendering of each 5250 screen
- Same labels, same order, same codes to type, same messages, same F-key legend
- No screen JavaScript at all: every key is a button
- What it adds is only what any web page gives you: click to type, clickable keys, fits a phone
- The "first step" story for a cautious customer: nothing for users to relearn

---

## The Variety Wholesalers experience (options 10-12)

- Brand taken from vwstores.com's own stylesheet: VW blue #0054a6, navy footer, their logo, their type style
- Light and dark themes, responsive from a 1440 px desktop to a 390 px phone
- Type-ahead pickers for SKU, store, region, district, groups and department, replacing the missing prompt programs
- Command palette (Ctrl+K), shortcut sheet, every green-screen F-key bound
- Sortable and filterable tables, CSV export, print styles
- KPI strips, heat-coded weeks of supply, sell-through bars, all computed from data the program already sends

---

## Showcase: the distribution builder

- TS0110 turns into a store distribution workbench
- Live totals of SKUs, packs, units and retail as you type
- Steppers, keyboard navigation and quick-filter chips
- Over-allocation warnings against what the DC can actually ship
- Smart fill: suggests packs for fast sellers the warehouse can supply
- A confirmation screen listing exactly what will ship, then the IBM i creates the distribution as it always did

---

## How we know it works

- Live IBM i sessions after every screen change, driving each program through the menu, with screen renderings in every summary
- 145 automated behaviour checks on the branded screens, including a negative control
- 14 real screens captured from live sessions, rendered at 5 widths plus dark mode, with the network blocked
- Profound's own EJS pre-flight checks on every display file
- Original green screens re-tested at every step: options 4-6 never changed

---

## Real issues, found and fixed in the session

- **Dropdowns hiding behind other panels**
  - The Genie skin's layering rule outranked our reset
  - Fixed, then the test harness was rebuilt inside the real skin context
  - The new checks failed 37 times before the fix and pass now
- **A screen stuck on "Loading SKU information..."**
  - Profound UI draws every format in a response into one container, whichever arrives last
  - Fixed so either order shows the right screen
  - Three other places with the same risk were fixed at the same time
- Both came from hands-on review, and both were fixed at the root, not patched

> Presenter note: this is the honest part of the story and it lands well. The agent didn't just change CSS until it looked right; it read the platform's own code to find the cause.

---

## From screens to insight: 22 reports

- Profiled PRFDEMOLIB first: what it holds, how full each field is, what it can answer
- Every report rule was run as SQL against their data before the prompt was written
- 22 plain-English prompts ready to paste into CoderFlow for Business, with no SQL and no markdown
- Each names the PRFDEMOLIB tables and columns, the rules, the layout, severity colours and a suggested action on every row
- A presenter guide with the expected numbers for every report

---

## The report catalogue

- **Replenishment:** stockout watch, out of stock today with lost sales, chronic out-of-stocks, store-to-store rebalancing, forecast accuracy
- **Distribution centers:** allocation shortfall, idle DC stock and pack-and-hold calendar
- **Merchandising:** slow movers, excess weeks of supply, never-sold stock, sell-through and markdowns, trending items, GMROI and turns, assortment Pareto
- **Pricing and sourcing:** margin and price integrity, alternate vendor savings, vendor scorecard
- **Loss and quality:** shrink and negative on-hand, customer returns
- **Operations and data:** store scorecard, data integrity worklist
- **Executive:** weekly merchandising briefing

---

## What their data says

- Chain in-stock is 77.9%; 293 store and SKU combinations are below their order point
- 50 shelves are empty on items customers are still buying
- Store 205 Laurel sells the most per square foot and has the emptiest shelves (62% in stock)
- One vendor, Panties Plus, is in stock 48.7% of the time across 41 SKUs
- Half/Full Slips sit at 4.7% sell-through, but nine of them are brand new and just starting to sell
- Girls' packaged underwear returns $16 of margin for every inventory dollar
- 64 tail SKUs bring in only 5% of sales

> Presenter note: all from a single-department sample. The point is what the same reports would say across their whole chain.

---

## Suggested demo flow

1. Weekly briefing: in-stock is 78%, here's what it costs
2. Store scorecard: the best store has the emptiest shelves
3. Out of stock today, then stockout watch: what to do this morning
4. Vendor scorecard: half the problem is one vendor
5. Store-to-store rebalancing: fix it without buying more
6. Sell-through and GMROI: where the inventory dollars aren't earning
7. Assortment Pareto: the long tail
8. Alternate vendor savings: finish on money

---

## By the numbers

- 28 source members received, about 7,000 lines
- 54 record formats rebuilt, level-ID identical; 71 build targets
- 3 programs, 3 experiences each, 9 menu options
- Only 4, 6 and 12 original RPG lines replaced per web program
- About 2,700 lines of web templates, styles and screen script
- 145 behaviour checks, 14 live fixtures, 5 screen widths
- 22 verified report prompts
- About 23,000 words of guides, data sheets and presenter notes

---

## What this means for Variety Wholesalers

- Their existing RPG keeps running; the modernisation is in the display layer
- They can move at their own pace: green screen, plain web, then the full experience
- The same data already supports exception reporting that points at stockouts, vendor performance and dead inventory
- Every step is in Git, documented and repeatable on their own system

---

## Next steps

- Build the 22 reports in CoderFlow for Business and walk through the demo flow
- Approve the task so the web assets deploy to the Profound UI server
- Optional: restore the missing prompt programs, or keep the web pickers that replace them
- Optional: fix the original INV046 group-search bug (a two-line change)
- Pick a next program from their library to run through the same path

---

## Thank you

- Their code, their data, three ways to see it, and 22 ways to act on it
- Questions and live demo
