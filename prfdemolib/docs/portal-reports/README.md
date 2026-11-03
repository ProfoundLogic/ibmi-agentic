# Variety Wholesalers: CoderFlow for Business report prompts

`vw-portal-report-prompts.txt` holds 22 plain-text prompts. Paste each one into CoderFlow for Business to build an insight or exception report over the JDA merchandising sample in library **PRFDEMOLIB**.

The prompts follow the house style for portal prompts:
- no markdown and no SQL
- written in a business user's voice
- the library, tables and key columns named
- the rules given in plain English
- a severity colour and a plain-English action on every row

This README is for the presenter. It doesn't get pasted.

## What the data is

PRFDEMOLIB is a real sample from Variety Wholesalers (Roses), a JDA MMS snapshot. The latest sale in it is **28 Oct 2026**. That's why the prompts that age inventory use "the latest date last sold in INVBAL" as the as-of date rather than today, so they give the same answer next week as now.

| Table | Rows | What it is |
|---|---|---|
| INVMST | 162 | Item master: cost, vendor, buyer, class, status, replenishment code |
| INVBAL | 1,788 | SKU x location balance: on hand, 8 weeks of sales, out-of-stock flags, YTD sales/cost/returns/adjustments, GMROI, turns |
| INVCBL | 149 | Chain-level balance per SKU |
| RPLPRF | 1,230 | Replenishment profile per SKU x store: order-at, order-up-to, forecast, lead time |
| TRFDTL | 2,852 | Open DC-to-store transfer requests (nothing shipped yet) |
| INVVEN | 878 | Alternate vendors per SKU x DC, with cost |
| ITMRTL | 162 | Regular, original and zone retail |
| INVFSD | 148 | First/last sold and markdown/markup history |
| TBLSTR | 16 | 10 Roses stores + 6 DCs (9001 Henderson, 9002 Newnan, 9151/9152 Big Lots DCs) |
| APSUPP | 27 | Vendor master |
| INVDPT | 391 | Department / sub-department / class names |
| PHPF | 2 | Pack-and-hold (5 future releases) |
| TBLBYR, TBLREG, TBLDST, TBLSEA | small | Buyers, regions, districts, seasons |

PO files (POMHDR, POMDTL, POMRCD) are empty, so no report relies on purchase orders.

**Things to know before you demo**
- **The assortment is concentrated.** 145 of the 162 SKUs are in department 12, Ladies Intimate Basics, all under buyer 23. That's why reports group by sub-department and class rather than department.
- **371 of the 380 stores that receive transfers are not in TBLSTR** (the sample only holds 10 stores). Report 04 shows store numbers for those, and report 19 counts them.
- **Report 14 needs a caveat.** Most alternate vendors in INVVEN are not in APSUPP, so the report shows numbers rather than names. The savings figures are large because they use chain moving annual units across the whole Roses chain. Present it as "here's where to ask for quotes", not as booked savings.
- **The seeded demo data is not here.** The extra items added for the green-screen demos (SKUs 8801xx/8802xx, inventory groups) live in the task library, not PRFDEMOLIB, so these reports don't see them.

## Expected results (checked against PRFDEMOLIB on 3 Nov 2026)

Use these to sanity-check a generated report. If a number is way off, the generator misread a rule.

| # | Report | What you should see |
|---|---|---|
| 01 | Stockout Watch | 293 store/SKU rows below order point, 84 SKUs; 44 already at zero, 10 more with under a week of cover |
| 02 | Out of Stock Today | 50 empty shelves on items still selling, 32 SKUs, about 69 units / $400 retail lost per week |
| 03 | Chronic Out-of-Stocks | 144 store/SKU rows out of stock 4+ of 8 weeks, 65 SKUs; 98 rows out 6+ weeks |
| 04 | DC Allocation Shortfall | 175 SKU/DC pairs with open requests across 4 DCs; 56 short, 21 with no DC stock, 3,296 units short. Top: STEAM PAN at 9151 short 3,600 for 30 stores; SOFT PEPPERMINT PUFFS at 9002, 24 on hand vs 600 requested for 19 stores |
| 05 | Store-to-Store Rebalancing | 269 of the 293 below-order-point rows have a donor store, 78 SKUs |
| 06 | Slow-Moving Inventory | 263 rows with stock and no sales in 8 weeks, 105 SKUs, 2,260 units, about $5,760 at cost; 102 of them never sold |
| 07 | Excess Weeks of Supply | 230 rows over 26 weeks of supply, 101 SKUs, about 15,600 excess units / $14,350 at cost |
| 08 | Never-Sold Inventory | 102 rows, 29 SKUs, 1,192 units, about $1,840 at cost |
| 09 | Class Sell-Through | HALF/FULL SLIPS: 28 SKUs, 4.7% sell-through, 587 units on hand. Most classes 70-88%. 40 SKUs have markdown history (39 still in stock) |
| 10 | Trending Items | 11 risers, 2 fallers. Top riser ASST BASIC BEANIE (289999): 1,042 to 17,003 units (+1,532%). DOVE SHEA BUTTER +55%. Nine new 2940xx full and half slips (class HALF/FULL SLIPS) going from zero to their first sales. Fallers: 166402 swim tops, 827408 control brief |
| 11 | Forecast Accuracy | 192 profiles with 8+ units sold: 26 over-forecast, 54 under-forecast |
| 12 | GMROI and Turns | GMROI from 0 (HALF/FULL SLIPS) to 16.2 (GIRLS PKG UNDERWEAR); 5.99 BRAS (64 SKUs) 7.1 |
| 13 | Margin and Price | 6 SKUs under 40% margin (SOFT PEPPERMINT PUFFS 25.5%, LAYS 25.7%, LDM COOKIES 31.6%, two Dove bars, a 4PK bikini); 7 SKUs with a lower zone-3 price; SKU 12 (department price placeholder) has no price or cost; 1 SKU with DC average cost over 5% above current cost |
| 14 | Alternate Vendor Savings | 7 SKUs with an alternate at least 5% cheaper: STEAM PAN, 24PK WATER, DOVE PINK, WASH CLOTH, DOVE SHEA, PEPPERMINT PUFFS, a sports bra |
| 15 | Vendor Scorecard | DOLCE VITA (Lady Ester) 65 SKUs, $61.7k, 94.7% in stock; PANTIES PLUS 41 SKUs, $51.1k, **48.7% in stock** |
| 16 | Shrink | Physical-count losses: 205 Laurel $2,945, 262 Pearl $1,525; 13 negative on-hand rows (12 at stores, 1 at DC 9152), worst 205 Laurel 5PK BRALETTE at -14 |
| 17 | Returns | 31 store/SKU rows over 5% (at least 20 sold), 23 SKUs |
| 18 | Store Scorecard | 205 Laurel: best sales per sq ft ($1.45) but worst in-stock (62.1%). 499 Jacksonville: last 4 weeks 1,806 units vs 606 before (+198%). 1838/458/535 run about 95% in stock |
| 19 | Data Integrity | 13 negative on hand; 70 store rows on system replenishment with no profile (e.g. 13x13 WASH CLOTH 100001 has no profile at any of the 10 stores); 1 SKU with no price/cost; 8 active SKUs past their dead date; vendor-not-in-master and inactive-with-stock both come back clean (0) |
| 20 | DC Position | Idle DC stock: 9001 5 SKUs ($2,372), 9002 5 ($1,932), 9151 1 ($756). Pack-and-hold: 2 SKUs, 5 releases Jan-Apr 2027 at DC 9001 (90,048 units, 86,040 of them ASST TOPS 262657) |
| 21 | Assortment Pareto | Band A 39 SKUs = 80% of sales; band C tail 64 SKUs = 5% of sales ($19.4k) holding $6,150 of inventory |
| 22 | Briefing | YTD store sales $393k, 45.9% margin; last week 8,465 units vs 6,734 average of the prior 7; chain in-stock 77.9% (red); store inventory $61.6k at cost |

## A demo storyline that works

1. **22 Weekly Merchandising Briefing.** Open with "in-stock is 78%, and here's what it costs us".
2. **18 Store Scorecard.** Laurel sells the most per square foot and has the emptiest shelves.
3. **02 Out of Stock Today, then 01 Stockout Watch.** What to do about it this morning.
4. **15 Vendor Scorecard.** Half of the in-stock problem is one vendor (Panties Plus at 49%).
5. **05 Store-to-Store Rebalancing.** Fix it without buying more.
6. **09 Class Sell-Through and 12 GMROI.** The other side of the coin: Half/Full Slips sit at 4.7% sell-through with zero GMROI while girls' underwear earns 16 to 1. Be ready for the follow-up: report 10 shows nine of those slips are brand new and just starting to sell, so the right call is "watch the new ones, clear the old ones", not a blanket markdown. It's a good moment to show two reports agreeing on the real story.
7. **21 Assortment Pareto.** The long tail, and the "different direction" conversation.
8. **14 Alternate Vendor Savings.** Finish on money.

## If a report comes out wrong

- **Distribution centers counted as stores:** the generator missed "locations 9001 and up are distribution centers". Say it again in the follow-up.
- **Dates look like 261028:** ask for YYMMDD with the century code (1 = 2000s) converted to real dates.
- **Retail is off by a multiple:** unit retail is REGPRC divided by REGMLT (REGMLT is 1 throughout this sample).
- **Weeks in the wrong order:** IBWK01 / CBWK01 is the most recent completed week.
